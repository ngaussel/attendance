-- Schéma Supabase pour l'émargement (new_arch)
-- Version simplifiée, mono-enseignant (pas de multi-tenant pour l'instant,
-- cf. saas_architecture.md §7 pour la trajectoire multi-tenant future).
--
-- Principe de sécurité : RLS activé sur les deux tables, SANS policy pour
-- l'anon/publishable key. Les étudiants ne touchent jamais les tables en
-- direct : ils passent uniquement par la fonction validate_attendance()
-- (SECURITY DEFINER, donc elle bypass RLS). Le prof (Shiny, via service_role
-- côté serveur) gère sessions directement et bypass RLS aussi.

create extension if not exists pgcrypto; -- pour gen_random_uuid()

-- ---------------------------------------------------------------------
-- 1. Sessions d'émargement (une par séance/créneau)
-- ---------------------------------------------------------------------
create table sessions (
  id uuid primary key default gen_random_uuid(),
  course text not null,
  venue text,
  venue_lat double precision,
  venue_lon double precision,
  geo_radius_m integer not null default 300,
  current_token text,               -- token rotatif, mis à jour par le prof toutes les TOKEN_TTL_SECONDS
  token_issued_at timestamptz,      -- horodatage du dernier token émis
  token_ttl_seconds integer not null default 25,
  fill_seconds integer not null default 50,   -- fenêtre de tolérance après expiration du token affiché
  opens_at timestamptz not null default now(),
  closes_at timestamptz,            -- null = pas de fin fixée, on ferme via status
  status text not null default 'open' check (status in ('open', 'closed')),
  created_at timestamptz not null default now()
);

create index sessions_status_idx on sessions (status);

-- ---------------------------------------------------------------------
-- 2. Présences validées
-- ---------------------------------------------------------------------
create table attendance_records (
  id uuid primary key default gen_random_uuid(),
  session_id uuid not null references sessions(id) on delete cascade,
  student_email text not null,
  student_id text,
  master text,
  first_name text,
  last_name text,
  device_fingerprint text,
  lat double precision,
  lon double precision,
  submitted_at timestamptz not null default now(),
  status text not null default 'present',
  unique (session_id, student_email)
);

-- Un seul appareil par session (parité avec le fingerprinting.txt actuel),
-- mais seulement quand un fingerprint est fourni.
create unique index attendance_one_device_per_session
  on attendance_records (session_id, device_fingerprint)
  where device_fingerprint is not null;

-- ---------------------------------------------------------------------
-- 3. RLS : activé, aucune policy => tout accès direct via anon/publishable
--    key est refusé. Seule la fonction validate_attendance() (SECURITY
--    DEFINER) peut écrire depuis le client étudiant.
-- ---------------------------------------------------------------------
alter table sessions enable row level security;
alter table attendance_records enable row level security;

-- ---------------------------------------------------------------------
-- 4. Fonction de validation atomique, appelée depuis la page étudiante
--    via supabase.rpc("validate_attendance", {...}).
-- ---------------------------------------------------------------------
create or replace function validate_attendance(
  p_session_id uuid,
  p_token text,
  p_student_email text,
  p_student_id text default null,
  p_master text default null,
  p_first_name text default null,
  p_last_name text default null,
  p_fingerprint text default null,
  p_lat double precision default null,
  p_lon double precision default null
)
returns jsonb
language plpgsql
security definer
set search_path = public
as $$
declare
  v_session sessions%rowtype;
  v_token_age_seconds double precision;
  v_distance_m double precision;
begin
  select * into v_session from sessions where id = p_session_id;

  if not found then
    return jsonb_build_object('ok', false, 'reason', 'SESSION_NOT_FOUND');
  end if;

  if v_session.status <> 'open' then
    return jsonb_build_object('ok', false, 'reason', 'SESSION_CLOSED');
  end if;

  if v_session.closes_at is not null and now() > v_session.closes_at then
    return jsonb_build_object('ok', false, 'reason', 'SESSION_CLOSED');
  end if;

  if v_session.current_token is null or p_token <> v_session.current_token then
    return jsonb_build_object('ok', false, 'reason', 'INVALID_TOKEN');
  end if;

  v_token_age_seconds := extract(epoch from (now() - v_session.token_issued_at));
  if v_token_age_seconds > (v_session.token_ttl_seconds + v_session.fill_seconds) then
    return jsonb_build_object('ok', false, 'reason', 'TOKEN_EXPIRED');
  end if;

  -- Vérification géoloc côté serveur (défense en profondeur : le client
  -- fait déjà un contrôle, mais il ne faut pas lui faire confiance seul).
  if v_session.venue_lat is not null then
    if p_lat is null or p_lon is null then
      return jsonb_build_object('ok', false, 'reason', 'LOCATION_REQUIRED');
    end if;

    v_distance_m := (
      6371000 * acos(
        least(1.0, greatest(-1.0,
          cos(radians(v_session.venue_lat)) * cos(radians(p_lat)) *
            cos(radians(p_lon) - radians(v_session.venue_lon)) +
          sin(radians(v_session.venue_lat)) * sin(radians(p_lat))
        ))
      )
    );
    if v_distance_m > v_session.geo_radius_m then
      return jsonb_build_object(
        'ok', false, 'reason', 'OUT_OF_RANGE', 'distance_m', round(v_distance_m)
      );
    end if;
  end if;

  if p_fingerprint is not null and exists (
    select 1 from attendance_records
    where session_id = p_session_id
      and device_fingerprint = p_fingerprint
      and student_email <> lower(p_student_email)
  ) then
    return jsonb_build_object('ok', false, 'reason', 'DEVICE_ALREADY_USED');
  end if;

  insert into attendance_records (
    session_id, student_email, student_id, master,
    first_name, last_name, device_fingerprint, lat, lon
  )
  values (
    p_session_id, lower(p_student_email), p_student_id, p_master,
    p_first_name, p_last_name, p_fingerprint, p_lat, p_lon
  )
  on conflict (session_id, student_email)
  do update set
    submitted_at = now(),
    student_id = excluded.student_id,
    master = excluded.master,
    first_name = excluded.first_name,
    last_name = excluded.last_name,
    device_fingerprint = excluded.device_fingerprint,
    lat = excluded.lat,
    lon = excluded.lon;

  return jsonb_build_object('ok', true);
end;
$$;

-- Autoriser l'exécution de la fonction par le rôle anon (clé publishable).
grant execute on function validate_attendance(
  uuid, text, text, text, text, text, text, text, double precision, double precision
) to anon;
