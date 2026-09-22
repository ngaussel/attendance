library(httr2)

SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
SUPABASE_SERVICE_ROLE_KEY <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")

if (!nzchar(SUPABASE_URL)) stop("SUPABASE_URL is not set. Add it to .Renviron.")
if (!nzchar(SUPABASE_SERVICE_ROLE_KEY)) stop("SUPABASE_SERVICE_ROLE_KEY is not set. Add it to .Renviron.")

supabase_request <- function(path) {
  request(paste0(SUPABASE_URL, "/rest/v1/", path)) |>
    req_headers(
      apikey        = SUPABASE_SERVICE_ROLE_KEY,
      Authorization = paste("Bearer", SUPABASE_SERVICE_ROLE_KEY)
    )
}

iso_utc <- function(t) format(t, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

supabase_create_session <- function(course, venue_lat, venue_lon, geo_radius_m,
                                     token_ttl_seconds = TOKEN_TTL_SECONDS,
                                     fill_seconds = FILL_SECONDS) {
  resp <- supabase_request("sessions") |>
    req_headers(Prefer = "return=representation") |>
    req_body_json(list(
      course           = course,
      venue_lat        = venue_lat,
      venue_lon        = venue_lon,
      geo_radius_m     = geo_radius_m,
      token_ttl_seconds = token_ttl_seconds,
      fill_seconds     = fill_seconds
    )) |>
    req_perform()

  resp_body_json(resp)[[1]]$id
}

supabase_rotate_token <- function(session_id, token) {
  # Insère un nouveau token dans l'historique (session_tokens) au lieu
  # d'écraser une seule colonne : un étudiant ayant scanné un token plus
  # ancien doit pouvoir encore l'utiliser tant qu'il est dans sa propre
  # fenêtre token_ttl_seconds + fill_seconds, même si le prof a déjà
  # tourné vers un token plus récent entre-temps.
  supabase_request("session_tokens") |>
    req_headers(Prefer = "return=minimal") |>
    req_body_json(list(
      session_id = session_id,
      token      = token,
      issued_at  = iso_utc(now_utc())
    )) |>
    req_perform()

  invisible(TRUE)
}

supabase_close_session <- function(session_id) {
  supabase_request(paste0("sessions?id=eq.", session_id)) |>
    req_method("PATCH") |>
    req_headers(Prefer = "return=minimal") |>
    req_body_json(list(status = "closed")) |>
    req_perform()

  invisible(TRUE)
}
