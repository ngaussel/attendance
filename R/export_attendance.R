# Lecture des présences depuis Supabase (remplace la lecture du log
# Google Sheets qui était faite dans tests.R avant la migration new_arch).
#
# Usage (console R, pas sourcé par l'app) :
#   source("R/export_attendance.R")
#   att <- get_attendance()
#   dplyr::glimpse(att)
#
#   # Export ponctuel si besoin de partager (Excel/CSV), la base reste la
#   # source de vérité :
#   readr::write_csv(att, "attendance_export.csv")
#   writexl::write_xlsx(att, "attendance_export.xlsx")  # install.packages("writexl") si besoin

library(httr2)
library(tibble)
library(purrr)
library(dplyr)
library(readr)

`%||%` <- function(a, b) if (is.null(a)) b else a

get_attendance <- function() {
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_key <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")
  if (!nzchar(supabase_url) || !nzchar(supabase_key)) {
    stop("SUPABASE_URL / SUPABASE_SERVICE_ROLE_KEY not set. Add them to .Renviron.")
  }

  resp <- request(paste0(supabase_url, "/rest/v1/attendance_records")) |>
    req_url_query(
      select = paste0(
        "student_email,student_id,master,first_name,last_name,",
        "submitted_at,status,session:sessions(course,venue,opens_at)"
      ),
      order = "submitted_at.desc"
    ) |>
    req_headers(
      apikey        = supabase_key,
      Authorization = paste("Bearer", supabase_key)
    ) |>
    req_perform()

  rows <- resp_body_json(resp, simplifyVector = FALSE)

  purrr::map_dfr(rows, function(r) {
    tibble(
      email             = r$student_email,
      student_id        = r$student_id %||% NA_character_,
      master            = r$master %||% NA_character_,
      first_name        = r$first_name %||% NA_character_,
      last_name         = r$last_name %||% NA_character_,
      course            = r$session$course %||% NA_character_,
      venue             = r$session$venue %||% NA_character_,
      session_opened_at = r$session$opens_at %||% NA_character_,
      submitted_at      = r$submitted_at,
      status            = r$status
    )
  }) |>
    mutate(
      session_opened_at = readr::parse_datetime(session_opened_at),
      submitted_at       = readr::parse_datetime(submitted_at)
    )
}
