library(googlesheets4)


gs4_auth(path = "regal-bonito-471216-f5-a861b85bdf3a.json",
         scopes = "https://www.googleapis.com/auth/spreadsheets")

text="bob"
qrcode::qr_code(text,) |> plot()
generate_qr_image(text = "bob")

# --- Lecture du log de présences -----------------------------------------------
library(dplyr)
library(readr)

SHEET_ID <- Sys.getenv("ATTENDANCE_SHEET_ID")
# ou directement : SHEET_ID <- "..."

log_raw <- googlesheets4::read_sheet(SHEET_ID, sheet = "log")

log <- log_raw |>
  mutate(
    ts     = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S", tz = "Europe/Paris"),
    date   = as.Date(date),
    across(c(email, lecture, student_id, student_lnid, student_fnid, master), as.character),
    geo_accuracy = as.integer(geo_accuracy),
    ok     = as.logical(ok)
  )

glimpse(log)
