# Shared in-memory state (survives across sessions within the same R process)
token_store  <- list()   # named list: token -> list(session_id, expires_at)
submitted_presences <- character(0)  # "session_id|student_id" already submitted
device_fingerprints <- list()        # named list: session_id -> character vector of hashes

# --- CONFIG ENV ----------------------------------------------------------------
SHEET_ID <- Sys.getenv("ATTENDANCE_SHEET_ID")
if (!nzchar(SHEET_ID)) stop("ATTENDANCE_SHEET_ID is not set. Add it to .Renviron.")
if (!nzchar(Sys.getenv("GS_SERVICE_ACCOUNT_JSON"))) {
  Sys.setenv(GS_SERVICE_ACCOUNT_JSON = "id_api_google.json")
}
SHEET_NAME_TOKENS  <- "tokens"
SHEET_NAME_LOG     <- "log"
SHEET_NAME_ROSTER     <- "roster"
SHEET_NAME_MMMEF     <- "MMMEF"
ROSTER_SHEET_NAME  <- "roster"
DOMAIN             <- "etu.univ-paris1.fr"
REQUIRE_ROSTER     <- FALSE
COURSE_MENU <- c("Financial Products", "Interest Rates", "Advanced Topics")

TOKEN_TTL_SECONDS  <- 25L
FILL_SECONDS <- 50L
TOKEN_LENGTH <- 8L
ALPHABET <- "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

GEO_RADIUS_METERS <- 300L

VENUES <- c(
  "Sorbonne - Amphi Richelieu"  = "48.84893,2.34343",
  "Sorbonne - Salle Cavaillès"  = "48.84870,2.34320",
  "Centre PMF - Amphi B"        = "48.82811,2.35476",
  "Centre PMF - Salle 5.13"     = "48.82820,2.35490",
  "MSE - Salle des Thèses"      = "48.83267,2.35744"
)
