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
SHEET_NAME_LOG     <- "log"
DOMAIN             <- "etu.univ-paris1.fr"
COURSE_MENU <- c("Financial Products", "Interest Rates", "Advanced Topics")

TOKEN_TTL_SECONDS  <- 25L
FILL_SECONDS <- 50L
TOKEN_LENGTH <- 8L
ALPHABET <- "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

GEO_RADIUS_METERS <- 300L

VENUES <- c(
  "Sorbonne - MSE 117"  = "48.835833,2.358323",
  "Sorbonne - MSE 114"  = "48.835684, 2.358230"
)
