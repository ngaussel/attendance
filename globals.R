# Shared in-memory state (survives across sessions within the same R process)
token_store  <- list()   # named list: token -> list(session_id, expires_at)
submitted_presences <- character(0)  # "session_id|student_id" already submitted

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
FILL_SECONDS <- 1000L
TOKEN_LENGTH <- 8L
ALPHABET <- "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"
