library(bslib)
library(glue)
library(dplyr)
library(stringr)
library(qrcode)
library(png)
library(base64enc)
library(googlesheets4)
library(gargle)


gs4_deauth()
if (nzchar(Sys.getenv("GS_SERVICE_ACCOUNT_JSON"))) {
  gs4_auth(path = Sys.getenv("GS_SERVICE_ACCOUNT_JSON"))
} else {
  # local dev: fenêtre OAuth au premier run
  gs4_auth(cache = TRUE)
}

rand_token <- function(n = TOKEN_LENGTH) {
  x <- strsplit(ALPHABET, "")[[1]]
  paste0(sample(x, n, replace = TRUE), collapse = "")
}

now_utc <- function() as.POSIXct(Sys.time(), tz = "UTC")


# Append lignes (data.frame)
append_rows <- function(sheet, df) {
  googlesheets4::sheet_append(SHEET_ID, data = df, sheet = sheet)
}

# Assurance en-têtes (idempotent)
ensure_headers <- function() {
  ss <- gs4_get(SHEET_ID)
  tabs <- ss$sheets$name
  
  if (!(SHEET_NAME_TOKENS %in% tabs)) {
    googlesheets4::sheet_add(SHEET_ID, sheet = SHEET_NAME_TOKENS)
    append_rows(SHEET_NAME_TOKENS, tibble(
      token      = "TOKEN",
      session_id = "SESSION_ID",
      issued_at  = "ISSUED_AT",
      expires_at = "EXPIRES_AT",
      used       = "USED"
    ))
    googlesheets4::range_delete(SHEET_ID, sheet = SHEET_NAME_TOKENS, range = "A2:E2", shift = "up")
  }
  
  if (!(SHEET_NAME_LOG %in% tabs)) {
    googlesheets4::sheet_add(SHEET_ID, sheet = SHEET_NAME_LOG)
    append_rows(SHEET_NAME_LOG, tibble(
      ts = "TIME_STAMP", email = "EMAIL", session_id = "SESSION_ID",
      student_id = "STUDENT_ID", token = "TOKEN",
      issued_at = "ISSUED_AT", expires_at = "EXPIRES_AT", ok = "OK"
    ))
    googlesheets4::range_delete(SHEET_ID, sheet = SHEET_NAME_LOG, range = "A2:H2", shift = "up")
  }
}

# Trouver un token + sa ligne (renvoie list(row_index, data))
find_token <- function(token) {
  df <- suppressMessages(googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_NAME_TOKENS))
  if (nrow(df) == 0) return(NULL)
  # normaliser colonnes
  names(df) <- tolower(names(df))
  i <- which(df$token == token)
  if (length(i) == 0) return(NULL)
  list(row_index = i + 1L, data = df[i, , drop = FALSE])  # +1 car header = ligne 1
}

# Marquer used=TRUE sur une ligne
mark_token_used <- function(row_index) {
  # colonne 'used' = E ; inversement, positionne dynamiquement…
  # On relit l'entête pour trouver l'index
  header <- read_sheet(SHEET_NAME_TOKENS, range = "1:1")
  header <- tolower(names(header))
  col_used <- which(header == "used")
  a1 <- glue("{LETTERS[col_used]}{row_index}")
  googlesheets4::range_write(SHEET_ID, data = data.frame(TRUE), sheet = SHEET_NAME_TOKENS,
                             range = a1, col_names = FALSE)
}

# Vérif email domaine
is_valid_domain_email <- function(email, domain = DOMAIN) {
  email <- tolower(trimws(email %||% ""))
  grepl(glue("@{domain}$"), email, ignore.case = TRUE)
}

# Vérif roster (optionnel)
is_in_roster <- function(email) {
  if (!REQUIRE_ROSTER) return(TRUE)
  tabs <- gs4_get(SHEET_ID)$sheets$name
  if (!(ROSTER_SHEET_NAME %in% tabs)) return(TRUE)
  r <- suppressMessages(read_sheet(ROSTER_SHEET_NAME))
  if (!("email" %in% tolower(names(r)))) return(TRUE)
  any(tolower(trimws(r$email)) == tolower(trimws(email)))
}

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
