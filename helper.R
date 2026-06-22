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


# Vérif email domaine
is_valid_domain_email <- function(email, domain = DOMAIN) {
  email <- tolower(trimws(email %||% ""))
  grepl(glue("@{domain}$"), email, ignore.case = TRUE)
}

haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000
  phi1 <- lat1 * pi/180; phi2 <- lat2 * pi/180
  dphi <- (lat2 - lat1) * pi/180; dlam <- (lon2 - lon1) * pi/180
  a <- sin(dphi/2)^2 + cos(phi1)*cos(phi2)*sin(dlam/2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
