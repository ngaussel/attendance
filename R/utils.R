rand_token <- function(n = TOKEN_LENGTH) {
  x <- strsplit(ALPHABET, "")[[1]]
  paste0(sample(x, n, replace = TRUE), collapse = "")
}

now_utc <- function() as.POSIXct(Sys.time(), tz = "UTC")
