
# Globals Store -----------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  assign("rfred_globals", new.env(), envir = parent.env(environment()))
}

# Constants ---------------------------------------------------------------

k.api.url <- "http://api.stlouisfed.org/fred"  # the base api url

# API Key -----------------------------------------------------------------

SetApiKey <- function(key) {
  # Sets the API key to query FRED
  #
  # Args:
  #   key: the API key
  #
  # Returns:
  #   NULL
  assign("api.key", key, rfred_globals)
}

GetApiKey <- function() {
  # Gets the API key
  #
  # Returns:
  #   the previously set API key
  get("api.key", rfred_globals)
}
