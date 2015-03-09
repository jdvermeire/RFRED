
# query functions ---------------------------------------------------------

rfred.Query <- function(api_call, api_container, ...) {
  # Provides low-level query function to FRED API
  #
  # Args:
  #   api_call: the api function to call, e.g. "/series/observations"
  #   api_container: the returned container name for the data
  #   ...: a parameter list
  #
  # Returns:
  #   the raw object from the FRED API
  
  # TODO(jdvermeire): error handle missing api key
  
  params <- match.call(expand.dots = FALSE)$...  # get params from fx call
  params$api_key <- get("api.key", rfred_globals)  # add api key
  params$file_type <- "json"  # set file type to json
  qry <- paste(paste0(k.api.url, api_call), 
               paste(names(params), params, sep = "=", collapse = "&"), 
               sep = "?")
  qry.con <- url(qry)  # create connection url
  on.exit(close(qry.con))  # make sure connection closes properly
  ans <- fromJSON(file = qry.con)[[api_container]]
  if (isOpen(qry.con)) close(qry.con)  # close connection
  return(ans)  
}


# Category Queires --------------------------------------------------------


q.category <- function(category_id = 0) {
  rfred.category(rfred.Query("/category", "categories", category_id)[[1]])
}

q.category.children <- function() {
  
}

q.category.related <- function() {
  
}

q.category.series <- function() {
  
}

q.category.tags <- function() {
  
}

q.category.related_tags <- function() {
  
}


# Release Queries ---------------------------------------------------------

q.releases <- function() {
  
}

q.releases.dates <- function() {
  
}

q.release <- function() {
  
}

q.release.dates <- function() {
  
}

q.release.series <- function() {
  
}

q.release.sources <- function() {
  
}

q.release.tags <- function() {
  
}

q.release.related_tags <- function() {
  
}