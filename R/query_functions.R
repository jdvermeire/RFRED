
# query functions ---------------------------------------------------------

rfred.Query <- function(api_call, ...) {
  # Provides low-level query function to FRED API
  #
  # Args:
  #   api_call: the api function to call, e.g. "/series/observations"
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
  ans <- fromJSON(file = qry.con)  # parse json
  if (isOpen(qry.con)) close(qry.con)  # close connection
  return(ans)  
}
