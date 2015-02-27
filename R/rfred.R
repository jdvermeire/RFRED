rfred <- function(key) {
  UseMethod("rfred", key)
}
rfred.character <- function(key) {
  # Initializes and creates an object of class "rfred"
  #
  # Args:
  #   key: A valid API key obtained from 
  #        https://research.stlouisfed.org/useraccount/apikeys
  #
  # Returns:
  #   An object of class "rfred" to be used in other rfred functions
  
  # create a new environment for the rfred object
  # this will house all of the nitty-gritty stuff
  ans <- new.env()
  
  # initialize all the environment variables and functions
  local({
    # vars
    ncores <- max(1, detectCores() - 1)  # get cores for parallelization
    cluster <- makeCluster(ncores)  # setup cluster for parallelization
    api.key <- key  # pass key into env
    api.url <- "http://api.stlouisfed.org/fred"  # the base api url
    # key-value pair for all queries
    api.params.k <- rbind(c(key = "api_key", value = api.key),
                          c("file_type", "json"))    
    
    #general functions
    api.query <- function(op, params) {
      # Executes a query against the FRED api
      #
      # Args:
      #   op: a character operation to execute, e.g. "/series/observations"
      #   params: a key-value pair list of parameters
      #
      # Returns:
      #   A list converted from a json object
      
      params <- rbind(api.params.k, params)  # add constant parameters
      qry <- paste(paste0(api.url, op), paste(parApply(cluster, params, 1, 
                                                       paste, collapse = "="), 
                                              collapse = "&"), sep = "?")
      qry.con <- url(qry)  # create connection url
      on.exit(close(qry.con))  # make sure connection closes properly
      ans <- fromJSON(file = qry.con)  # parse json
      return(ans)
    }
    
    # wrapper functions
    api.category <- function(category_id = 0) {
      # Retrieves category from FRED
      #
      # Args:
      #   category_id: the id for the category
      #
      # Returns:
      #   The category
      
      params <- c("category_id", category_id)
      # query api
      ans <- api.query("/category", params)$categories
      # set class
      class(ans) <- "rfred.category"
      return(ans)      
    }
    api.category.children <- function(category.id, realtime.start, 
                                      realtime.end) {}
    api.category.related <- function(category.id, realtime.start, 
                                     realtime.end) {}
    api.category.series <- function(category.id, realtime.start, realtime.end,
                                    limit, offset, order.by, sort.order,
                                    filter.variable, filter.value, tag.names) {}
    api.category.tags <- function(category.id, realtime.start, realtime.end,
                                  tag.names, tag.group.id, search.text, limit,
                                  offset, order.by, sort.order) {}
    api.category.related.tags <- function(category.id, realtime.start,
                                          realtime.end, tag.names, tag.group.id,
                                          search.text, limit, offset, order.by,
                                          sort.order) {}
    
    api.releases <- function() {}
    api.releases.dates <- function() {}
    api.release <- function() {}
    api.release.dates <- function() {}
    api.release.series <- function() {}
    api.release.sources <- function() {}
    api.release.tags <- function() {}
    api.release.related.tags <- function() {}
    
    api.series <- function() {}
    api.series.categories <- function() {}
    api.series.observations <- function(series.id, 
                                        realtime.start = Sys.Date(), 
                                        realtime.end = Sys.Date(),
                                        limit = 100000L, 
                                        offset = 0L, 
                                        sort.order = "asc", 
                                        observation.start = "1776-07-04", 
                                        observation.end = "9999-12-31",
                                        units = "lin", 
                                        frequency = NULL, 
                                        aggregation.method = "avg",
                                        output.type = 1L, 
                                        vintage.dates = NULL) {
      # Gets observation data for given series
      #
      # Args:
      #   series.id: id of the series
      #   realtime.start: beginning of the real-time period
      #   realtime.end: end of the real-time period
      #   limit: max number of observations returned (max 100000)
      #   offset: pagenation offset
      #   sort.order: direction of sort
      #   observation.start: beginning of observation period
      #   observation.end: end of observation period
      #   units: specifies data value transformation
      #   frequency: rolls-up high-frequency to a lower-frequency
      #   aggregation.method: specifices how to roll up the frequency
      #   output.type: indicates what type of release vintage is used
      #   vintage.dates: list of historical dates for previous versions
      #
      # Returns:
      #   A data.frame of observation data
      
      params <- rbind(c("series_id", series.id),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      c("limit", limit),
                      c("offset", offset),
                      c("sort_order", sort.order),
                      c("observation_start", as.character(observation.start)),
                      c("observation_end", as.character(observation.end)),
                      c("units", units),
                      if (!is.null(frequency)) {c("frequency", frequency)},
                      if (!is.null(frequency)) {c("aggregation_method", 
                                                  aggregation.method)},
                      c("output_type", output.type),
                      if (!is.null(vintage.dates)) {
                        c("vintage_dates", vintage.dates)
                      })
      # query api
      rtn <- api.query("/series/observations", params)
      # convert observations lists into vectors
      ans <- parLapply(cluster, rtn$observations, 
                                    fun = function(dat) {
                                      c(realtime.start = dat$realtime_start,
                                        realtime.end = dat$realtime_end,
                                        date = dat$date,
                                        value = dat$value)
                                    })
      # combine results into data.frame
      ans <- data.frame(do.call("rbind", ans), stringsAsFactors = FALSE)
      # convert fields
      ans$realtime.start <- as.Date(ans$realtime.start)
      ans$realtime.end <- as.Date(ans$realtime.end)
      ans$date <- as.Date(ans$date)
      ans$value <- suppressWarnings(as.numeric(ans$value))
      #set attributes
      attr(ans, "series.id") <- series.id
      attr(ans, "realtime.start") <- rtn$realtime_start
      attr(ans, "realtime.end") <- rtn$realtime_end
      attr(ans, "observation.start") <- rtn$observation_start
      attr(ans, "observation.end") <- rtn$observation_end
      attr(ans, "units") <- rtn$units
      attr(ans, "output.type") <- rtn$output_type
      attr(ans, "file.type") <- rtn$file_type
      attr(ans, "order.by") <- rtn$order_by
      attr(ans, "sort.order") <- rtn$sort_order
      attr(ans, "count") <- rtn$count
      attr(ans, "offset") <- rtn$offset
      attr(ans, "limit") <- rtn$limit
      # set class
      class(ans) <- "rfred.series.observations"
      return(ans)
    }
    api.series.release <- function() {}
    api.series.search <- function() {}
    api.series.search.tags <- function() {}
    api.series.search.related.tags <- function() {}
    api.series.tags <- function() {}
    api.series.updates <- function() {}
    api.series.vintagedates <- function() {}
    
    api.sources <- function() {}
    api.source <- function() {}
    api.source.releases <- function() {}
    
    api.tags <- function() {}
    api.related.tags <- function() {}
    api.tags.series <- function() {}
  }, envir = ans)
  
  class(ans) <- "rfred"
  
  return(ans)
}

rfred.obs <- function(fred, series, ...) {
  UseMethod("rfred.obs", series)
}
rfred.obs.character <- function(fred, series, ...) {
  fun <- local(function(x) {
    api.series.observations(x)
  }, envir = fred)
  fun(series)
}
rfred.obs.rfred.series <- function(fred, series, ...) {
  rfred.obs(fred, series$id)
}

# print.rfred.series.observations <- function(x) {
#   ans <- paste0(apply(cbind(encodeString(c("RealTime Start:", "RealTime End:",
#                                           "Observation Start:", 
#                                           "Observation End:", "Units:", 
#                                           "Output Type:", "File Type:",
#                                           "Order by:", "Sort Order:", "Offset:",
#                                           "Limit:", "Count:"), width = 20),
#                            c(attr(x, "realtime.start"), attr(x, "realtime.end"),
#                              attr(x, "observation.start"), 
#                              attr(x, "observation.end"), attr(x, "units"),
#                              attr(x, "output.type"), attr(x, "file.type"), 
#                              attr(x, "order.by"), attr(x, "sort.order"),
#                              attr(x, "offset"), attr(x, "limit"), 
#                              attr(x, "count"))), 1, paste0, collapse = ""), 
#                 collapse = "\n")
#  cat(ans)
# }