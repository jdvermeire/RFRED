
# Class Functions ---------------------------------------------------------

rfred.category <- function(obj) {
  # parses a list object into a category
  #
  # Args:
  #   obj: the object to parse
  #
  # Returns:
  #   the category object
  
  ans <- list(id = as.integer(obj$id),
              name = obj$name,
              parent_id = as.integer(obj$parent_id))
  
  class(ans) <- "rfred.category"
  return(ans)
}

rfred.release <- function(obj) {
  # parses a list object into a release
  #
  # Args:
  #   obj: the object to parse
  #
  # Returns:
  #   the release object
  
  ans <- list(id = as.integer(obj$id),
              realtime_start = as.Date(obj$realtime_start),
              realtime_end = as.Date(obj$realtime_end),
              name = obj$name,
              press_release = as.logical(obj$press_release),
              link = obj$link,
              notes = obj$notes)
  
  class(ans) <- "rfred.release"
  return(ans)
}

rfred.series <- function(obj) {
  # parses a list object into a series
  #
  # Args:
  #   obj: the object to parse
  #
  # Returns:
  #   the series object
  
  ans <- list(id = obj$id,
              realtime_start = as.Date(obj$realtime_start),
              realtime_end = as.Date(obj$realtime_end),
              title = obj$title,
              observation_start = as.Date(obj$observation_start),
              observation_end = as.Date(obj$observation_end),
              frequency = obj$frequency,
              frequency_short = obj$frequency_short,
              units = obj$units,
              units_short = obj$units_short,
              seasonal_adjustment = obj$seasonal_adjustment,
              seasonal_adjustment.short = obj$seasonal_adjustment_short,
              last_updated = as.POSIXct(paste0(obj$last_updated, "00"),
                                        format = "%Y-%m-%d %H:%M:%S%z"),
              popularity = as.integer(obj$popularity),
              notes = obj$notes)
  
  class(ans) <- "rfred.series"
  return(ans)
}

rfred.source <- function(obj) {
  # parses a list object into a source
  #
  # Args:
  #   obj: the object to parse
  #
  # Returns:
  #   the source object
  
  ans <- list(id = as.integer(obj$id),
              realtime_start = as.Date(obj$realtime_start),
              realtime_end = as.Date(obj$realtime_end),
              name = obj$name,
              link = obj$link,
              notes = obj$notes)

  class(ans) <- "rfred.source"
  return(ans)
}

rfred.tag <- function(obj) {
  # parses a list object into a tag
  #
  # Args:
  #   obj: the object to parse
  #
  # Returns:
  #   the tag object
  
  ans <- list(id = obj$name,
              name = obj$name,
              group_id = obj$group_id,
              notes = obj$notes,
              created = as.POSIXct(paste0(obj$created, "00"), 
                                   format = "%Y-%m-%d %H:%M:%S%z"),
              popularity = as.integer(obj$popularity),
              series_count = as.integer(obj$series_count))

  class(ans) <- "rfred.tag"
  return(ans)
}

rfred.list <- function(obj, obj_type) {
  # creates a special list of rfred objects. includes some helper functions
  # for accessing information within the list
  #
  # Args:
  #   obj: the object to parse
  #   obj_type: the type of object to parse {category, release, series, etc...}
  #
  # Returns:
  #   a rfred.list object
  
  ans <- lapply(obj, FUN = eval(as.symbol(paste0("rfred.", obj_type))))
  names(ans) <- do.call("rbind", lapply(ans, FUN = function(x) {x$id}))
  ans$view <- function() {
    as.data.frame(do.call("rbind", lapply(ans[c(-"view",-"get")], FUN = c)))
  }
  ans$get <- function(x) {
    x <- as.character(x)
    ans[[x]]
  }
  
  class(ans) <- "rfred.list"
  return(ans)
}