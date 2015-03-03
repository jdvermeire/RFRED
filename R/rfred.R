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
      
      params <- rbind(api.params.k, params,
                      stringsAsFactors = FALSE)  # add constant parameters
      qry <- paste(paste0(api.url, op), 
                   paste(parApply(cluster, params, 1, paste, collapse = "="), 
                         collapse = "&"), sep = "?")
      qry.con <- url(qry)  # create connection url
      on.exit(close(qry.con))  # make sure connection closes properly
      ans <- fromJSON(file = qry.con)  # parse json
      if (isOpen(qry.con)) close(qry.con)
      return(ans)
    }
    
    # parse functions
    parse.category <- function(obj) {
      # Parses list object as category
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns:
      #   a category object
      
      ans <- list(id = as.integer(obj$id),
                  name = obj$name,
                  parent.id = as.integer(obj$parent_id))
      #set class
      class(ans) <- "rfred.category"
      return(ans)
    }
    parse.category.list <- function(obj) {
      # Parses list of categories from FRED
      #
      # Args:
      #   obj: the list of categories object
      #
      # Returns:
      #   The parsed list of categories
      
      # init ans
      ans <- NULL
      # parse into "rfred.category" class
      ans <- parLapply(cluster, obj, fun = parse.category)
      # parse into a data.frame for easy viewing
      cat.df <- parLapply(cluster, ans, fun = c)
      cat.df <- data.frame(do.call("rbind", cat.df), stringsAsFactors = FALSE)
      # use ids as names for list elements
      names(ans) <- cat.df$id
      # add data.frame to ans
      ans$list <- cat.df
      # add function to easily get category
      ans$get <- function(x) {
        # Gets category from list by id
        #
        # Args:
        #   x: character or integer value of id
        #
        # Returns:
        #   A category
        
        # TODO(jdvermeire): add error handling for unknown ids
        x <- as.character(x)
        ans[[x]]
      }
      # set class
      class(ans) <- "rfred.category.list"
      return(ans)
    }
    parse.release <- function(obj) {
      # Parses FRED returned release
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns:
      #   The release
      
      # init ans
      ans <- list(id = as.integer(obj$id),
                  realtime.start = as.Date(obj$realtime_start),
                  realtime.end = as.Date(obj$realtime_end),
                  name = obj$name,
                  press.release = as.logical(obj$press_release),
                  link = obj$link,
                  notes = obj$notes)
      # set class
      class(ans) <- "rfred.release"
      return(ans)
    }
    parse.release.list <- function(obj) {
      # Parses FRED returned list of releases
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns
      #   A list of releases
      
      # init ans
      ans <- NULL
      # parse into "rfred.release" class
      ans <- parLapply(cluster, obj, fun = parse.release)
      # parse into a data.frame for easy viewing
      rls.df <- parLapply(cluster, ans, fun = c)
      rls.df <- data.frame(do.call("rbind", rls.df), stringsAsFactors = FALSE)
      # use ids as names for list elements
      names(ans) <- rls.df$id
      # add data.frame to ans
      ans$list <- rls.df
      # add function to easily get release
      ans$get <- function(x) {
        # Gets release from list by id
        #
        # Args:
        #   x: character or integer value of id
        #
        # Returns:
        #   A release
        
        # TODO(jdvermeire): add error handling for unknown ids
        x <- as.character(x)
        ans[[x]]
      }
      # set class
      class(ans) <- "rfred.release.list"
      return(ans)
    }
    parse.release.date <- function(obj) {
      # Parses a FRED returned release date
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns:
      #   The release date
      
      # init ans
      ans <- list(id = as.integer(obj$release_id),
                  name = obj$release_name,
                  date = as.Date(obj$date))
      # set class
      class(ans) <- "rfred.release.date"
      return(ans)
    }
    parse.release.date.list <- function(obj) {
      # Parses FRED returned list of release dates
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns
      #   A list of release dates
      
      # init ans
      ans <- NULL
      # parse into "rfred.release.date" class
      ans <- parLapply(cluster, obj, fun = parse.release.date)
      # parse into a data.frame for easy viewing
      rls.df <- parLapply(cluster, ans, fun = c)
      rls.df <- data.frame(do.call("rbind", rls.df), stringsAsFactors = FALSE)
      # use ids as names for list elements
      names(ans) <- rls.df$id
      # add data.frame to ans
      ans$list <- rls.df
      # add function to easily get release date
      ans$get <- function(x) {
        # Gets release date from list by id
        #
        # Args:
        #   x: character or integer value of id
        #
        # Returns:
        #   A release date
        
        # TODO(jdvermeire): add error handling for unknown ids
        x <- as.character(x)
        ans[[x]]
      }
      # set class
      class(ans) <- "rfred.release.date.list"
      return(ans)
    }
    parse.series <- function(obj) {
      # Parses a FRED returned series
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns:
      #   A series
      
      # init ans
      ans <- list(id = obj$id,
                  realtime.start = as.Date(obj$realtime_start),
                  realtime.end = as.Date(obj$realtime_end),
                  title = obj$title,
                  observation.start = as.Date(obj$observation_start),
                  observation.end = as.Date(obj$observation_end),
                  frequency = obj$frequency,
                  frequency.short = obj$frequency_short,
                  units = obj$units,
                  units.short = obj$units_short,
                  seasonal.adjustment = obj$seasonal_adjustment,
                  seasonal.adjustment.short = obj$seasonal_adjustment_short,
                  last.updated = as.POSIXct(paste0(obj$last_updated, "00"),
                                            format = "%Y-%m-%d %H:%M:%S%z"),
                  popularity = as.integer(obj$popularity),
                  notes = obj$notes)
      # set class
      class(ans) <- "rfred.series"
      return(ans)
    }
    parse.series.list <- function(obj) {
      # Parses FRED returned list of series
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns
      #   A list of series
      
      # init ans
      ans <- NULL
      # parse into "rfred.series" class
      ans <- parLapply(cluster, obj, fun = parse.series)
      # parse into a data.frame for easy viewing
      srs.df <- parLapply(cluster, ans, fun = c)
      srs.df <- data.frame(do.call("rbind", srs.df), stringsAsFactors = FALSE)
      # use ids as names for list elements
      names(ans) <- srs.df$id
      # add data.frame to ans
      ans$list <- srs.df
      # add function to easily get series
      ans$get <- function(x) {
        # Gets series from list by id
        #
        # Args:
        #   x: character or integer value of id
        #
        # Returns:
        #   A series
        
        # TODO(jdvermeire): add error handling for unknown ids
        x <- as.character(x)
        ans[[x]]
      }
      # set class
      class(ans) <- "rfred.series.list"
      return(ans)
    }
    parse.source <- function(obj) {
      # Parses a FRED returned source
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns:
      #   A source
      
      # init ans
      ans <- list(id = as.integer(obj$id),
                  realtime.start = as.Date(obj$realtime_start),
                  realtime.end = as.Date(obj$realtime_end),
                  name = obj$name,
                  link = obj$link,
                  notes = obj$notes)
      # set class
      class(ans) <- "rfred.source"
      return(ans)
    }
    parse.source.list <- function(obj) {
      # Parses FRED returned list of source
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns
      #   A list of source
      
      # init ans
      ans <- NULL
      # parse into "rfred.source" class
      ans <- parLapply(cluster, obj, fun = parse.source)
      # parse into a data.frame for easy viewing
      src.df <- parLapply(cluster, ans, fun = c)
      src.df <- data.frame(do.call("rbind", src.df), stringsAsFactors = FALSE)
      # use ids as names for list elements
      names(ans) <- src.df$id
      # add data.frame to ans
      ans$list <- src.df
      # add function to easily get source
      ans$get <- function(x) {
        # Gets source from list by id
        #
        # Args:
        #   x: character or integer value of id
        #
        # Returns:
        #   A source
        
        # TODO(jdvermeire): add error handling for unknown ids
        x <- as.character(x)
        ans[[x]]
      }
      # set class
      class(ans) <- "rfred.source.list"
      return(ans)
    }
    parse.tag <- function(obj) {
      # Parses a FRED returned tag
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns:
      #   A tag
      
      # init ans
      ans <- list(name = obj$name,
                  group.id = obj$group_id,
                  notes = obj$notes,
                  created = as.POSIXct(paste0(obj$created, "00"), 
                                       format = "%Y-%m-%d %H:%M:%S%z"),
                  popularity = as.integer(obj$popularity),
                  series.count = as.integer(obj$series_count))
      # set class
      class(ans) <- "rfred.tag"
      return(ans)
    }
    parse.tag.list <- function(obj) {
      # Parses FRED returned list of tags
      #
      # Args:
      #   obj: the object to parse
      #
      # Returns
      #   A list of tags
      
      # init ans
      ans <- NULL
      # parse into "rfred.tag" class
      ans <- parLapply(cluster, obj, fun = parse.tag)
      # parse into a data.frame for easy viewing
      tag.df <- parLapply(cluster, ans, fun = c)
      tag.df <- data.frame(do.call("rbind", tag.df), stringsAsFactors = FALSE)
      # use names for list elements
      names(ans) <- tag.df$name
      # add data.frame to ans
      ans$list <- tag.df
      # add function to easily get tag
      ans$get <- function(x) {
        # Gets tag from list by name
        #
        # Args:
        #   x: character or integer value of name
        #
        # Returns:
        #   A tag
        
        # TODO(jdvermeire): add error handling for unknown names
        x <- as.character(x)
        ans[[x]]
      }
      # set class
      class(ans) <- "rfred.tag.list"
      return(ans)
    }
    
    # wrapper functions
    api.category <- function(category.id = 0) {
      # Retrieves category from FRED
      #
      # Args:
      #   category.id: the id for the category
      #
      # Returns:
      #   The category
      
      # set params arg
      params <- c("category_id", category.id)
      # query api
      rtn <- api.query("/category", params)$categories
      # parse category
      ans <- parse.category(rtn)
      return(ans)      
    }

    api.generic.category.list <- function(api.call, id.name, id.value,
                                          realtime.start, realtime.end) {
      # set params
      params <- rbind(c(id.name, id.value),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      stringsAsFactors = FALSE)
      # execute query
      rtn <- api.query(api.call, params)$categories
      # parse
      ans <- parse.category.list(rtn)
      return(ans)
    }
    api.category.children <- function(category.id = 0, 
                                      realtime.start = Sys.Date(), 
                                      realtime.end = Sys.Date()) {
      # Retrieves a list of child categories for the given category
      #
      # Args:
      #   category.id: the id for the parent category
      #   realtime.start: the realtime period start
      #   realtime.end: the realtime period end
      #
      # Returns:
      #   A list of categories that are childern of the parent
      
      api.generic.category.list("/category/children", "category_id", 
                                category.id, realtime.start, realtime.end)
    }
    api.category.related <- function(category.id = 0, 
                                     realtime.start = Sys.Date(), 
                                     realtime.end = Sys.Date()) {
      # Retrieves a list of related categories for the given category
      #
      # Args:
      #   category.id: the category id
      #   realtime.start: the realtime period start
      #   realtime.end: the realtime period end
      #
      # Returns:
      #   A list of categories related to the given category
      
      api.generic.category.list("/category/related", "category_id", 
                                category.id, realtime.start, realtime.end)
    }
    api.series.categories <- function(series.id,
                                      realtime.start = Sys.Date(),
                                      realtime.end = Sys.Date()) {
      # Retrieves a list of categories for a series
      #
      # Args:
      #   series.id: the series id
      #   realtime.start: the realtime period start
      #   realtime.end: the realtime period end
      #
      # Returns:
      #   A list of categories
      api.generic.category.list("/series/categories", "series_id", series.id,
                                realtime.start, realtime.end)
    }
    
    api.generic.release <- function(api.call, id.name, id.value, realtime.start,
                                    realtime.end) {
      # set params
      params <- rbind(c(id.name, id.value),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      stringsAsFactors = FALSE)
      # execute query
      rtn <- api.query(api.call, params)$releases
      # parse
      ans <- parse.release(rtn)
      return(ans)
    }
    api.release <- function(release.id,
                            realtime.start = Sys.Date(),
                            realtime.end = Sys.Date()) {
      # Retrieves a release
      #
      # Args:
      #   release.id: the release id
      #   realtime.start: the realtime period start
      #   realtime.end: the realtime period end
      #
      # Returns:
      #   A release
      api.generic.release("/release", "release_id", release.id, realtime.start,
                          realtime.end)
    }
    api.series.release <- function(series.id,
                                   realtime.start = Sys.Date(),
                                   realtime.end = Sys.Date()) {
      # Retrieves a release for a series
      #
      # Args:
      #   series.id: the series id
      #   realtime.start: the realtime period start
      #   realtime.end: the realtime period end
      #
      # Returns:
      #   A release
      api.generic.release("/series/release", "series_id", series.id,
                          realtime.start, realtime.end)
    }

    api.generic.release.list <- function(api.call, id.name, id.value, 
                                         realtime.start, realtime.end, limit,
                                         offset, order.by, sort.order) {
      # set params
      params <- rbind(c(id.name, id.value),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      c("limit", limit),
                      c("offset", offset),
                      c("order_by", order.by[1]),
                      c("sort_order", sort.order[1]),
                      stringsAsFactors = FALSE)
      # execute query
      rtn <- api.query(api.call, params)$releases
      #parse results
      ans <- parse.release.list(rtn)
      return(ans)
    }
    api.releases <- function(realtime.start = Sys.Date(),
                             realtime.end = Sys.Date(),
                             limit = 1000L,
                             offset = 0L,
                             order.by = c("release_id",
                                          "name",
                                          "press_release",
                                          "realtime_start",
                                          "realtime_end"),
                             sort.order = c("asc", "desc")) {
      # Retrieves a list of releases
      #
      # Args:
      #   realtime.start: the realtime start date
      #   realtime.end: the realtime end date
      #   limit: the max number of records to return
      #   offset: the pagenation offset
      #   order.by: the field by which to sort
      #   sort.order: the direction of sort
      #
      # Returns:
      #   a list of releases
      api.generic.release.list("/releases", NULL, NULL, realtime.start,
                               realtime.end, limit, offset, order.by, 
                               sort.order)
    }
    api.source.releases <- function(source.id,
                                    realtime.start = Sys.Date(),
                                    realtime.end = Sys.Date(),
                                    limit = 1000L,
                                    offset = 0L,
                                    order.by = c("release_id",
                                                 "name",
                                                 "press_release",
                                                 "realtime_start",
                                                 "realtime_end"),
                                    sort.order = c("asc", "desc")) {
      # Retrieves a list of releases for a source
      #
      # Args:
      #   source.id: the source id
      #   realtime.start: the realtime start date
      #   realtime.end: the realtime end date
      #   limit: the page limit
      #   offset: the page offset
      #   order.by: the field by which to sort
      #   sort.order: the direction of sort
      #
      # Returns:
      #   a list of releases
      api.generic.release.list("/source/releases", "source_id", source.id, 
                               realtime.start, realtime.end, limit, offset, 
                               order.by, sort.order)
    }
    
    api.generic.release.date.list <- function(api.call, id.name, id.value,
                                              realtime.start, realtime.end,
                                              limit, offset, order.by,
                                              sort.order,
                                              include.dates.with.no.data) {
      # set params
      params <- rbind(c(id.name, id.value),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      c("limit", limit),
                      c("offset", offset),
                      c("order_by", order.by[1]),
                      c("sort_order", sort.order[1]),
                      c("include_release_dates_with_no_data",
                        tolower(as.character(include.dates.with.no.data[1]))),
                      stringsAsFactors = FALSE)
      # execute query
      rtn <- api.query(api.call, params)$release_dates
      # parse
      ans <- parse.release.date.list(rtn)
      return(ans)
    }
    api.releases.dates <- function(realtime.start = paste(format(Sys.Date(),
                                                                 format = "%Y"),
                                                          "01", "01", sep = "-"),
                                   realtime.end = "9999-12-31",
                                   limit = 1000L,
                                   offset = 0L,
                                   order.by = c("release_date", "release_id",
                                                "release_name"),
                                   sort.order = c("desc", "asc"),
                                   include.dates.with.no.data = c(FALSE, TRUE)) 
    {
      # Retrieves a list of dates for all releases
      #
      # Args:
      #   realtime.start: the realtime start date
      #   realtime.end: the realtime end date
      #   limit: the page limit
      #   offset: the page offset
      #   order.by: the field by which to sort
      #   sort.order: the sort direction
      #   include.dates.with.no.data: logical flag to include dates with no data
      #
      # Returns:
      #   a list of release dates
      api.generic.release.date.list("/releases/dates", NULL, NULL, 
                                    realtime.start, realtime.end, limit, offset,
                                    order.by, sort.order,
                                    include.dates.with.no.data)
    }
    api.release.dates <- function(release.id,
                                  realtime.start = "1776-07-04",
                                  realtime.end = "9999-12-31",
                                  limit = 1000L,
                                  offset = 0L,
                                  sort.order = c("asc", "desc"),
                                  include.dates.with.no.data = c(FALSE, TRUE)) {
      # Retrieves a list of dates for a release
      #
      # Args:
      #   release.id: the release id
      #   realtime.start: the realtime start date
      #   realtime.end: the realtime end date
      #   limit: the page limit
      #   offset: the page offset
      #   order.by: the field by which to sort
      #   sort.order: the sort direction
      #   include.dates.with.no.data: logical flag to include dates with no data
      #
      # Returns:
      #   a list of release dates
      api.generic.release.date.list("/release/dates", "release_id", release.id, 
                                    realtime.start, realtime.end, limit, offset,
                                    order.by, sort.order,
                                    include.dates.with.no.data)
    }

    api.series <- function() {
      
    }

    api.generic.series.list <- function(api.call, id.name, id.value,
                                        realtime.start, realtime.end, limit,
                                        offset, order.by, sort.order, 
                                        filter.variable, filter.value,
                                        tag.names, exclude.tag.names) {
      # set params
      params <- rbind(c(id.name, id.value),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      c("limit", limit),
                      c("offset", offset),
                      c("order_by", order.by[1]),
                      c("sort_order", sort.order[1]),
                      if (!is.null(filter.variable)) {c("filter_variable",
                                                        filter.variable)},
                      if (!is.null(filter.variable)) {c("filter_value",
                                                        filter.value)},
                      if (!is.null(tag.names)) {
                        c("tag_names", paste0(tag.names, collapse = ";"))
                      },
                      if (!is.null(exclude.tag.names)) {
                        c("exclude_tag_names", 
                          paste0(exclude.tag.names, collapse = ";"))
                      })
      # execute query
      rtn <- api.query(api.call, params)$seriess
      # parse result
      ans <- parse.series.list(rtn)
      return(ans)      
    }
    api.category.series <- function(category.id, 
                                    realtime.start = Sys.Date(), 
                                    realtime.end = Sys.Date(),
                                    limit = 1000L, 
                                    offset = 0L, 
                                    order.by = c("series_id", "title", "units",
                                                 "frequency", 
                                                 "seasonal_adjustment",
                                                 "realtime_start", 
                                                 "realtime_end",
                                                 "last_updated",
                                                 "observation_start",
                                                 "observation_end",
                                                 "popularity"), 
                                    sort.order = c("asc", "desc"),
                                    filter.variable, 
                                    filter.value, 
                                    tag.names,
                                    exclude.tag.names) {
      # Retrieves a list of series for a category
      #
      # Args:
      #   category.id: the id for the category
      #   realtime.start: the realtime start
      #   realtime.end: the realtime end
      #   limit: the max number of records returned by the query
      #   offset: the paging offset
      #   order.by: the attribute to order the results
      #   sort.order: ascending or descending
      #   filter.variable: field to filter on
      #   filter.value: the value to filter
      #   tag.names: list of tags to filter on
      #   exclude.tag.names: list of tags to exclude
      #
      # Returns:
      #   A list of series
      api.generic.series.list("/category/series", "category_id", category.id,
                              realtime.start, realtime.end, limit, offset,
                              order.by, sort.order, filter.variable, 
                              filter.value, tag.names, exclude.tag.names)
    }
    api.release.series <- function(release.id, 
                                   realtime.start = Sys.Date(), 
                                   realtime.end = Sys.Date(),
                                   limit = 1000L, 
                                   offset = 0L, 
                                   order.by = c("series_id", "title", "units",
                                                "frequency", 
                                                "seasonal_adjustment",
                                                "realtime_start", 
                                                "realtime_end",
                                                "last_updated",
                                                "observation_start",
                                                "observation_end",
                                                "popularity"), 
                                   sort.order = c("asc", "desc"),
                                   filter.variable, 
                                   filter.value, 
                                   tag.names,
                                   exclude.tag.names) {
      # Retrieves a list of series for a release
      #
      # Args:
      #   release.id: the id for the release
      #   realtime.start: the realtime start
      #   realtime.end: the realtime end
      #   limit: the max number of records returned by the query
      #   offset: the paging offset
      #   order.by: the attribute to order the results
      #   sort.order: ascending or descending
      #   filter.variable: field to filter on
      #   filter.value: the value to filter
      #   tag.names: list of tags to filter on
      #   exclude.tag.names: list of tags to exclude
      #
      # Returns:
      #   A list of series
      api.generic.series.list("/release/series", "release_id", release.id,
                              realtime.start, realtime.end, limit, offset,
                              order.by, sort.order, filter.variable, 
                              filter.value, tag.names, exclude.tag.names)
    }
    
    api.category.tags <- function(category.id, 
                                  realtime.start = Sys.Date(), 
                                  realtime.end = Sys.Date(),
                                  tag.names,
                                  exclude.tag.names,
                                  tag.group.id, 
                                  search.text, 
                                  limit,
                                  offset, 
                                  order.by = c("series_count",
                                               "popularity",
                                               "created",
                                               "name",
                                               "group_id"), 
                                  sort.order = c("asc", "desc")) {
      # TODO(jdvermeire): error handling
      # set params
      params <- rbind(c("category_id", category.id),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      if (!is.null(tag.names)) {
                        c("tag_names", paste0(tag.names, collapse = ";"))
                      },
                      if (!is.null(exclude.tag.names)) {
                        c("exclude_tag_names", paste0(exclude.tag.names,
                                                      collapse = ";"))
                      },
                      if (!is.null(tag.group.id)) {
                        c("tag_group_id", tag.group.id[1])
                      },
                      if (!is.null(search.text)) {
                        c("search_text", paste0(search.text, collapse = " "))
                      },
                      c("limit", limit),
                      c("offset", offset),
                      c("order_by", order.by[1]),
                      c("sort_order", sort.order[1]))
      # execute query
      rtn <- api.query("/category/tags", params)$tags
      # parse results
      ans <- parse.tag.list(rtn)
      return(ans)
    }
    api.category.related.tags <- function(category.id, 
                                          realtime.start = Sys.Date(), 
                                          realtime.end = Sys.Date(),
                                          tag.names,
                                          exclude.tag.names,
                                          tag.group.id, 
                                          search.text, 
                                          limit,
                                          offset, 
                                          order.by = c("series_count",
                                                       "popularity",
                                                       "created",
                                                       "name",
                                                       "group_id"), 
                                          sort.order = c("asc", "desc")) {
      # TODO(jdvermeire): error handling
      # set params
      params <- rbind(c("category_id", category.id),
                      c("realtime_start", as.character(realtime.start)),
                      c("realtime_end", as.character(realtime.end)),
                      if (!is.null(tag.names)) {
                        c("tag_names", paste0(tag.names, collapse = ";"))
                      },
                      if (!is.null(exclude.tag.names)) {
                        c("exclude_tag_names", paste0(exclude.tag.names,
                                                      collapse = ";"))
                      },
                      if (!is.null(tag.group.id)) {
                        c("tag_group_id", tag.group.id[1])
                      },
                      if (!is.null(search.text)) {
                        c("search_text", paste0(search.text, collapse = " "))
                      },
                      c("limit", limit),
                      c("offset", offset),
                      c("order_by", order.by[1]),
                      c("sort_order", sort.order[1]))
      # execute query
      rtn <- api.query("/category/related_tags", params)$tags
      # parse results
      ans <- parse.tag.list(rtn)
      return(ans)
      
    }
    
    api.release.sources <- function() {
      
    }
    api.release.tags <- function() {
      
    }
    api.release.related.tags <- function() {
      
    }
    
    api.series.categories <- function() {
      
    }
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
                      }, stringsAsFactors = FALSE)
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
    api.series.release <- function() {
      
    }
    api.series.search <- function() {
      
    }
    api.series.search.tags <- function() {
      
    }
    api.series.search.related.tags <- function() {
      
    }
    api.series.tags <- function() {
      
    }
    api.series.updates <- function() {
      
    }
    api.series.vintagedates <- function() {
      
    }
    
    api.sources <- function() {
      
    }
    api.source <- function() {
      
    }
    api.source.releases <- function() {
      
    }
    
    api.tags <- function() {
      
    }
    api.related.tags <- function() {
      
    }
    api.tags.series <- function() {
      
    }
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
  rfred.obs(fred, series$id, ...)
}

