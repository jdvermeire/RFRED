
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



# Series Queries ----------------------------------------------------------

q.series <- function(series_id,
                     realtime_start = as.character(Sys.Date()),
                     realtime_end = as.character(Sys.Date())) {
  
}

q.series.categories <- function(series_id,
                                realtime_start = as.character(Sys.Date()),
                                realtime_end = as.character(Sys.Date())) {
  
}

q.series.observations <- function(series_id,
                                  realtime_start = as.character(Sys.Date()),
                                  realtime_end = as.character(Sys.Date()),
                                  limit = 100000,
                                  offset = 0,
                                  sort_order = c("asc","desc")[1],
                                  observation_start = "1776-07-04",
                                  observation_end = "9999-12-31",
                                  units = c("lin", "chg", "ch1", "pch", "pc1",
                                            "pca", "cch", "cch", "cca", 
                                            "log")[1],
                                  frequency = c("d", "w", "bw", "m", "q", "sa",
                                                "a", "wef", "weth", "wew", 
                                                "wetu", "wem", "wesu", "wesa",
                                                "bwew", "bwem")[1],
                                  aggregation_method = c("avg", "sum", "eop")[1],
                                  output_type = c(1, 2, 3, 4)[1],
                                  vintage_dates) {
  
}

q.series.release <- function(series_id,
                             realtime_start = as.character(Sys.Date()),
                             realtime_end = as.character(Sys.Date())) {
  
}

q.series.search <- function(search_text,
                            search_type = c("full_text", "series_id")[1],
                            realtime_start = as.character(Sys.Date()),
                            realtime_end = as.character(Sys.Date()),
                            limit = 1000,
                            offset = 0,
                            order_by = c("search_rank", "series_id", "title",
                                         "units", "frequency", 
                                         "seasonal_adjustment", "realtime_start",
                                         "realtime_end")[1],
                            sort_order = c("asc", "desc")[1],
                            filter_variable = c("none", "frequency", "units",
                                                "seasonal_adjustment")[1],
                            filter_value,
                            tag_names,
                            exclude_tag_names) {
  
}

q.series.search.tags <- function(series_search_text,
                                 realtime_start = as.character(Sys.Date()),
                                 realtime_end = as.character(Sys.Date()),
                                 tag_names,
                                 exclude_tag_names,
                                 tag_group_id = c("freq", "gen", "geo", "geot",
                                                  "rls", "seas", "src")[1],
                                 tag_search_text,
                                 limit = 1000,
                                 offset = 0,
                                 order_by = c("series_count", "popularity", 
                                              "created", "name", "group_id")[1],
                                 sort_order = c("asc", "desc")[1]) {
  
}

q.series.search.related_tags <- function(
  series_search_text,
  realtime_start = as.character(Sys.Date()),
  realtime_end = as.character(Sys.Date()),
  tag_names,
  exclude_tag_names,
  tag_group_id = c("freq", "gen", "geo", "geot",
                   "rls", "seas", "src")[1],
  tag_search_text,
  limit = 1000,
  offset = 0,
  order_by = c("series_count", "popularity", 
               "created", "name", "group_id")[1],
  sort_order = c("asc", "desc")[1])
) {
  
}

q.series.tags <- function(series_id,
                          realtime_start = as.character(Sys.Date()),
                          realtime_end = as.character(Sys.Date()),
                          order_by = c("series_count", "popularity", 
                                       "created", "name", "group_id")[1],
                          sort_order = c("asc", "desc")[1]) {
  
}

q.series.updates <- function(realtime_start = as.character(Sys.Date()),
                             realtime_end = as.character(Sys.Date()),
                             limit = 100,
                             offset = 0,
                             filter_value = c("all", "macro", "regional")[1]) {
  
}

q.series.vintagedates <- function(series_id,
                                  realtime_start = "1776-04-07",
                                  realtime_end = "9999-12-31",
                                  limit = 10000,
                                  offset = 0,
                                  sort_order = c("asc", "desc")[1]) {
  
}



# Source Queries ----------------------------------------------------------

q.sources <- function(realtime_start = as.character(Sys.Date()),
                      realtime_end = as.character(Sys.Date()),
                      limit = 1000,
                      offset = 0,
                      order_by = c("source_id", "name", "realtime_start",
                                   "realtime_end")[1],
                      sort_order = c("asc", "desc")[1]) {
  
}

q.source <- function(source_id,
                     realtime_start = as.character(Sys.Date()),
                     realtime_end = as.character(Sys.Date()) {
  
}

q.source.releases <- function(source_id,
                              realtime_start = as.character(Sys.Date()),
                              realtime_end = as.character(Sys.Date()),
                              limit = 1000,
                              offset = 0,
                              order_by = c("release_id", "name", "press_release",
                                           "realtime_start", "realtime_end")[1],
                              sort_order = c("asc", "desc")[1]) {
  
}


# Tag Queries -------------------------------------------------------------

q.tags <- function(realtime_start = as.character(Sys.Date()),
                   realtime_end = as.character(Sys.Date()),
                   tag_names,
                   exclude_tag_names,
                   tag_group_id = c("freq", "gen", "geo", "geot", "rls", "seas", 
                                    "src")[1],
                   search_text,
                   limit = 1000,
                   offset = 0,
                   order_by = c("series_count", "popularity", "created", "name", 
                                "group_id")[1],
                   sort_order = c("asc", "desc")[1]) {
  
}

q.related_tags <- function(realtime_start = as.character(Sys.Date()),
                           realtime_end = as.character(Sys.Date()),
                           tag_names,
                           exclude_tag_names,
                           tag_group_id = c("freq", "gen", "geo", "geot", "rls", 
                                            "seas", "src")[1],
                           search_text,
                           limit = 1000,
                           offset = 0,
                           order_by = c("series_count", "popularity", "created", 
                                        "name", "group_id")[1],
                           sort_order = c("asc", "desc")[1]) {
  
}

q.tags.series <- function(tag_names,
                          exclude_tag_names,
                          realtime_start = as.character(Sys.Date()),
                          realtime_end = as.character(Sys.Date()),
                          limit = 1000,
                          offset = 0,
                          order_by = c("series_id", "title", "frequency",
                                       "seasonal_adjustment", "realtime_start",
                                       "realtime_end", "last_updated",
                                       "observation_start", "observation_end",
                                       "popularity")[1],
                          sort_order = c("asc", "desc")[1]) {
  
}
