#' Retrieve the base uri for either opendap or html service
#'
#' @export
#' @param what character, the name of the desired service
#' @return URI
grads_base_uri <- function(what = c("opendap", "html")[1]){
  switch(tolower(what[1]),
         "html" = "https://nomads.ncep.noaa.gov/dods/nam",
         "http://nomads.ncep.noaa.gov:80/dods/nam")
}

#' Generate an opendap uri
#'
#' @export
#' @param date Date or character, the date to query
#' @param product character, the product to query
#' @param base_uri, character, the base grads uri
grads_uri <- function(date = Sys.Date(), product = "nam_00z",
                        base_uri = grads_base_uri(what = "opendap")){

  if (!inherits(date, "Date")) date <- as.Date(date)
  file.path(base_uri[1],
            format(date[1], "nam%Y%m%d"),
            product[1])
}
