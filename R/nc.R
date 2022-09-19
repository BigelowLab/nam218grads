#' Open an opendap resource
#'
#' @export
#' @param uri character, the uri for the resource
#' @param tries numeric, number of tries before failing
#' @param sleep numeric, seconds to pause between attempts to connect
#' @return ncdf4 object or NULL
open_grads <- function(uri, tries = 3, sleep = 3){

  i <- 1
  while(i <= tries){
    x <- try(ncdf4::nc_open(uri[1]))
    if (inherits(x, 'ncdf4')) break;
    Sys.sleep(sleep) # just chill
    i <- i + 1
  }

  if (inherits(x, "try-error")){
    print(x)
    x <- NULL
  }

  x
}

#' Close a grads resource
#'
#' @export
#' @param x ncdf4 object
close_grads <- function(x){
  ncdf4::nc_close(x)
}

#' Retrieve information about a grads resource
#'
#' @export
#' @param x ncdf4 object
#' @param longnames logical, if TRUE then returve the longnames
#' @return character vector
get_varnames <- function(x, longnames = FALSE){
  stopifnot(inherits(x, "ncdf4"))
  vnames <- names(x$var)
  if (longnames){
    vnames <- sapply(vnames,
                     function(vname){
                        x$var[[vname]]$longname
                     })
  }
  vnames
}
#' @export
#' @describeIn get_varnames Retrieve the names of available dimensions
get_dimnames <- function(x, longnames = FALSE){
  stopifnot(inherits(x, "ncdf4"))
  dnames <- names(x$dim)
  if (longnames){
    dnames <- sapply(dnames,
                     function(dname){
                       x$dim[[dname]]$longname
                     })
  }
  dnames
}
#' @export
#' @describeIn get_varnames Retrieve the dimensions
#' @return named numeric vector of dimension lengths
get_dims <- function(x){
  stopifnot(inherits(x, "ncdf4"))
  dnames <- names(x$dim)
  sapply(dnames,
                 function(dname){
                   x$dim[[dname]]$len
                })
}

#' Retrieve geospatial information about a grads resource
#'
#' @export
#' @param x ncdf4 object
#' @param what character, one of 'time', 'lon', 'lat', 'lev' or 'all'
#' @return varies by value of what
#' \itemize{
#'   \item{\code{lon, lat, lev} numeric vectors}
#'   \item{\code{time} POSIXct vector}
#'   \item{\code{all} list of \code{lon, lat, lev, time} vectors}
#' }
get_loc <- function(x, what = 'time'){
  switch(tolower(what[1]),
         "lon" = get_lon(x),
         "lat" = get_lat(x),
         "lev" = get_lev(x),
         'all' = sapply(c("lon", "lat", "lev", "time"),
                        function(w) get_loc(x, w), simplify = FALSE),
         get_time(x))
}
#' @export
#' @describeIn get_loc retrieve POSIXct times
#' @param shift numeric, see \href{https://github.com/dankelley/oce/issues/738}{oce}
#' @return POSIXct times
get_time <- function(x, shift = -2){
  stopifnot(inherits(x, "ncdf4"))
  origin <- as.POSIXct(x$dim$time$units,
                       format = "days since %Y-%m-%d %H:%M:%OS",
                       tz = "UTC")
  (x$dim$time$vals * (24 * 3600)) + origin + (shift * (24 * 3600))
}
#' @export
#' @describeIn get_loc retrieve numeric longitudes
#' @return numeric longitude locations
get_lon <- function(x){
  stopifnot(inherits(x, "ncdf4"))
  x$dim$lon$vals
}
#' @export
#' @describeIn get_loc retrieve numeric latitudes
#' @return numeric latitude locations
get_lat <- function(x){
  stopifnot(inherits(x, "ncdf4"))
  x$dim$lat$vals
}
#' @export
#' @describeIn get_loc retrieve numeric levels
#' @return numeric level locations
get_lev <- function(x){
  stopifnot(inherits(x, "ncdf4"))
  x$dim$lev$vals
}


#' Get the native bounds for each dimension
#'
#' @export
#' @param x ncdf4 object
#' @param what character, one of 'time', 'lon', 'lat', 'lev' or 'all'
#' @return varies by value of what
#' \itemize{
#'   \item{\code{lon, lat, lev} numeric vectors}
#'   \item{\code{time} POSIXct vector}
#'   \item{\code{all} list}
#'}
get_range <- function(x, what = 'lon'){
  switch(tolower(what[1]),
         "lon" = range(get_lon(x)),
         "lat" = range(get_lat(x)),
         "lev" = range(get_lev(x)),
         'all' = sapply(get_loc(x, 'all'), range, simplify = FALSE),
         range(get_time(x)))
}

#' Retrieve the native bounding box of the data
#'
#' @export
#' @param x ncdf4 object
#' @return 4 element vector of \code{[xmin, xmax, ymin, ymax]}
get_bounds <- function(x){
  unname(c(get_range(x, 'lon'), get_range(x, 'lat')))
}

#' Find the closest indices into a dimension for a given set of values
#'
#' This is useful to convert real dimensional values to array indices for making
#' data requests. The index assigned is always the dimensional step closest.
#'
#' @export
#' @param x ncdf4 object
#' @param value vector of one of values (numeric for lon, lat, lev and POSIXct for time)
#' @param what character, the name of the dimension
#' @param make_rle logical, if TRUE use the first and last elements of value (which
#'        may also be the first) to construct \code{[start, length]} encodings
#' @return numeric vector if either indices closest to requested locations or
#'        two element \code{[start, length]} vector if \code{rle = TRUE}
loc_index <- function(x, value, what = 'lon', make_rle = FALSE){
  loc <- get_loc(x, what)
  r <- sapply(value,
         function(v){
           which.min(abs(loc - v))
         }, simplify = TRUE)
  if (make_rle){
    len <- length(r)
    if (length(r) == 1){
      r <- c(r,1)
    } else {
      r <- c(r[1], r[length(r)] - r[1] + 1)
    }
  }
  r
}



#' Retrieve the dimension names for a variable
#'
#' @export
#' @param x ncdf4 object
#' @param var character, the name of the variable
#' @return character vector of variable names
get_vardims <- function(x, var = get_varnames(x)[1]){
  sapply(x$var[[var]]$dim, "[[", "name")
}

#' Retrieve an array of data from a ncdf object
#'
#' @export
#' @param x ncdf4 object
#' @param var variable id (name)
#' @param index named list of \code{[start,length]} vectors for each dimension
#' \itemize{
#' \item{lon}{such as c(1,913)}
#' \item{lat}{such as c(1,443)}
#' \item{time}{such as c(5, 7) which starts on the 5th and ends on the 12th}
#' \item{lev}{such as c(8,1) which retrieves just the 8th level}
#' }
#' @param collapse_degen logical, if FALSE then preserve length 1 dimensions
#'  NAM218 grib files have variables with either \code{[lon, lat, time]} or
#'  \code{[lon, lat, lev, time]} dimensions. If the grib files stored those in
#'  \code{[lon, lat, time, lev]} order we could have dropped degenerate dimensions.
#' @return matrix or array
get_var_array <- function(x, var, index, collapse_degen = FALSE){
  vdims <- get_vardims(x, var)
  start <- sapply(vdims,
                  function(vname) index[[vname]][1])
  count <- sapply(vdims,
                  function(vname) index[[vname]][2])
  ncdf4::ncvar_get(x, varid = var,
                   start = start,
                   count = count,
                   collapse_degen = collapse_degen)
}


#' Retrieve a navigation structure for extracting slabs.
#'
#' Handy if you are doing multiple extractions - just compute the extraction
#' navigation once and resuse it (not that computing is slow.)
#'
#' @export
#' @param x ncdf4 object
#' @param bb a 4 element bounding box for subsetting ordered as
#'        \code{[xmin, xmax, ymin, ymax]}
#' @param time POSIXct vector of one or more times to retrieve. These are matched the
#'        closest known times in the object. See \code{get_time}  Default
#'        is the first recorded time in the object.
#' @param lev numeric vector of one or more levels. These are matched the
#'        closest known levels in the object. See \code{get_lev} Default
#'        is the first level time in the object.  Ignored if \code{lev} is not
#'        a dimension of the variable.
#' @return a list of extraction navigation elements
#' \itemize{
#'   \item{obb, otime, olev: original request inputs}
#'   \item{ilon, ilat, itime, ilev: index into dimensions for obb, otime and olev}
#'   \item{lons, lats, times, levs: full real world dimension values}
#'   \item{dx, dy: x and y resolution}
#'   \item{xlim, ylim: limits envelope}
#'   \item{index: list of array (slab) indices as in [start, count] form}
#'   \item{stbb: st_bbox object}
#'   \item{time_index, lev_index: full sequential indices into times and levs}
#' }
get_navigation <- function(x,
                           bb = get_bounds(x),
                           time = get_loc(x, 'time')[1:3],
                           lev = get_loc(x, "lev")[1:5]){

  # where along the dimensions do the requests fall?
  ilon <- loc_index(x, bb[1:2], "lon")
  ilat <- loc_index(x, bb[3:4], "lat")
  ilev <- loc_index(x, lev, "lev")
  itime <- loc_index(x, time, 'time')

  # what are the actual dimension values
  lons <- get_lon(x)
  lats <- get_lat(x)
  times <- get_time(x)
  levs <- get_lev(x)

  # spacing and limits
  dx <- lons[2]-lons[1]
  dy <- lats[2]-lats[1]
  xlim <- lons[ilon] + c(-dx, dx)/2
  ylim <- lats[ilat] + c(-dy, dy)/2

  # encoded slab coords as [start, count]
  index <- list(
    lon = loc_index(x, bb[1:2], "lon", make_rle = TRUE),
    lat = loc_index(x, bb[3:4], "lat", make_rle = TRUE),
    time =  loc_index(x, time, "time", make_rle = TRUE),
    lev = loc_index(x, lev, "lev", make_rle = TRUE))

  # the bbox as st object
  stbb <- sf::st_bbox(c(xmin = xlim[1],
                        ymin = ylim[1],
                        xmax = xlim[2],
                        ymax = ylim[2]),
                      crs = 4326)

  # the slab coordinates as indices into original dimensions,
  # guaranteed to be ordered, unlike itime/ilev etc which could be just start stop
  time_index <- index$time[1] + (seq_len(index$time[2]) - 1)
  lev_index <- index$lev[1] + (seq_len(index$lev[2]) - 1)

  list(obb = bb, otime = time, olev = lev,
       lons = lons,
       lats = lats,
       times = times,
       levs = levs,
       ilon = ilon,
       ilat = ilat,
       itime = itime,
       ilev = ilev,
       dx = dx,
       dy = dy,
       xlim = xlim,
       ylim = ylim,
       index = index,
       stbb = stbb,
       time_index = time_index,
       lev_index = lev_index)
}

#' Retrieve a variable as array or stars object.
#'
#' Data are stored as \code{[lon, lat, time]} or \code{[lon, lat, lev, time]}
#' Degenerate indices (dimension = 1) are discarded, so if a single time is
#' requested for a \code{[lon, lat, time]} variable then a single band object is
#' returned.
#'
#' The requested bounding box coordinates are matched to the closest grid cell
#' centers, thus the output grids may differ in extent form the requested bounding
#' box.
#'
#' Requested times and levels are considered contiguous - we are extracting slabs
#' of data after all. Currently the first and last times or levels requested mark
#' the inclusive bounds of the slab in those dimensions. Requesting a single time or
#' level works pefectly well.  If you need disjoint bands (not contiguous bands) then
#' you will need to make a separate request for each.
#'
#' @export
#' @param x ncdf4 object
#' @param var character, one or more names of the variables to retrieve
#' @param bb a 4 element bounding box for subsetting ordered as
#'        \code{[xmin, xmax, ymin, ymax]}
#' @param time POSIXct vector of one or more times to retrieve. These are matched the
#'        closest known times in the object. See \code{get_time}  Default
#'        is the first recorded time in the object. See \code{shift} argument
#' @param lev numeric vector of one or more levels. These are matched the
#'        closest known levels in the object. See \code{get_lev} Default
#'        is the first level time in the object.  Ignored if \code{lev} is not
#'        a dimension of the variable.
#' @param nav list, see \code{get_navigation} If not provided then computed from
#'        \code{bb, time and lev}.  If provided then \code{bb, time and lev} are
#'        ignored (since you have already computed the navigation.)
#' @param shift 2 number of days to shift the input time - see \code{get_time}. Set
#'        to \code{0} to add no shift
#' @param form character either 'array' of 'stars' (default)
#' \itemize{
#'   \item{array}{an array or list of arrays, possibly degenerate to a matrix}
#'   \item{stars}{a stars object, possibly with band (time) and z (level)}
#' }
get_var <- function(x,
                    var = "tmpsfc",
                    bb = get_bounds(x),
                    time = get_loc(x, 'time')[1:3],
                    lev = get_loc(x, "lev")[1:5],
                    nav = NULL,
                    shift = 2,
                    form = c("array", "stars")[2]){


  if (is.null(nav)){
    nav <- get_navigation(x, bb = bb, time = time, lev = lev)
  }

  if (length(var) > 1){
    r <- sapply(var,
                function(v){
                  get_var(x, v, nav = nav, form = form)
                }, simplify = FALSE)
    if (tolower(form[1]) == 'stars'){
      r <- Reduce(c, r) %>%
        stats::setNames(var)
    }
    return(r)
  }

  stopifnot(var[1] %in% get_varnames(x))

  m <- get_var_array(x, var[1], nav$index)

  if (tolower(form[1]) %in% c('array', "matrix")) return(m)

  d <- dim(m)

  if (length(d) == 4){
    # lon, lat, lev, time
    r <- lapply(seq_len(d[4]),
                 function(i){
                   stars::st_as_stars(nav$stbb,
                            nx = d[1],
                            ny = d[2],
                            nz = d[3],
                            values = m[,,,i]) %>%
                   stars::st_flip(which = 2) %>%
                   stars::st_set_dimensions(which = 'z',
                                            values = nav$levs[nav$lev_index])
                 }) %>%
      bind_stars(nms= format(nav$times[nav$itime], "%Y%m%dT%H%M%S")) %>%
      merge(name = 'time')  %>%
      stars::st_set_dimensions(which = 'time',
                               values = nav$times[nav$time_index])

  } else if (length(d) == 3) {
    # lon, lat, time
    r <- stars::st_as_stars(nav$stbb,
                       nx = d[1],
                       ny = d[2],
                       nz = d[3],
                       values = m) %>%
      stars::st_flip(which = 2)  %>%
      stars::st_set_dimensions(which = 3,
                               values = nav$times[nav$time_index],
                               names = 'time')
  } else {
    # lon lat - we restore time so to speak
    r <- stars::st_as_stars(nav$stbb,
                            nx = d[1],
                            ny = d[2],
                            nz = 1,
                            values = m) %>%
      stars::st_flip(which = 2) %>%
      stars::st_set_dimensions(which = 3,
                               values = nav$times[nav$time_index],
                               names = 'time')
  }
  r <- stats::setNames(r, var)
  r
}

