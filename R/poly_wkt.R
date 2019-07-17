#' Make a wkt string from polygon that rgbif will accept
#'
#' gbif takes wkt strings for polygons that are wound anti-clockwise,
#' while sf polygons are wound clockwise. This function takes an sf polygon
#' object and converts it to a wkt string with reversed winding order.
#'
#' @param data a polygon `sf` or `sfc` object
#' @param names if `data` is an `sf` object, an optional name of a column
#' in `data` to be used to name the output vecotr of wkt strings
#'
#' @return a single polygon wkt string with winding order reversed
#' @export
#'
#' @importFrom sf st_geometry st_cast st_as_text
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#' poly_wkt(nc)
#'
poly_wkt <- function(data, names = NULL) {
  check_sf(data, names)
  data <- st_cast(data, "POLYGON")

  # If user supplied a names column, extract it
  if (!is.null(names)) {
    # to do - unduplicate names - names will be duplicated if data contained
    # multipolygons as they are split into multiple polygons by st_cast
    nms <- data[[names]]
  }

  data <- st_geometry(data)

  # create wkt for single polygon
  polywkt <- st_as_text(data)
  polywkt <- wicket::wkt_reverse(polywkt)

  if (!is.null(names)) {
    names(polywkt) <- nms
  }
  polywkt
}

#' Check data to see if it is an sf/c object, convert to sfc if sf
#'
#' @param data
#' @param names
#'
#' @return sfc object
#' @noRd
check_sf <- function(data, names) {
  if (!inherits(data, c("sf", "sfc"))) {
    stop("data must be either an sf or an sfc object", call. = FALSE)
  }

  if (!is.null(names)) {
    if (!inherits(data, "sf")) {
      stop("'names' is only useful with sf objects", call. = FALSE)
    }

    if(!is.character(names) || length (names !=1)) {
      stop("'names' must be character vector of length one", call. = FALSE)
    }

    if (!names %in% names(data)) {
      stop(names, " is not a column in 'data'", call. = FALSE)
    }
  }
}
