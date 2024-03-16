#' Get or Set geotargets Options
#'
#' Get or set behavior for geospatial data target stores using geotargets-specific global options.
#'
#' @param option_name Character. Option name. See Details.
#'
#' @details
#'
#' ## Available Options
#'
#'  - `"geotargets.gdal.raster.driver"` - character. Length 1. Set the driver used for raster data in target store (default: `"GTiff"`). Options for driver names can be found here: <https://gdal.org/drivers/raster/index.html>
#'
#'  - `"geotargets.gdal.raster.creation_options"` - character. Set the GDAL creation options used when writing raster files to target store (default: `""`). You may specify multiple values e.g. `c("COMPRESS=DEFLATE", "TFW=YES")`. Each GDAL driver supports a unique set of creation options. For example, with the default `"GTiff"` driver: <https://gdal.org/drivers/raster/gtiff.html#creation-options>
#'
#'  - `"geotargets.gdal.vector.driver"` - character. Length 1. Set the file type used for vector data in target store (default: `"GeoJSON"`).
#'
#'  - `"geotargets.gdal.vector.creation_options"` - character. Set the GDAL layer creation options used when writing vector files to target store (default: `"ENCODING=UTF-8"`). You may specify multiple values e.g. `c("WRITE_BBOX=YES", "COORDINATE_PRECISION=10")`. Each GDAL driver supports a unique set of creation options. For example, with the default `"GeoJSON"` driver: <https://gdal.org/drivers/vector/geojson.html#layer-creation-options>
#'
#' - `"geotargets.stars.proxy"` - logical. Control the object type read from a _stars_ target via `tar_stars()` with [stars::read_stars()]. Value `FALSE` (default) returns `stars` object; if `TRUE` a `stars_proxy` is returned.

#'  Each option can be overridden with a system environment variable. Options include:
#'
#'   - `GEOTARGETS_GDAL_RASTER_DRIVER`
#'   - `GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS`
#'   - `GEOTARGETS_GDAL_VECTOR_DRIVER`
#'   - `GEOTARGETS_GDAL_VECTOR_CREATION_OPTIONS`
#'   - `GEOTARGETS_STARS_PROXY`
#'
#'  When specifying options that support multiple values using a system environment variable, the separate options should be delimited with a semicolon (";"). For example: `"COMPRESS=DEFLATE;TFW=YES"`.
#'
#' @rdname geotargets-options
#' @export
geotargets_option_get <- function(option_name) {

    option_name <- geotargets_repair_option_name(option_name)
    option_value <- geotargets_env()[[option_name]]

    if (length(option_value) == 0) {
        option_value <- NULL
    }

    get_option <- function(option_name, option_value, name) {
        getOption(option_name, default = option_value %||% name)
    }

    get_geotargets_gdal_raster_creation_options <- function(option_name, option_value) {
        gdal_creation_options <- Sys.getenv(
            x = "GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS",
            unset = get_option(option_name, option_value, "")
        )
        the_option <- strsplit(gdal_creation_options, ";")[[1]]
        the_option
    }

    get_geotargets_gdal_raster_driver <- function(option_name, option_value) {
        Sys.getenv(
            x = "GEOTARGETS_GDAL_RASTER_DRIVER",
            unset = get_option(option_name, option_value, "GTiff")
        )
    }

    get_geotargets_gdal_vector_creation_options <- function(option_name, option_value) {
        gdal_creation_options <- Sys.getenv(
            x = "GEOTARGETS_GDAL_VECTOR_CREATION_OPTIONS",
            unset = get_option(option_name, option_value, "ENCODING=UTF-8")
        )
        the_options <- strsplit(gdal_creation_options, ";")[[1]]
        the_options
    }

    get_geotargets_gdal_vector_driver <- function(option_name, option_value) {
        Sys.getenv(
            x = "GEOTARGETS_GDAL_VECTOR_DRIVER",
            unset = get_option(option_name, option_value, "GeoJSON")
        )
    }

    get_geotargets_stars_proxy <- function(option_name, option_value) {
        as.logical(Sys.getenv(
            x = "GEOTARGETS_STARS_PROXY",
            unset = get_option(option_name, option_value, FALSE)
        ))
    }

    switch(option_name,
           "geotargets.gdal.raster.creation_options" =
               get_geotargets_gdal_raster_creation_options(option_name, option_value),
           "geotargets.gdal.raster.driver" =
               get_geotargets_gdal_raster_driver(option_name, option_value),
           "geotargets.gdal.vector.creation_options" =
               get_geotargets_gdal_vector_creation_options(option_name, option_value),
           "geotargets.gdal.vector.driver" =
               get_geotargets_gdal_vector_driver(option_name, option_value),
           "geotargets.stars.proxy" =
               get_geotargets_stars_proxy(option_name, option_value)
    )
}

#' @param option_value Value to assign to option `x`.
#' @rdname geotargets-options
#' @export
geotargets_option_set <- function(option_name, option_value) {
    option_name <- geotargets_repair_option_name(option_name)
    geotargets.env[[option_name]] <- option_value
}
