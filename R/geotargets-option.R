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
#'  - `"geotargets.gdal.raster.creation_options"` - set the GDAL creation options used when writing raster files to target store (default: `"ENCODING=UTF-8"`)
#'
#'  - `"geotargets.gdal.raster.driver"` - set the file type used for raster data in target store (default: `"GTiff"`)
#'
#'  Each option can be overridden with a system environment variable. Options include:
#'
#'   - `GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS`
#'   - `GEOTARGETS_GDAL_RASTER_DRIVER`
#'
#' @rdname geotargets-options
#' @export
geotargets_option_get <- function(option_name) {
    if (!startsWith(option_name, "geotargets.")) {
        option_name <- paste0("geotargets.", option_name)
    }

    option_value <- geotargets_env()[[option_name]]

    get_option <- function(option_name, option_value, name){
        getOption(option_name, default = option_value %||% name)
    }

    get_geotargets_gdal_raster_creation_options <- function(option_name, option_value) {
        gdal_creation_options <- Sys.getenv(
            x = "GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS",
            unset = get_option(option_name, option_value, "ENCODING=UTF-8")
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

    switch(option_name,
           "geotargets.gdal.raster.creation_options" =
               get_geotargets_gdal_raster_creation_options(option_name, option_value),
           "geotargets.gdal.raster.driver" =
               get_geotargets_gdal_raster_driver(option_name, option_value)
    )
}

#' @param option_value Value to assign to option `x`.
#' @rdname geotargets-options
#' @export
geotargets_option_set <- function(option_name, option_value) {
    if (!startsWith(option_name, "geotargets.")) {
        option_name <- paste0("geotargets.", option_name)
    }
    geotargets.env[[option_name]] <- option_value
}
