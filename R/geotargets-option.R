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
#'  - `"geotargets.raster.gdal_creation_options"` - set the GDAL creation options used when writing raster files to target store (default: `"ENCODING=UTF-8"`)
#'
#'  - `"geotargets.raster.gdal_driver_name"` - set the file type used for raster data in target store (default: `"GTiff"`)
#'
#'  Each option can be overridden with a system environment variable. Options include:
#'
#'   - `GEOTARGETS_RASTER_GDAL_CREATION_OPTIONS`
#'   - `GEOTARGETS_RASTER_GDAL_DRIVER_NAME`
#'
#' @rdname geotargets-options
#' @export
geotargets_option_get <- function(option_name) {
    if (!startsWith(option_name, "geotargets.")) {
        option_name <- paste0("geotargets.", option_name)
    }

    option_value <- geotargets_env()[[option_name]]

    switch(option_name,
           "geotargets.raster.gdal_creation_options" = {
               strsplit(Sys.getenv("GEOTARGETS_RASTER_GDAL_CREATION_OPTIONS",
                                   unset = getOption(option_name, default = ifelse(is.null(option_value), "ENCODING=UTF-8", option_value))),
                        ";")[[1]]
           },
           "geotargets.raster.gdal_driver_name" = {
               Sys.getenv("GEOTARGETS_RASTER_GDAL_DRIVER_NAME",
                          unset = getOption(option_name, default = ifelse(is.null(option_value), "GTiff", option_value))
               )
           })
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
