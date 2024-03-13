#' Get or Set geotargets Options
#'
#' Get or set behavior for geospatial data target stores using geotargets-specific global options.
#'
#' @param x Character. Option name. See Details.
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
geotargets_option_get <- function(x) {
    if (!startsWith(x, "geotargets.")) {
        x <- paste0("geotargets.", x)
    }

    value <- geotargets_env()[[x]]

    switch(x,
           "geotargets.raster.gdal_creation_options" = {
               strsplit(Sys.getenv("GEOTARGETS_RASTER_GDAL_CREATION_OPTIONS",
                                   unset = getOption(x, default = ifelse(is.null(value), "ENCODING=UTF-8", value))),
                        ";")[[1]]
           },
           "geotargets.raster.gdal_driver_name" = {
               Sys.getenv("GEOTARGETS_RASTER_GDAL_DRIVER_NAME",
                          unset = getOption(x, default = ifelse(is.null(value), "GTiff", value))
               )
           })
}

#' @param value Value to assign to option `x`.
#' @rdname geotargets-options
#' @export
geotargets_option_set <- function(x, value) {
    if (!startsWith(x, "geotargets.")) {
        x <- paste0("geotargets.", x)
    }
    geotargets.env[[x]] <- value
}
