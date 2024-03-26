#' Get or Set geotargets Options
#'
#' Get or set behavior for geospatial data target stores using
#' geotargets-specific global options.
#'
#' @param gdal_raster_driver character, length 1; set the driver used for raster
#'   data in target store (default: `"GTiff"`). Options for driver names can be
#'   found here: <https://gdal.org/drivers/raster/index.html>
#' @param gdal_raster_creation_options character; set the GDAL creation options
#'   used when writing raster files to target store (default: `""`). You may
#'   specify multiple values e.g. `c("COMPRESS=DEFLATE", "TFW=YES")`. Each GDAL
#'   driver supports a unique set of creation options. For example, with the
#'   default `"GTiff"` driver:
#'   <https://gdal.org/drivers/raster/gtiff.html#creation-options>
#' @param gdal_vector_driver character, length 1; set the file type used for
#' vector data in target store (default: `"GeoJSON"`).
#' @param gdal_vector_creation_options character; set the GDAL layer creation
#'   options used when writing vector files to target store (default:
#'   `"ENCODING=UTF-8"`). You may specify multiple values e.g.
#'   `c("WRITE_BBOX=YES", "COORDINATE_PRECISION=10")`. Each GDAL driver supports
#'   a unique set of creation options. For example, with the default `"GeoJSON"`
#'   driver:
#'   <https://gdal.org/drivers/vector/geojson.html#layer-creation-options>
#'
#' @rdname geotargets-options
#' @export
geotargets_option_set <- function(
        gdal_raster_driver = NULL,
        gdal_raster_creation_options = NULL,
        gdal_vector_driver = NULL,
        gdal_vector_creation_options = NULL
) {

    options(
        "geotargets.gdal.raster.driver" =
            gdal_raster_driver %||%
            getOption("geotargets.gdal.raster.driver",
                      default = Sys.getenv("GEOTARGETS_GDAL_RASTER_DRIVER",
                                           unset = "GTiff")),

        "geotargets.gdal.raster.creation.options" =
            gdal_raster_creation_options %||%
            getOption("geotargets.gdal.raster.creation.options",
                      default =  Sys.getenv("GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS",
                                            unset = "ENCODING=UTF-8")),

        "geotargets.gdal.vector.driver" =
            gdal_raster_creation_options %||%
            getOption("geotargets.gdal.vector.driver",
                      default =  Sys.getenv("GEOTARGETS_GDAL_VECTOR_DRIVER",
                                            unset = "GeoJSON")),

        "geotargets.gdal.vector.creation.options" =
            gdal_raster_creation_options %||%
            getOption("geotargets.gdal.vector.creation.options",
                      default =  Sys.getenv("GEOTARGETS_GDAL_VECTOR_CREATION_OPTIONS",
                                            unset = "ENCODING=UTF-8"))
    )

}

#' @param name character; option name to get
#'
#' @rdname geotargets-options
#' @export
geotargets_option_get <- function(name) {
    option_name <- geotargets_repair_option_name(name)
    getOption(option_name)
}
