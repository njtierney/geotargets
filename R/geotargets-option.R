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
#' @param cache_dir character. Path to directory where file sources for
#'  `PackedSpatRaster` objects can be stored. Default: `"geotargets_cache"`
#'   when `geotargets::geotargets_option_get("cache.dir")`
#'   is not set.
#' @details
#' These options can also be set using `options()`.  For example,
#' `geotargets_options_set(gdal_raster_driver = "GTiff")` is equivalent to
#' `options("geotargets.gdal.raster.driver" = "GTiff")`.
#'
#' @return Specific options, such as "gdal.raster.driver". See "Details" for
#'   more information.
#'
#' @rdname geotargets-options
#' @export
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'  targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'    library(geotargets)
#'   op <- getOption("geotargets.gdal.raster.driver")
#'   withr::defer(options("geotargets.gdal.raster.driver" = op))
#'   geotargets_option_set(gdal_raster_driver = "COG")
#'    targets::tar_script({
#'      list(
#'        geotargets::tar_terra_rast(
#'          terra_rast_example,
#'          system.file("ex/elev.tif", package = "terra") |> terra::rast()
#'        )
#'      )
#'    })
#'    targets::tar_make()
#'    x <- targets::tar_read(terra_rast_example)
#'  })
#'}
#'
geotargets_option_set <- function(
        gdal_raster_driver = NULL,
        gdal_raster_creation_options = NULL,
        gdal_vector_driver = NULL,
        gdal_vector_creation_options = NULL,
        cache_dir = NULL
) {
    # TODO do this programmatically with formals() or something?  `options()` also accepts a named list
    options(
        "geotargets.gdal.raster.driver" = gdal_raster_driver %||%
            geotargets_option_get("gdal.raster.driver"),
        "geotargets.gdal.raster.creation.options" = gdal_raster_creation_options %||%
            geotargets_option_get("gdal.raster.creation.options"),
        "geotargets.gdal.vector.driver" = gdal_vector_driver %||%
            geotargets_option_get("gdal.vector.driver"),
        "geotargets.gdal.vector.creation.options" = gdal_vector_creation_options %||%
            geotargets_option_get("gdal.vector.creation.options"),
        "geotargets.cache.dir" = cache_dir %||% 
            geotargets_option_get("cache.dir")
    )

}

#' @param name character; option name to get.
#'
#' @rdname geotargets-options
#' @examples
#' geotargets_option_get("gdal.raster.driver")
#' geotargets_option_get("gdal.raster.creation.options")
#' @export
geotargets_option_get <- function(name) {
    option_name <- geotargets_repair_option_name(name)
    # check if `name` is one of the possible options
    option_name <-
        rlang::arg_match0(option_name, c(
            "geotargets.gdal.raster.driver",
            "geotargets.gdal.raster.creation.options",
            "geotargets.gdal.vector.driver",
            "geotargets.gdal.vector.creation.options",
            "geotargets.cache.dir"
        ))

    env_name <- gsub("\\.", "_", toupper(option_name))
    opt <- getOption(option_name, default = Sys.getenv(env_name))

    #replace empty string from Sys.getenv default with NULL
    if (length(opt) == 1 && opt == "") {
        opt <- NULL
    }
    #return
    opt
}
