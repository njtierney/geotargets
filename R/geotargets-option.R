#' Get or Set geotargets Options
#'
#' Get or set behavior for geospatial data target stores using
#' geotargets-specific global options.
#'
#' @param gdal_raster_driver character, length 1; set the driver used for raster
#'   data in target store (default: `"GTiff"`). Options for driver names can be
#'   found here: <https://gdal.org/drivers/raster/index.html>.
#' @param gdal_raster_creation_options character; set the GDAL creation options
#'   used when writing raster files to target store (default: `""`). You may
#'   specify multiple values e.g. `c("COMPRESS=DEFLATE", "TFW=YES")`. Each GDAL
#'   driver supports a unique set of creation options. For example, with the
#'   default `"GTiff"` driver:
#'   <https://gdal.org/drivers/raster/gtiff.html#creation-options>.
#' @param gdal_vector_driver character, length 1; set the file type used for
#' vector data in target store (default: `"GeoJSON"`).
#' @param gdal_vector_creation_options character; set the GDAL layer creation
#'   options used when writing vector files to target store (default:
#'   `"ENCODING=UTF-8"`). You may specify multiple values e.g.
#'   `c("WRITE_BBOX=YES", "COORDINATE_PRECISION=10")`. Each GDAL driver supports
#'   a unique set of creation options. For example, with the default `"GeoJSON"`
#'   driver:
#'   <https://gdal.org/drivers/vector/geojson.html#layer-creation-options>
#' @param terra_preserve_metadata character. When `"drop"` (default), any
#'   auxiliary files that would be written by [terra::writeRaster()] containing
#'   raster metadata such as units and datetimes are lost (note that this does
#'   not include layer names set with `names() <-`).  When `"zip"`, these
#'   metadata are retained by archiving all written files as a zip file upon
#'   writing and unzipping them upon reading. This adds extra overhead and will
#'   slow pipelines. Also note metadata may be impacted by different versions
#'   of GDAL and different drivers. Note that you can specify this option for
#'   individual targets, e.g., inside [tar_terra_rast()] there is the option,
#'   `preserve_metadata`.
#'
#' @details
#' # Potential issues retaining metdatadata
#'
#'   If you have an issue with retaining metadata (such as units, time, etc),
#'   this could be due to the versions of GDAL and terra on your machine. We
#'   recommend exploring if this issue persists outside of geotargets. That is,
#'   try saving the file out and reading it back in using regular R code. If you
#'   find that this is an issue with geotargets, please file an issues at
#'   \url{https://github.com/njtierney/geotargets/issues/} and we will try and
#'   get this working for you.
#'
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
#' # For CRAN. Ensures these examples run under certain conditions.
#' # To run this locally, run the code inside this if statement
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' # tar_dir() runs code from a temporary directory.
#'   targets::tar_dir({
#'     library(geotargets)
#'     op <- getOption("geotargets.gdal.raster.driver")
#'     withr::defer(options("geotargets.gdal.raster.driver" = op))
#'     geotargets_option_set(
#'       gdal_raster_driver = "COG",
#'       terra_preserve_metadata = "zip"
#'     )
#'     targets::tar_script({
#'       list(
#'         geotargets::tar_terra_rast(
#'           terra_rast_example,
#'           {
#'             new_rast <- system.file("ex/elev.tif", package = "terra") |>
#'               terra::rast()
#'             terra::units(new_rast) <- "m"
#'             new_rast
#'           }
#'         )
#'       )
#'     })
#'     targets::tar_make()
#'     x <- targets::tar_read(terra_rast_example)
#'     x
#'     terra::units(x)
#'   })
#' }
#'
geotargets_option_set <- function(gdal_raster_driver = NULL,
                                  gdal_raster_creation_options = NULL,
                                  gdal_vector_driver = NULL,
                                  gdal_vector_creation_options = NULL,
                                  terra_preserve_metadata = NULL) {
    # TODO do this programmatically with formals() or something?
    # `options()` also accepts a named list
  options(
    "geotargets.gdal.raster.driver" = gdal_raster_driver %||% # nolint
      geotargets_option_get("gdal.raster.driver"),
    "geotargets.gdal.raster.creation.options" = gdal_raster_creation_options %||% # nolint
      geotargets_option_get("gdal.raster.creation.options"),
    "geotargets.gdal.vector.driver" = gdal_vector_driver %||%
      geotargets_option_get("gdal.vector.driver"),
    "geotargets.gdal.vector.creation.options" = gdal_vector_creation_options %||% #nolint
      geotargets_option_get("gdal.vector.creation.options"),
    "geotargets.terra.preserve.metadata" = terra_preserve_metadata %||%
      geotargets_option_get("terra.preserve.metadata")
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
      "geotargets.terra.preserve.metadata"
    ))

  env_name <- gsub("\\.", "_", toupper(option_name))
  opt <- getOption(option_name, default = Sys.getenv(env_name))

  # replace empty string from Sys.getenv default with NULL
  if (length(opt) == 1 && opt == "") {
    opt <- NULL
  }
  # return
  opt
}
