#' Create a terra _SpatRasterCollection_ target
#'
#' Provides a target format for [terra::SpatRasterCollection] objects,
#'   which have no restriction in the extent or other geometric parameters.
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#' @seealso [targets::tar_target_raw()]
#' @author Andrew Gene Brown
#' @author Nicholas Tierney
#' @export
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'   targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'     library(geotargets)
#'     targets::tar_script({
#'       elev_scale <- function(z = 1, projection = "EPSG:4326") {
#'         terra::project(
#'           terra::rast(system.file("ex", "elev.tif", package = "terra")) * z,
#'           projection
#'         )
#'       }
#'       list(
#'         tar_terra_sprc(
#'           raster_elevs,
#'           # two rasters, one unaltered, one scaled by factor of 2 and
#'           # reprojected to interrupted good homolosine
#'           command = terra::sprc(list(
#'             elev_scale(1),
#'             elev_scale(2, "+proj=igh")
#'           ))
#'         )
#'       )
#'     })
#'     targets::tar_make()
#'     x <- targets::tar_read(raster_elevs)
#'   })
#' }
tar_terra_sprc <- function(name,
                           command,
                           pattern = NULL,
                           filetype = NULL,
                           gdal = NULL,
                           ...,
                           tidy_eval = targets::tar_option_get("tidy_eval"),
                           packages = targets::tar_option_get("packages"),
                           library = targets::tar_option_get("library"),
                           repository = targets::tar_option_get("repository"),
                           iteration = targets::tar_option_get("iteration"),
                           error = targets::tar_option_get("error"),
                           memory = targets::tar_option_get("memory"),
                           garbage_collection = targets::tar_option_get("garbage_collection"),
                           deployment = targets::tar_option_get("deployment"),
                           priority = targets::tar_option_get("priority"),
                           resources = targets::tar_option_get("resources"),
                           storage = targets::tar_option_get("storage"),
                           retrieval = targets::tar_option_get("retrieval"),
                           cue = targets::tar_option_get("cue")) {
  check_pkg_installed("terra")

  name <- targets::tar_deparse_language(substitute(name))

  envir <- targets::tar_option_get("envir")

  command <- targets::tar_tidy_eval(
    expr = as.expression(substitute(command)),
    envir = envir,
    tidy_eval = tidy_eval
  )

  pattern <- targets::tar_tidy_eval(
    expr = as.expression(substitute(pattern)),
    envir = envir,
    tidy_eval = tidy_eval
  )

  drv <- get_gdal_available_driver_list("raster")

  # if not specified by user, pull the corresponding geotargets option
  filetype <- filetype %||% geotargets_option_get("gdal.raster.driver")
  filetype <- rlang::arg_match0(filetype, drv$name)

  gdal <- gdal %||% geotargets_option_get("gdal.raster.creation_options")

  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = packages,
    library = library,
    format = create_format_terra_rasters_sprc(filetype = filetype,
                                              gdal = gdal,
                                              ...),
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

#' @param filetype File format expressed as GDAL driver names passed to `terra::writeRaster()`
#' @param gdal GDAL driver specific datasource creation options passed to `terra::writeRaster()`
#' @param ... Additional arguments not yet used
#' @noRd
create_format_terra_rasters_sprc <- function(filetype, gdal, ...) {
  check_pkg_installed("terra")

  drv <- get_gdal_available_driver_list("raster")

  filetype <- filetype %||% geotargets_option_get("gdal.raster.driver")
  filetype <- rlang::arg_match0(filetype, drv$name)

  gdal <- gdal %||% geotargets_option_get("gdal.raster.creation_options")
  ## TODO
  ## Need to append the "opt" argument for GDAL options that is currently
  ## controlled with the if(i > 1) part.

  .write_terra_rasters_sprc <- eval(
    substitute(
      function(object, path) {
        for (i in seq(object)) {
          if (i > 1) {
            opt <- "APPEND_SUBDATASET=YES"
          } else {
            opt <- ""
          }
          terra::writeRaster(
            x = object[i],
            filename = path,
            filetype = filetype,
            overwrite = (i == 1),
            gdal = opt
          )
        }
      },
      list(filetype = filetype, gdal = gdal)
    )
  )

  format_sprc_geotiff <- targets::tar_format(
    read = function(path) terra::sprc(path),
    write = .write_terra_rasters_sprc,
    marshal = function(object) terra::wrap(object),
    unmarshal = function(object) terra::unwrap(object)
  )

  format_sprc_geotiff
}
