#' Create a terra _SpatRaster_ target
#'
#' Provides a target format for [terra::SpatRaster-class] objects.
#'
#' @details
#' The terra package uses objects like [terra::SpatRaster-class],
#' [terra::SpatVector-class], and [terra::SpatRasterDataset] (SDS), which do
#' not contain the data directly--they contain a C++ pointer to memory where
#' the data is stored.  As a result, these objects are not portable between
#' R sessions without special handling, which causes problems when including
#' them in `targets` pipelines with [targets::tar_target()]. The functions,
#' [tar_terra_rast()], [tar_terra_sds()], [tar_terra_sprc()],
#' [tar_terra_tiles()], and [tar_terra_vect()] handle this issue by writing and
#' reading the target as a geospatial file (specified by `filetype`) rather
#' than saving the relevant object (e.g., `SpatRaster`, `SpatVector`, etc.),
#' itself.
#'
#' @param name Symbol, name of the target. A target name must be a valid name
#'   for a symbol in R, and it must not start with a dot. See
#'   [targets::tar_target()] for more information.
#' @param command R code to run the target.
#' @param pattern Code to define a dynamic branching pattern for a target. See
#'   [targets::tar_target()] for more information.
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param preserve_metadata character. When `"drop"` (default), any
#'   auxiliary files that would be written by [terra::writeRaster()] containing
#'   raster metadata such as units and datetimes are lost (note that this does
#'   not include layer names set with `names() <-`).  When `"zip"`, these
#'   metadata are retained by archiving all written files as a zip file upon
#'   writing and unzipping them upon reading. This adds extra overhead and will
#'   slow pipelines. Also note metadata may be impacted by different versions
#'   of GDAL and different drivers. If you have an issue with retaining
#'   metadata for your setup, please file an issue at
#'   \url{https://github.com/njtierney/geotargets/issues/} and we will try and
#'   get this working for you. Also note that you can specify this option
#'   inside [geotargets_option_set()] if you want to set this for the entire
#'   pipeline.
#'
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#'
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'   `"list"`, the only option that works currently.
#'
#' @returns target class "tar_stem" for use in a target pipeline
#' @importFrom rlang %||% arg_match0
#' @seealso [targets::tar_target()]
#' @export
#' @examples
#' # For CRAN. Ensures these examples run under certain conditions.
#' # To run this locally, run the code inside this if statement
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'   targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'     library(geotargets)
#'     targets::tar_script({
#'       list(
#'         geotargets::tar_terra_rast(
#'           terra_rast_example,
#'           system.file("ex/elev.tif", package = "terra") |> terra::rast()
#'         )
#'       )
#'     })
#'     targets::tar_make()
#'     x <- targets::tar_read(terra_rast_example)
#'   })
#' }
tar_terra_rast <- function(
        name,
        command,
        pattern = NULL,
        filetype = geotargets_option_get("gdal.raster.driver"),
        gdal = geotargets_option_get("gdal.raster.creation.options"),
        preserve_metadata = geotargets_option_get("terra.preserve.metadata"),
        ...,
        tidy_eval = targets::tar_option_get("tidy_eval"),
        packages = targets::tar_option_get("packages"),
        library = targets::tar_option_get("library"),
        repository = targets::tar_option_get("repository"),
        error = targets::tar_option_get("error"),
        memory = targets::tar_option_get("memory"),
        garbage_collection = targets::tar_option_get("garbage_collection"),
        deployment = targets::tar_option_get("deployment"),
        priority = targets::tar_option_get("priority"),
        resources = targets::tar_option_get("resources"),
        storage = targets::tar_option_get("storage"),
        retrieval = targets::tar_option_get("retrieval"),
        cue = targets::tar_option_get("cue"),
        description = targets::tar_option_get("description")
) {
  filetype <- filetype %||% "GTiff"

  # check that filetype option is available
  drv <- get_gdal_available_driver_list("raster")
  filetype <- rlang::arg_match0(filetype, drv$name)

  # currently only "drop" and "zip" are valid options
  preserve_metadata <- preserve_metadata %||% "drop"
  preserve_metadata <- rlang::arg_match0(preserve_metadata, c("drop", "zip"))

  # ensure that user-passed `resources` doesn't include `custom_format`
  check_user_resources(resources)

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

  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = packages,
    library = library,
    format = targets::tar_format(
      read = tar_rast_read(preserve_metadata = preserve_metadata),
      write = tar_rast_write(
          filetype = filetype,
          gdal = gdal,
          preserve_metadata = preserve_metadata
          ),
      marshal = function(object) terra::wrap(object),
      unmarshal = function(object) terra::unwrap(object),
      substitute = list(
          filetype = filetype,
          gdal = gdal,
          preserve_metadata = preserve_metadata
          )
    ),
    repository = repository,
    iteration = "list", # only "list" works right now
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}

tar_rast_read <- function(preserve_metadata) {
  switch(preserve_metadata,
    zip = function(path) {
      tmp <- tempdir()
      # NOTE: cannot use withr::local_tempdir() because the unzipped files need
      # to persist so that the resulting `SpatRaster` object doesn't have a
      # broken file pointer
      zip::unzip(zipfile = path, exdir = tmp)
      terra::rast(file.path(tmp, basename(path)))
    },
    drop = function(path) terra::rast(path)
  )
}

tar_rast_write <- function(filetype, gdal, preserve_metadata) {
  switch(preserve_metadata,
    zip = function(object, path) {
      # write the raster in a fresh local tempdir() that disappears when
      # function is done
      tmp <- withr::local_tempdir()
      raster_tmp_file <- file.path(tmp, basename(path))
      zip_tmp_file <- file.path(tmp, "object.zip")
      terra::writeRaster(
        object,
        filename = raster_tmp_file,
        filetype = filetype,
        overwrite = TRUE,
        gdal = gdal
      )
      # package files into a zip file using `zip::zip()`
      raster_files <- list.files(path = tmp, full.names = TRUE)
      zip::zip(
        zipfile = zip_tmp_file,
        files = raster_files,
        compression_level = 1,
        mode = "cherry-pick",
        root = tmp
      )
      # move the zip file to the expected place
      file.copy(zip_tmp_file, path)
    },
    drop = function(object, path) {
      terra::writeRaster(
        object,
        filename = path,
        filetype = filetype,
        overwrite = TRUE,
        gdal = gdal
      )
    }
  )
}
