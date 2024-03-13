# format_terra_vect_shapefile <- targets::tar_format(
#   read = function(path) {
#     terra::vect(
#       paste0(
#         "/vsizip/",
#         file.path(
#           path,
#           replace_dot_zip_with_shp(path)
#         )
#       )
#     )
#   },
#   write = function(object, path) {
#     terra::writeVector(
#       x = object,
#       filename = replace_dot_zip_with_shp(path),
#       filetype = "ESRI Shapefile",
#       overwrite = TRUE
#     )
#     zf <- list.files(
#       pattern = replace_dot_zip(
#         x = path,
#         replacement = ""
#       )
#     )
#     utils::zip(
#       zipfile = path,
#       files = zf
#     )
#     unlink(zf)
#   },
#   marshal = function(object) terra::wrap(object),
#   unmarshal = function(object) terra::unwrap(object)
# )

#' Targets format for terra vectors
#'
#' Provides targets format for `terra::vect` objects
#'
#' @param filetype character. File format expressed as GDAL driver names passed to `terra::writeVector()`
#' @param gdal character. GDAL driver specific datasource creation options passed to `terra::writeVector()`
#' @param ... Additional arguments not yet used
#' @inheritParams targets::tar_target
#'
#' @export
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'   targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'     targets::tar_script({
#'       lux_area <- function(projection = "EPSG:4326") {
#'         terra::project(
#'           terra::vect(system.file("ex", "lux.shp",
#'             package = "terra"
#'           )),
#'           projection
#'         )
#'       }
#'       list(
#'         geotargets::tar_terra_vect(
#'           terra_vect_example,
#'           lux_area()
#'         )
#'       )
#'     })
#'     targets::tar_make()
#'     x <- targets::tar_read(terra_vect_example)
#'   })
#' }
tar_terra_vect <- function(name,
                          command,
                          pattern = NULL,
                          filetype = NULL,
                          gdal = NULL,
                          ...,
                          packages = targets::tar_option_get("packages"),
                          tidy_eval = targets::tar_option_get("tidy_eval"),
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

  # TODO: pull defaults from geotargets package options
  if (is.null(filetype)) {
      filetype <- "GeoJSON"
  }

  if (is.null(gdal)) {
      gdal <- "ENCODING=UTF-8"
  }

  if(filetype == "ESRI Shapefile") {
      #special handling of ESRI shapefiles because the output is a dir of multiple files.
      format <- targets::tar_format(
          read = function(path) terra::vect(paste0("/vsizip/{", path, "}")),
          write = function(object, path) {
              terra::writeVector(
                  x = object,
                  filename = paste0(path, ".shz"),
                  filetype = "ESRI Shapefile"
              )
              file.rename(paste0(path, ".shz"), path)
          },
          marshal = function(object) terra::wrap(object),
          unmarshal = function(object) terra::unwrap(object)
      )
  } else {
      format <- create_format_terra_vect(filetype, gdal, ...)
  }

  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = packages,
    library = library,
    format = format_terra_shapefile_zip,
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


#' @param filetype File format expressed as GDAL driver names passed to `terra::writeVector()`
#' @param gdal GDAL driver specific datasource creation options passed to `terra::writeVector()`
#' @param ... Additional arguments not yet used
#' @noRd
create_format_terra_vect <- function(filetype, gdal, ...) {

    if (!requireNamespace("terra")) {
        stop("package 'terra' is required", call. = FALSE)
    }

    # get list of drivers available for writing depending on what the user's GDAL supports
    drv <- terra::gdal(drivers = TRUE)
    drv <- drv[drv$type == "vector" & grepl("write", drv$can), ]

    if (is.null(filetype)) {
        filetype <- "GeoJSON"
    }

    filetype <- match.arg(filetype, drv$name)

    .write_terra_vector <- function(object, path) {
        terra::writeVector(
            object,
            path,
            filetype = NULL,
            overwrite = TRUE,
            gdal = NULL
        )
    }
    body(.write_terra_vector)[[2]][["filetype"]] <- filetype
    body(.write_terra_vector)[[2]][["gdal"]] <- gdal

    targets::tar_format(
        read = function(path) terra::rast(path),
        write = .write_terra_vector,
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}
