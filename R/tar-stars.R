#' Create a stars _stars_ Target
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Provides a target format for `stars` objects. Note that most or all `stars`
#' objects work with ordinary `tar_target()` and do not necessarily *need*
#' `geotargets` target factories the way `terra` objects do.  Currently
#' `tar_stars()` has the same limitations as [stars::write_stars()], so use with
#' caution.
#'
#' @param name Symbol, name of the target. A target name must be a valid name
#'  for a symbol in R, and it must not start with a dot. See
#'  [targets::tar_target()] for more information.
#' @param command R code to run the target.
#' @param pattern Code to define a dynamic branching pattern for a target. See
#'  [targets::tar_target()] for more information.
#' @param driver character. File format expressed as GDAL driver names passed to
#'  [stars::write_stars()]. See [sf::st_drivers()].
#' @param options character. GDAL driver specific datasource creation options
#'  passed to [stars::write_stars()].
#' @param proxy logical. Passed to [stars::read_stars()]. If `TRUE` the target
#'  will be read as an object of class `stars_proxy`. Otherwise, the object is
#'  class `stars`.
#' @param mdim logical. Use the [Multidimensional Raster Data
#'  Model](https://gdal.org/user/multidim_raster_data_model.html) via
#'  [stars::write_mdim()]? Default: `FALSE`. Only supported for some drivers,
#'  e.g. `"netCDF"` or `"Zarr"`.
#' @param ncdf logical. Use the NetCDF library directly to read data via
#'  [stars::read_ncdf()]? Default: `FALSE`. Only supported for
#'  `driver="netCDF"`.
#' @param ... Additional arguments not yet used.
#' @returns target class "tar_stem" for use in a target pipeline
#' @inheritParams targets::tar_target
#'
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'  `"list"`, the only option that works currently.
#'
#' @seealso [targets::tar_target()]
#' @export
#' @examplesIf rlang::is_installed("stars")
#' # For CRAN. Ensures these examples run under certain conditions.
#' # To run this locally, run the code inside this if statement
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'   targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'     library(geotargets)
#'     targets::tar_script({
#'       list(
#'         geotargets::tar_stars(
#'           stars_example,
#'           stars::read_stars(
#'           system.file("tif", "olinda_dem_utm25s.tif", package = "stars")
#'           )
#'         )
#'       )
#'     })
#'     targets::tar_make()
#'     x <- targets::tar_read(stars_example)
#'   })
#' }
tar_stars <- function(
        name,
        command,
        pattern = NULL,
        proxy = FALSE,
        mdim = FALSE,
        ncdf = FALSE,
        driver = geotargets_option_get("gdal.raster.driver"),
        options = geotargets_option_get("gdal.raster.creation.options"),
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
  check_pkg_installed("stars")
  if (ncdf) {
    check_pkg_installed("ncmeta")
  }

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

  tar_stars_raw(
    name = name,
    command = command,
    pattern = pattern,
    proxy = proxy,
    mdim = mdim,
    ncdf = ncdf,
    driver = driver,
    options = options,
    packages = packages,
    library = library,
    repository = repository,
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

#' @export
#' @rdname tar_stars
tar_stars_proxy <- function(
        name,
        command,
        pattern = NULL,
        mdim = FALSE,
        ncdf = FALSE,
        driver = geotargets_option_get("gdal.raster.driver"),
        options = geotargets_option_get("gdal.raster.creation.options"),
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
  check_pkg_installed("stars")
  if (ncdf) {
    check_pkg_installed("ncmeta")
  }

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

  tar_stars_raw(
    name = name,
    command = command,
    pattern = pattern,
    proxy = TRUE,
    mdim = mdim,
    ncdf = ncdf,
    driver = driver,
    options = options,
    ...,
    packages = packages,
    library = library,
    repository = repository,
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


#' tar_stars method with no tidy eval etc.
#' @noRd
tar_stars_raw <- function(
        name,
        command,
        pattern = NULL,
        proxy,
        mdim = FALSE,
        ncdf = FALSE,
        driver = geotargets_option_get("gdal.raster.driver"),
        options = geotargets_option_get("gdal.raster.creation.options"),
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
  driver <- driver %||% "GTiff"
  options <- options %||% character(0)

  # get drivers available for writing (depends on user's GDAL build)
  drv <- sf::st_drivers(what = "raster")
  drv <- drv[drv$write, ]

  driver <- rlang::arg_match0(driver, drv$name)

  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = packages,
    library = library,
    format = targets::tar_format(
      read = function(path) {
        if (ncdf) {
          stars::read_ncdf(path, proxy = proxy)
        } else if (isTRUE(mdim)) {
          stars::read_mdim(path, proxy = proxy)
        } else {
          stars::read_stars(path, proxy = proxy)
        }
      },
      write = function(object, path) {
        if (mdim) {
          stars::write_mdim(
            object,
            path,
            overwrite = TRUE,
            driver = driver,
            options = options
          )
        } else {
          stars::write_stars(
            object,
            path,
            overwrite = TRUE,
            driver = driver,
            options = options
          )
        }
      },
      substitute = list(
        ncdf = ncdf,
        mdim = mdim,
        proxy = proxy,
        driver = driver,
        options = options
      )
    ),
    repository = repository,
    iteration = "list", # the only option that works
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
