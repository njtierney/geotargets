#' Create a stars _stars_ Target
#'
#' Provides a target format for stars objects.
#'
#' @param driver character. File format expressed as GDAL driver names passed to [stars::write_stars()]. See [sf::st_drivers()].
#' @param options character. GDAL driver specific datasource creation options passed to [stars::write_stars()]
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#' @importFrom rlang %||% arg_match0 is_installed
#' @seealso [targets::tar_target_raw()]
#' @export
#' @examplesIf rlang::is_installed("stars")
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'  targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'    library(geotargets)
#'    targets::tar_script({
#'      list(
#'        geotargets::tar_stars(
#'          stars_example,
#'          stars::read_stars(system.file("tif", "olinda_dem_utm25s.tif", package = "stars"))
#'        )
#'      )
#'    })
#'    targets::tar_make()
#'    x <- targets::tar_read(stars_example)
#'  })
#'}
tar_stars <- function(name,
                      command,
                      pattern = NULL,
                      driver = NULL,
                      options = NULL,
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

    # if not specified by user, pull the corresponding geotargets option
    driver <- driver %||% geotargets_option_get("gdal.raster.driver")
    options <- options %||% geotargets_option_get("gdal.raster.creation_options")

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = create_format_stars(driver = driver, options = options, ...),
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
#' @param driver character. File format expressed as GDAL driver names passed to [stars::write_stars()]. See [sf::st_drivers()].
#' @param options character. GDAL driver specific datasource creation options passed to [stars::write_stars()]
#' @param ... Additional arguments not yet used
#' @noRd
create_format_stars <- function(driver, options, ...) {

    if (!requireNamespace("sf")) {
        stop("package 'sf' is required", call. = FALSE)
    }

    if (!requireNamespace("stars")) {
        stop("package 'stars' is required", call. = FALSE)
    }

    # get list of drivers available for writing depending on what the user's GDAL supports
    drv <- sf::st_drivers(what = "raster")
    drv <- drv[drv$write, ]

    driver <- driver %||% geotargets_option_get("gdal.raster.driver")
    driver <- rlang::arg_match0(driver, drv$name)

    options <- options %||% geotargets_option_get("gdal.raster.creation_options")

    # NOTE: Option getting functions are set in the .write_terra_raster function template
    #       to resolve issue with body<- not working in some evaluation contexts ({covr}).
    # TODO: It should be fine to have driver and options as NULL
    .write_stars <- function(object, path) {
        stars::write_stars(
            object,
            path,
            driver = geotargets::geotargets_option_get("gdal.raster.driver"),
            overwrite = TRUE,
            options = geotargets::geotargets_option_get("gdal.raster.creation_options")
        )
    }

    body(.write_stars)[[2]][["driver"]] <- driver
    body(.write_stars)[[2]][["options"]] <- options

    targets::tar_format(
        read = function(path) stars::read_stars(path),
        write = .write_stars,
        marshal = function(object) object, # Not currently used
        unmarshal = function(object) object
    )
}

