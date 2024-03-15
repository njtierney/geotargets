#' Create a terra _SpatRaster_ target
#'
#' Provides a target format for [terra::SpatRaster-class] objects.
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#' @importFrom rlang %||% arg_match0
#' @seealso [targets::tar_target_raw()]
#' @export
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'  targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'    library(geotargets)
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
tar_terra_rast <- function(name,
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
    filetype <- filetype %||% geotargets_option_get("gdal.raster.driver")
    gdal <- gdal %||% geotargets_option_get("gdal.raster.creation_options")

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = create_format_terra_raster(filetype = filetype, gdal = gdal, ...),
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
create_format_terra_raster <- function(filetype, gdal, ...) {

    if (!requireNamespace("terra")) {
        stop("package 'terra' is required", call. = FALSE)
    }

    # get list of drivers available for writing depending on what the user's GDAL supports
    drv <- terra::gdal(drivers = TRUE)
    drv <- drv[drv$type == "raster" & grepl("write", drv$can), ]

    filetype <- filetype %||% geotargets_option_get("gdal.raster.driver")
    filetype <- rlang::arg_match0(filetype, drv$name)

    gdal <- gdal %||% geotargets_option_get("gdal.raster.creation_options")

    # NOTE: Option getting functions are set in the .write_terra_raster function template
    #       to resolve issue with body<- not working in some evaluation contexts ({covr}).
    # TODO: It should be fine to have filetype and gdal as NULL
    .write_terra_raster <- function(object, path) {
        terra::writeRaster(
            object,
            path,
            filetype = geotargets::geotargets_option_get("gdal.raster.driver"),
            overwrite = TRUE,
            gdal = geotargets::geotargets_option_get("gdal.raster.creation_options")
        )
    }

    body(.write_terra_raster)[[2]][["filetype"]] <- filetype
    body(.write_terra_raster)[[2]][["gdal"]] <- gdal

    targets::tar_format(
        read = function(path) terra::rast(path),
        write = .write_terra_raster,
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}

