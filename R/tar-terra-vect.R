#' Create a terra _SpatVector_ target
#'
#' Provides a target format for [terra::SpatVector-class] objects.
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeVector()]. See 'Note' for more details
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeVector()].
#' @param ... Additional arguments not yet used
#' @inheritParams targets::tar_target
#'
#' @note Although you may pass any supported GDAL vector driver to the
#'   `filetype` argument, not all formats are guaranteed to work with
#'   `geotargets`.  At the moment, we have tested `GeoJSON` and `ESRI Shapefile`
#'   which both appear to work generally.
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
    rlang::check_installed("terra")
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
    filetype <- filetype %||% geotargets_option_get("gdal.vector.driver")
    gdal <- gdal %||% geotargets_option_get("gdal.vector.creation_options")

    format <- ifelse(
        test = filetype == "ESRI Shapefile",
        #special handling of ESRI shapefiles because the output is a dir of multiple files.
        yes = create_format_terra_vect_shz(options = gdal, ...),
        no =  create_format_terra_vect(filetype, options = gdal, ...)
    )

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = format,
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


#' @param filetype File format expressed as GDAL driver names passed to
#'   `terra::writeVector()`
#' @param options GDAL driver specific datasource creation options passed to
#'   `terra::writeVector()`
#' @param ... Additional arguments not yet used
#' @noRd
create_format_terra_vect <- function(filetype, options, ...) {
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
            options = NULL
        )
    }
    body(.write_terra_vector)[[2]][["filetype"]] <- filetype
    body(.write_terra_vector)[[2]][["options"]] <- options

    targets::tar_format(
        read = function(path) terra::vect(path),
        write = .write_terra_vector,
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}

#' Special handling for ESRI Shapefiles
#' @param options GDAL driver specific datasource creation options passed to
#'   `terra::writeVector()`
#' @param ... Additional arguments not yet used
#' @noRd
create_format_terra_vect_shz <- function(options, ...) {
    .write_terra_vector <- function(object, path) {
        terra::writeVector(
            x = object,
            filename = paste0(path, ".shz"),
            filetype = "ESRI Shapefile",
            overwrite = TRUE,
            options = NULL
        )
        file.rename(paste0(path, ".shz"), path)
    }
    body(.write_terra_vector)[[2]][["options"]] <- options

    targets::tar_format(
        read = function(path) terra::vect(paste0("/vsizip/{", path, "}")),
        write = .write_terra_vector,
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}
