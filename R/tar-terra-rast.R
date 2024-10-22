#' Create a terra _SpatRaster_ target
#'
#' Provides a target format for [terra::SpatRaster-class] objects.
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param use_cache logical. When `FALSE` (default), you may find that some
#'   metadata is "stripped" from raster targets. If set to `TRUE`, the actual
#'   raster file and any "sidecar" metadata files are stored in `cach_dir` and
#'   only a `PackedSpatRaster` object is stored (as .rds) in the targets store.
#' @param cache_dir character. A relative path to a directory to use as a cache
#'   for raster files and associated "sidecar" files containing metadata.
#'   Defaults to `"_geotargets"`. Ignored if `use_cache` is `FALSE`.
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#'
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'   `"list"`, the only option that works currently.
#'
#' @returns target class "tar_stem" for use in a target pipeline
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
                           filetype = geotargets_option_get("gdal.raster.driver"),
                           gdal = geotargets_option_get("gdal.raster.creation.options"),
                           use_cache = geotargets_option_get("use.cache"),
                           cache_dir = geotargets_option_get("cache.dir"),
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
                           description = targets::tar_option_get("description")) {
    check_pkg_installed("terra")

    filetype <- filetype %||% "GTiff"

    #check that filetype option is available
    drv <- get_gdal_available_driver_list("raster")
    filetype <- rlang::arg_match0(filetype, drv$name)

    #handle optional cache strat
    use_cache <- use_cache %||% FALSE
    cache_dir <- cache_dir %||% "_geotargets"
    if (isTRUE(use_cache)) {
        geotargets_init_cache(cache_dir)
    }

    #ensure that user-passed `resources` doesn't include `custom_format`
    if ("custom_format" %in% names(resources)) {
        cli::cli_abort("{.val custom_format} cannot be supplied to targets created with {.fn tar_terra_rast}")
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

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = targets::tar_format(
            read = terra_rast_read(use_cache),
            write = terra_rast_write(filetype, gdal, use_cache, cache_dir),
            marshal = function(object) terra::wrap(object),
            unmarshal = function(object) terra::unwrap(object),
            substitute = list(filetype = filetype, gdal = gdal, use_cache = use_cache, cache_dir = cache_dir)
        ),
        repository = repository,
        iteration = "list", #only "list" works right now
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

terra_rast_read <- function(use_cache) {
    if (isTRUE(use_cache)) {
        function(path) {
            terra::unwrap(readRDS(path))
        }
    } else {
        function(path) {
            terra::rast(path)
        }
    }
}

terra_rast_write <- function(filetype, gdal, use_cache, cache_dir) {
    if (isTRUE(use_cache)) {
        function(object, path) {
            saveRDS(
                terra::wrapCache(
                    object,
                    filename = file.path(cache_dir, basename(path)),
                    filetype = filetype,
                    gdal = gdal,
                    overwrite = TRUE),
                file = path
            )
        }
    } else {
        function(object, path) {
            terra::writeRaster(
                object,
                path,
                filetype = filetype,
                gdal = gdal,
                overwrite = TRUE
            )
        }
    }
}
