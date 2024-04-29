#' Create a wrapped terra SpatRaster target
#'
#' Provides a target format for [terra::SpatRaster-class] objects backed by
#' [terra::PackedSpatRaster-class] and file-based targets in the `geotargets`
#' cache folder (`cachedir`).
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param cachedir character. Path to directory where file sources for `PackedSpatRaster` objects can be stored. Default: `"geotargets_cache"` when `geotargets::geotargets_option_get("cache.dir")` is not set.
#' @param ... Additional arguments not yet used
#' @seealso [geotargets_destroy_cache()] [geotargets_init_cache()]
#' @note Although you may pass any supported GDAL vector driver to the
#'   `filetype` argument, not all formats are guaranteed to work with
#'   `geotargets`. At the moment, we have tested `GTiff` and `GPKG` and
#'   they appear to work generally.
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
tar_terra_rast_wrap <- function(name,
                           command,
                           pattern = NULL,
                           filetype = "GTiff",
                           gdal = geotargets::geotargets_option_get("gdal.raster.creation.options"),
                           cachedir = geotargets::geotargets_option_get("cache.dir"),
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
    filetype <- filetype %||% "GTiff"
    gdal <- gdal %||% character(0)
    cachedir <- cachedir %||% "geotargets_cache"

    #check that filetype option is available
    drv <- get_gdal_available_driver_list("raster")
    filetype <- rlang::arg_match0(filetype, drv$name)

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

    .format_terra_rast_wrap_write <- eval(substitute(function(object, path) {
        # TODO: provide mapping of major file types to extensions
        extension <- switch(filetype,
                            "GTiff" = ".tif",
                            "GPKG" = ".gpkg",
                            "")
        saveRDS(terra::wrapCache(object,
                                 filename = file.path(cachedir,
                                                      basename(path),
                                                      paste0(basename(path),
                                                             extension)),
                                 filetype = filetype,
                                 gdal = gdal,
                                 overwrite = TRUE),
                file = path)
    }, list(cachedir = cachedir, filetype = filetype, gdal = gdal)))

    geotargets_init_cache(name = name)

    # rast_cache_init <- targets::tar_target_raw(
    #     paste0(name, "_cache_init"),
    #     str2expression(paste0("normalizePath(file.path(",
    #                           shQuote(cachedir), ", ",
    #                           shQuote(name), "))")),
    #     format = "file"
    # )

    rast_wrap <- targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = targets::tar_format(
            read = function(path) terra::unwrap(readRDS(path)),
            write = .format_terra_rast_wrap_write,
            marshal = function(object) terra::wrap(object),
            unmarshal = function(object) terra::unwrap(object)
        ),
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

    rast_cache_files <- targets::tar_target_raw(
        paste0(name, "_cache_files"),
        str2expression(paste0("
            list.files(
                file.path(", shQuote(cachedir), ", ", shQuote(name),"),
                full.names = TRUE,
                recursive = TRUE
            )")),
        format = "file_fast",
        deps = name
    )

    list(#rast_cache_init,
         rast_wrap,
         rast_cache_files)
}
