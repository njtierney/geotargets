#' Create a terra _SpatRaster_ target
#'
#' Provides a target format for [terra::SpatRaster-class] objects.
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param zipfile logical. Should the file in the target store be a ZIP archive?
#'   Required for `filetype` formats that have sidecar files. Not all GDAL
#'   drivers support directly generating SOZip-enabled files. Default: `FALSE`.
#' @param ... Additional arguments not yet used
#'
#' @note Although you may pass any supported GDAL vector driver to the
#'   `filetype` argument, not all formats are guaranteed to work with
#'   `geotargets`.  At the moment, we have tested `GTiff` and `GPKG` and
#'    they appear to work generally. Both `GTiff` and `GPKG` rasters can be
#'    stored as ZIP files by setting `zipfile=TRUE`. To write a SOZip-enabled
#'    `GTiff` target set `gdal=c("STREAMABLE_OUTPUT=YES", "COMPRESS=NONE")`.
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
                           filetype = geotargets_option_get("gdal.raster.driver"),
                           gdal = geotargets_option_get("gdal.raster.creation.options"),
                           zipfile = FALSE,
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

    .format_terra_rast_read <- eval(substitute(function(path) {
        path2 <- ifelse(zipfile, paste0("/vsizip/{", path, "}"), path)
        terra::rast(path2)
    }, list(zipfile = zipfile)))

    .format_terra_rast_write <- eval(substitute(function(object, path) {
        path2 <- ifelse(zipfile, paste0("/vsizip/{", path, "}/", basename(path)), path)
        filetype <- Sys.getenv("GEOTARGETS_GDAL_RASTER_DRIVER")
        extension <- ""
        if (filetype == "GPKG") {
            extension <- ".gpkg.zip"
            path2 <- paste0(path, extension)
        }
        terra::writeRaster(
            object,
            path2,
            filetype = filetype,
            overwrite = TRUE,
            gdal = strsplit(Sys.getenv("GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS", unset = ";"), ";")[[1]]
        )
        if (extension != "") {
            file.rename(paste0(path, extension), path)
        }
    }, list(zipfile = zipfile)))

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = targets::tar_format(
            read = .format_terra_rast_read,
            write = .format_terra_rast_write,
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
        resources = targets::tar_resources(
            custom_format = targets::tar_resources_custom_format(
                #these envvars are used in write function of format
                envvars = c("GEOTARGETS_GDAL_RASTER_DRIVER" = filetype,
                            "GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS" = paste0(gdal, collapse = ";"))
            )
        ),
        storage = storage,
        retrieval = retrieval,
        cue = cue
    )
}
