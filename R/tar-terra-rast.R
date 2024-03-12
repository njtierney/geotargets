#' Targets format for terra rasters
#'
#' Provides targets format for `terra::rast`
#'
#' @inheritParams targets::tar_target
#'
#' @export
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' library(geotargets)
#' targets::tar_script({
#'   list(
#'       geotargets::tar_terra_rast(
#'           test,
#'           system.file("ex/elev.tif", package="terra") |> terra::rast()
#'       )
#'   )
#'   })
#'    targets::tar_make()
#' x <- targets::tar_read(test)
#' })
#' }
tar_terra_rast <- function(name,
                           command,
                           pattern = NULL,
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

    format_terra_rast_geotiff <- targets::tar_format(
        read = function(path) terra::rast(path),
        write = function(object, path) {
            terra::writeRaster(
                x = object,
                filename = path,
                overwrite = TRUE,
                filetype = "GTiff"
            )
        },
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = format_terra_rast_geotiff,
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
