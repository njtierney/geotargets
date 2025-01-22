#' Create a Virtual terra _SpatRaster_ target
#'
#' Provides a target format for [terra::SpatRaster-class] objects representing a
#' GDAL Virtual Dataset (VRT)
#'
#' @param ... Additional arguments passed to [terra::vrt()]
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
#'        geotargets::tar_terra_vrt(
#'          terra_rast_example,
#'          terra::vrt(system.file("ex/elev.tif", package = "terra"))
#'        )
#'      )
#'    })
#'    targets::tar_make()
#'    x <- targets::tar_read(terra_rast_example)
#'  })
#'}
tar_terra_vrt <- function(name,
                          command,
                          pattern = NULL,
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

    .format_terra_vrt_read <- function(path) {
        terra::rast(path)
    }

    .format_terra_vrt_write <- function(object, path) {
        # list of SpatRaster or SpatRasterCollection => SpatRasterCollection
        if (is.list(object)) {
            object <- terra::sprc(object)
        }

        # SpatRaster or SpatRasterCollection => character vector of source file path
        if (inherits(object, c("SpatRaster", "SpatRasterCollection"))) {
            object <- terra::sources(object)
        }

        # default: the VRT contains project-specific paths to target store
        #  terra returns absolute paths: remove store parent directory from VRT paths
        #  user specified configs with absolute path to target store are preserved
        object <- gsub(paste0(".*(", targets::tar_path_store(), ".*)$"), "\\1", object)

        # add additional arguments from tar_terra_vrt(...)
        VRT_ARGS <- c(list(x = object, filename = path), args)

        # create a SpatRaster with a VRT file referencing the source paths
        do.call(terra::vrt, VRT_ARGS)
    }

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = targets::tar_format(
            read = .format_terra_vrt_read,
            write = .format_terra_vrt_write,
            marshal = function(object) terra::wrap(object),
            unmarshal = function(object) terra::unwrap(object),
            substitute = list(args = list(...))
        ),
        repository = repository,
        iteration = "list",
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
