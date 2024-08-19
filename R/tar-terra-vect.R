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
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'   `"list"`, the only option that works currently.
#'
#' @returns target class "tar_stem" for use in a target pipeline
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
                           filetype = geotargets_option_get("gdal.vector.driver"),
                           gdal = geotargets_option_get("gdal.vector.creation.options"),
                           ...,
                           packages = targets::tar_option_get("packages"),
                           tidy_eval = targets::tar_option_get("tidy_eval"),
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
    filetype <- filetype %||% "GeoJSON"
    gdal <- gdal %||% "ENCODING=UTF-8"

    #Check that filetype is available
    drv <- get_gdal_available_driver_list("vector")
    filetype <- rlang::arg_match0(filetype, drv$name)

    check_pkg_installed("terra")

    #ensure that user-passed `resources` doesn't include `custom_format`
    if ("custom_format" %in% names(resources)) {
        cli::cli_abort("{.val custom_format} cannot be supplied to targets created with {.fn tar_terra_vect}")
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

    format <- ifelse(
        test = filetype == "ESRI Shapefile",
        #special handling of ESRI shapefiles because the output is a dir of multiple files.
        yes = create_format_terra_vect_shz(),
        no =  create_format_terra_vect()
    )

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = format,
        repository = repository,
        iteration = "list",  #only "list" works for now
        error = error,
        memory = memory,
        garbage_collection = garbage_collection,
        deployment = deployment,
        priority = priority,
        resources = utils::modifyList(
            targets::tar_resources(
                custom_format = targets::tar_resources_custom_format(
                    #these envvars are used in write function of format
                    envvars = c(
                        "GEOTARGETS_GDAL_VECTOR_DRIVER" = filetype,
                        "GEOTARGETS_GDAL_VECTOR_CREATION_OPTIONS" = (
                            paste0(gdal, collapse = ";")
                        )
                    )
                )
            ), resources),
        storage = storage,
        retrieval = retrieval,
        cue = cue,
        description = description
    )
}


#' @noRd
create_format_terra_vect <- function() {

    check_pkg_installed("terra")

    targets::tar_format(
        read = function(path) terra::vect(path),
        write = function(object, path) {
            terra::writeVector(
                object,
                path,
                filetype = Sys.getenv("GEOTARGETS_GDAL_VECTOR_DRIVER"),
                overwrite = TRUE,
                options = strsplit(
                    Sys.getenv("GEOTARGETS_GDAL_VECTOR_CREATION_OPTIONS",
                               unset = ";"),
                    ";")[[1]]
            )
        },
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}

#' Special handling for ESRI Shapefiles
#' @noRd
create_format_terra_vect_shz <- function() {

    check_pkg_installed("terra")

    targets::tar_format(
        read = function(path) terra::vect(paste0("/vsizip/{", path, "}")),
        write = function(object, path) {
            terra::writeVector(
                x = object,
                filename = paste0(path, ".shz"),
                filetype = "ESRI Shapefile",
                overwrite = TRUE,
                options = strsplit(
                    Sys.getenv("GEOTARGETS_GDAL_VECTOR_CREATION_OPTIONS",
                               unset = ";"),
                    ";")[[1]]
            )
            file.rename(paste0(path, ".shz"), path)
        },
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}
