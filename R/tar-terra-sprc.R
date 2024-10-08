#' Create a terra _SpatRasterCollection_ target
#'
#' Provides a target format for [terra::SpatRasterCollection] objects,
#'   which have no restriction in the extent or other geometric parameters.
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#'
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'   `"list"`, the only option that works currently.
#'
#' @returns target class "tar_stem" for use in a target pipeline
#' @seealso [targets::tar_target_raw()]
#' @author Andrew Gene Brown
#' @author Nicholas Tierney
#' @export
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'   targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'     library(geotargets)
#'     targets::tar_script({
#'       elev_scale <- function(z = 1, projection = "EPSG:4326") {
#'         terra::project(
#'           terra::rast(system.file("ex", "elev.tif", package = "terra")) * z,
#'           projection
#'         )
#'       }
#'       list(
#'         tar_terra_sprc(
#'           raster_elevs,
#'           # two rasters, one unaltered, one scaled by factor of 2 and
#'           # reprojected to interrupted good homolosine
#'           command = terra::sprc(list(
#'             elev_scale(1),
#'             elev_scale(2, "+proj=igh")
#'           ))
#'         )
#'       )
#'     })
#'     targets::tar_make()
#'     x <- targets::tar_read(raster_elevs)
#'   })
#' }
tar_terra_sprc <- function(name,
                           command,
                           pattern = NULL,
                           filetype = geotargets_option_get("gdal.raster.driver"),
                           gdal = geotargets_option_get("gdal.raster.creation.options"),
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
    #ensure that user-passed `resources` doesn't include `custom_format`
    if ("custom_format" %in% names(resources)) {
        cli::cli_abort("{.val custom_format} cannot be supplied to targets created with {.fn tar_terra_sprc}")
    }

    gdal <- gdal %||% character(0)
    filetype <- filetype %||% "GTiff"

    # check that filetype option is available
    drv <- get_gdal_available_driver_list("raster")
    filetype <- rlang::arg_match0(filetype, drv$name)

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

    tar_terra_collection_raw(
        name = name,
        command = command,
        pattern = pattern,
        filetype = filetype,
        gdal = gdal,
        packages = packages,
        library = library,
        format = format_terra_collections(type = "sprc"),
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


#' Create a terra _SpatRasterDataset_ target
#'
#' Provides a target format for [terra::SpatRasterDataset] objects,
#'   which hold sub-datasets, each a `SpatRaster` that can have multiple layers.
#'
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::writeRaster()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::writeRaster()]
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#'
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'   `"list"`, the only option that works currently.
#'
#' @returns target class "tar_stem" for use in a target pipeline
#' @seealso [targets::tar_target_raw()], [tar_terra_sprc()]
#' @author Andrew Gene Brown
#' @author Nicholas Tierney
#' @author Eric R. Scott
#' @export
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'   targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'     library(geotargets)
#'     targets::tar_script({
#'       elev_scale <- function(z = 1) {
#'           terra::rast(system.file("ex", "elev.tif", package = "terra")) * z
#'       }
#'       list(
#'         tar_terra_sds(
#'           raster_elevs,
#'           # two rasters, one unaltered, one scaled by factor of 2
#'           command = terra::sds(list(
#'             elev_scale(1),
#'             elev_scale(2)
#'           ))
#'         )
#'       )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(raster_elevs)
#'   })
#' }
tar_terra_sds <- function(name,
                          command,
                          pattern = NULL,
                          filetype = geotargets_option_get("gdal.raster.driver"),
                          gdal = geotargets_option_get("gdal.raster.creation.options"),
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
    #ensure that user-passed `resources` doesn't include `custom_format`
    if ("custom_format" %in% names(resources)) {
        cli::cli_abort("{.val custom_format} cannot be supplied to targets created with {.fn tar_terra_sprc}")
    }

    gdal <- gdal %||% character(0)
    filetype <- filetype %||% "GTiff"

    # check that filetype option is available
    drv <- get_gdal_available_driver_list("raster")
    filetype <- rlang::arg_match0(filetype, drv$name)

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

    tar_terra_collection_raw(
        name = name,
        command = command,
        pattern = pattern,
        filetype = filetype,
        gdal = gdal,
        packages = packages,
        library = library,
        format = format_terra_collections(type = "sds"),
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




#' @noRd
tar_terra_collection_raw <- function(
        name,
        command,
        filetype = geotargets_option_get("gdal.raster.driver"),
        gdal = geotargets_option_get("gdal.raster.creation.options"),
        pattern = NULL,
        packages = targets::tar_option_get("packages"),
        library = targets::tar_option_get("library"),
        format = format_terra_collections(),
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
    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = format,
        repository = repository,
        iteration = "list",
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
                        "GEOTARGETS_GDAL_RASTER_DRIVER" = filetype,
                        "GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS" = (
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




#' Format function for sprc and sds
#' @noRd
format_terra_collections <- function(type = c("sprc", "sds")) {
    type <- match.arg(type)
    targets::tar_format(
        read = switch(type,
                      "sprc" = function(path) terra::sprc(path),
                      "sds" = function(path) terra::sds(path)
        ),
        write = function(object, path) {
            gdal <- strsplit(
                Sys.getenv("GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS",
                           unset = ";"),
                ";")[[1]]

            for (i in seq(object)) {
                if (i > 1) {
                    opt <- "APPEND_SUBDATASET=YES"
                } else {
                    opt <- ""
                }
                withCallingHandlers(
                    warning = function(cnd) {
                        # The warning message "[rast] skipped sub-datasets..."
                        # is printed because the return value of writeRaster()
                        # is rast(<output>).  In this context it is not
                        # informative since the write function is sprc(), not
                        # rast()
                        if (grepl("\\[rast\\] skipped sub-datasets", cnd$message)) {
                            rlang::cnd_muffle(cnd)
                        } else {
                            warning(cnd$message)
                        }
                    },
                    terra::writeRaster(
                        x = object[i],
                        filename = path,
                        filetype = Sys.getenv("GEOTARGETS_GDAL_RASTER_DRIVER"),
                        overwrite = (i == 1),
                        gdal = c(opt, gdal)
                    )
                )
            }
        },
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}
