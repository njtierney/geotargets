#' Split a raster into tiles that can be iterated over with dynamic branching
#'
#' This target factory is useful when a raster is too large or too high
#' resolution to work on in-memory. It can instead be split into tiles that can
#' be iterated over, potentially using parallel workers.
#'
#' @param raster a `SpatRaster` object to be split into tiles
#' @param ncol ncol
#' @param nrow nrow
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::makeTiles()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::makeTiles()]
#'
#' @param ... additional arguments not yet used
#' @inheritParams targets::tar_target
#' @author Eric Scott
#'
#' @return a list of two targets: an upstream target that creates a list of
#'   extents and a downstream pattern that maps over these extents to create a
#'   list of SpatRaster objects.
#' @export
#'
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'   targets::tar_dir({
#'     targets::tar_script({
#'         library(targets)
#'         library(geotargets)
#'         library(terra)
#'         list(
#'             tar_target(
#'                 my_file,
#'                 system.file("ex/elev.tif", package="terra"),
#'                 format = "file"
#'             ),
#'             tar_terra_rast(
#'                 my_map,
#'                 terra::rast(my_file)
#'             ),
#'             tar_terra_tiles(
#'                 name = rast_split,
#'                 raster = my_map,
#'                 ncol = 2,
#'                 nrow = 2
#'             )
#'         )
#'     })
#'     targets::tar_manifest()
#'   })
#' }
tar_terra_tiles <- function(
        name,
        raster,
        ncol,
        nrow,
        filetype = geotargets_option_get("gdal.raster.driver"),
        gdal = geotargets_option_get("gdal.raster.creation.options"),
        ...,
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
        description = targets::tar_option_get("description")
) {
    name <- targets::tar_deparse_language(substitute(name))

    tar_terra_tiles_raw(
        name = name,
        raster = rlang::enexpr(raster),
        ncol = ncol,
        nrow = nrow,
        filetype = filetype,
        gdal = gdal,
        ...,
        packages = packages,
        library = library,
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
tar_terra_tiles_raw <- function(
        name,
        raster,
        ncol,
        nrow,
        filetype = geotargets_option_get("gdal.raster.driver"),
        gdal = geotargets_option_get("gdal.raster.creation.options"),
        ...,
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
        description = targets::tar_option_get("description")
) {
    targets::tar_assert_chr(name, "name must be a character.")
    targets::tar_assert_scalar(name, "name must have length 1.")
    filetype <- filetype %||% "GTiff"
    gdal <- gdal %||% character(0)

    name_exts <- paste0(name, "_exts")
    sym_exts <- as.symbol(name_exts)

    #target to create extents to map over
    windows <- targets::tar_target_raw(
        name = name_exts,
        command = rlang::expr(create_tile_exts(!!raster, ncol = !!ncol, nrow = !!nrow)),
        pattern = NULL,
        packages = packages,
        library = library,
        format = "rds",
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
        cue = cue,
        description = description
    )

    # target to crop raster to extents, mapping over extents
    tiles <- targets::tar_target_raw(
        name = name,
        command = rlang::expr(set_window(!!raster, terra::ext(!!sym_exts))),
        pattern = as.expression(as.call(c(as.symbol("map"), sym_exts))),
        packages = packages,
        library = library,
        format = targets::tar_format(
            read = function(path) terra::rast(path),
            write = function(object, path) {
                terra::writeRaster(
                    object,
                    path,
                    filetype = Sys.getenv("GEOTARGETS_GDAL_RASTER_DRIVER"),
                    overwrite = TRUE,
                    gdal = strsplit(
                        Sys.getenv("GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS",
                                   unset = ";"),
                        ";")[[1]]
                )
            },
            marshal = function(object) terra::wrap(object),
            unmarshal = function(object) terra::unwrap(object)
        ),
        repository = repository,
        iteration = "list", #only list works (for now at least)
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

    list(windows, tiles)
}

#' Copy a raster within a window
#'
#' Create a new SpatRaster object based on an original SpatRaster and a window.
#' This is a wrapper around [terra::window()] which, rather than modifying the
#' SpatRaster in place, returns a new SpatRaster leaving the original unchanged.
#'
#' @param raster A SpatRaster object
#' @param window A SpatExtent object
#'
#' @note While this may have general use, it was created primarily for use
#'   within [tar_terra_tiles()].
#' @author Eric Scott
#' @export
#' @examples
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- terra::rast(f)
#' e <- terra::ext(c(5.9, 6,49.95, 50))
#' r2 <- set_window(r, e)
#' terra::ext(r)
#' terra::ext(r2)
#'
set_window <- function(raster, window) {
    raster_out <- c(raster) #forces copying the raster, not just the R object pointing to the same raster in memory
    terra::window(raster_out) <- window
    raster_out
}



#' Create extents for raster tiles
#'
#' A wrapper around [terra::getTileExtents()] for creating a tile specification.
#' Rather than having to supply a template, one is created from the original
#' raster extent and CRS using `ncol` and `nrow`.  The output is a list of named
#' numeric vectors that can be coerced to SpatExtent objects with
#' [terra::ext()].
#'
#' @param raster a SpatRaster object
#' @param ncol integer; number of columns to split the SpatRaster into
#' @param nrow integer; number of rows to split the SpatRaster into
#'
#' @note While this may have general use, it was created primarily for use
#'   within [tar_terra_tiles()].
#' @author Eric Scott
#' @return list of named numeric vectors with xmin, xmax, ymin, and ymax values
#' @export
#'
#' @examples
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- terra::rast(f)
#' create_tile_exts(r, ncol = 2, nrow = 2)
create_tile_exts <- function(raster, ncol, nrow) {
    template <- terra::rast(terra::ext(raster), ncol = ncol, nrow = nrow, crs = terra::crs(raster))
    tile_ext <- terra::getTileExtents(raster, template)
    lapply(1:nrow(tile_ext), \(i) tile_ext[i,])
}
