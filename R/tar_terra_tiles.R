#' Split a raster into tiles that can be iterated over with dynamic branching
#'
#' This target factory is useful when a raster is too large or too high
#' resolution to work on in-memory. It can instead be split into tiles that can
#' be iterated over, potentially using parallel workers.
#'
#' @param raster a `SpatRaster` object to be split into tiles
#' @param tile_fun a helper function that returns a list of numeric vectors such as [tile_grid] or [tile_blocksize] specified in one of the following ways:
#'  - A named function, e.g. `tile_blocksize` or `"tile_blocksize"`
#'  - An anonymous function, e.g. `\(x) tile_grid(x, nrow = 2, ncol = 2)`
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::makeTiles()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::makeTiles()]
#'
#' @param ... additional arguments not yet used
#' @inheritParams targets::tar_target
#' @author Eric Scott
#'
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'   `"list"`, the only option that works currently.
#'
#' @return a list of two targets: an upstream target that creates a list of
#'   extents and a downstream pattern that maps over these extents to create a
#'   list of SpatRaster objects.
#'
#' @seealso [tile_grid()], [tile_blocksize()], [tar_terra_rast()]
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
        tile_fun,
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
        tile_fun = rlang::enexpr(tile_fun),
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
        tile_fun,
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
        command = rlang::call2(tile_fun, raster),
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
#' Create a new SpatRaster object as specified by a window (area of interest)
#' over the original SpatRaster. This is a wrapper around [terra::window()]
#' which, rather than modifying the SpatRaster in place, returns a new
#' SpatRaster leaving the original unchanged.
#'
#' @param raster a SpatRaster object
#' @param window a SpatExtent object defining the area of interest
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



#' Helper functions to create tiles
#'
#' Wrappers around [terra::getTileExtents()] that return a list of named numeric
#' vectors describing the extents of tiles rather than `SpatExtent` objects.
#' While these may have general use, they are intended primarily for supplying
#' to the `tile_fun` argument of [tar_terra_tiles()].
#'
#' `tile_blocksize()` creates extents using the raster's native blocksize (see
#' [terra::fileBlocksize()]), which should be more memory efficient. `tile_grid()`
#' allows specification of a number of rows and columns to split the raster
#' into.  E.g. nrow = 2 and ncol = 2 would create 4 tiles (because it specifies a 2x2 matrix, which has 4 elements).
#'
#' @param raster a SpatRaster object
#' @param ncol integer; number of columns to split the SpatRaster into
#' @param nrow integer; number of rows to split the SpatRaster into
#'
#' @author Eric Scott
#' @return list of named numeric vectors with xmin, xmax, ymin, and ymax values
#'   that can be coerced to SpatExtent objects with [terra::ext()].
#' @export
#' @rdname tile_helpers
#'
#' @examples
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- terra::rast(f)
#' r_tiles <- tile_grid(r, ncol = 2, nrow = 2)
#' r_tiles
tile_grid <- function(raster, ncol, nrow) {
    template <- terra::rast(
        x = terra::ext(raster),
        ncol = ncol,
        nrow = nrow,
        crs = terra::crs(raster)
        )
    tile_ext <- terra::getTileExtents(
        x = raster,
        template
        )
    n_tiles <- seq_len(nrow(tile_ext))
    tile_list <- lapply(
        n_tiles,
        \(i) tile_ext[i,]
        )
    tile_list
}

#' @export
#' @rdname tile_helpers
tile_blocksize <- function(raster) {
    tile_ext <- terra::getTileExtents(raster, terra::fileBlocksize(raster)[1,])
    n_tiles <- seq_len(nrow(tile_ext))
    tile_list <- lapply(
        n_tiles,
        \(i) tile_ext[i,]
    )
    tile_list
}

