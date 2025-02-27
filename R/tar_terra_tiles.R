#' Split a raster into tiles that can be iterated over with dynamic branching
#'
#' Creates two targets, a list of extents defining tiles and a downstream
#' pattern that maps over these extents to create a list of `SpatRaster` objects
#' that can be used with [dynamic
#' branching](https://books.ropensci.org/targets/dynamic.html).
#'
#' @details When a raster is too large or too high resolution to work on
#'   in-memory, one possible solution is to iterate over tiles. Raster tiles can
#'   then be operated on one at a time, or possibly in parallel if resources are
#'   available, and then the results can be aggregated. A natural way to do this
#'   in the context of a `targets` pipeline is to split the raster into multiple
#'   raster targets with dynamic branching so that downstream targets can be
#'   applied to each branch of the upstream target with the `pattern` argument
#'   to `tar_terra_rast()` or `tar_target()`. `tar_terra_tiles()` facilitates
#'   creation of such a dynamically branched target. This workflow isn't
#'   appropriate for operations that aggregate spatially, only pixel-wise
#'   operations (possibly aggregating across multiple layers).
#'
#' This target factory is useful when a raster is too large or too high
#' resolution to work on in-memory. It can instead be split into tiles that can
#' be iterated over using dynamic branching.
#' @param name Symbol, name of the target. A target name must be a valid name
#'   for a symbol in R, and it must not start with a dot. See
#'   [targets::tar_target()] for more information.
#' @param raster a `SpatRaster` object to be split into tiles.
#' @param tile_fun a helper function that returns a list of numeric vectors such
#'   as [tile_grid()], [tile_n()] or [tile_blocksize] specified in one of the
#'   following ways:
#'  - A named function, e.g. `tile_blocksize` or `"tile_blocksize"`.
#'  - An anonymous function, e.g. `\(x) tile_grid(x, nrow = 2, ncol = 2)`.
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::makeTiles()].
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::makeTiles()].
#'
#' @param ... additional arguments not yet used.
#' @inheritParams targets::tar_target
#' @author Eric Scott
#'
#' @note The `iteration` argument is unavailable because it is hard-coded to
#'   `"list"`, the only option that works currently.
#'
#'   When using the [tile_blocksize()] helper function, you may need to set
#'   `memory = "transient"` on the upstream target provided to the `raster`
#'   argument of `tar_terra_tiles()`.  More details are in the help file for
#'   [tile_blocksize()].
#'
#' @return a list of two targets: an upstream target that creates a list of
#'   extents and a downstream pattern that maps over these extents to create a
#'   list of SpatRaster objects.
#'
#' @seealso [tile_n()], [tile_grid()], [tile_blocksize()], [tar_terra_rast()]
#' @export
#'
#' @examples
#' # For CRAN. Ensures these examples run under certain conditions.
#' # To run this locally, run the code inside this if statement
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
#'                 tile_fun = \(x) tile_grid(x, ncol = 2, nrow = 2)
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
                    filetype = filetype,
                    overwrite = TRUE,
                    gdal = gdal
                )
            },
            marshal = function(object) terra::wrap(object),
            unmarshal = function(object) terra::unwrap(object),
            substitute = list(filetype = filetype, gdal = gdal)
        ),
        repository = repository,
        iteration = "list", #only list works (for now at least)
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

    list(windows, tiles)
}

#' Copy a raster within a window
#'
#' Create a new SpatRaster object as specified by a window (area of interest)
#' over the original SpatRaster. This is a wrapper around [terra::window()]
#' which, rather than modifying the SpatRaster in place, returns a new
#' SpatRaster leaving the original unchanged.
#'
#' @param raster a SpatRaster object.
#' @param window a SpatExtent object defining the area of interest.
#'
#' @note While this may have general use, it was created primarily for use
#'   within [tar_terra_tiles()].
#' @return SpatRaster
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
    # forces copying the raster, not just the R object pointing to the same
    # raster in memory
    raster_out <- c(raster)
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
#' `tile_blocksize()` creates extents using the raster's native block size (see
#' [terra::fileBlocksize()]), which should be more memory efficient. Create
#' tiles with multiples of the raster's blocksize with `n_blocks_row` and
#' `n_blocks_col`. We strongly suggest the user explore how many tiles are
#' created by `tile_blocksize()` before creating a dynamically branched target
#' using this helper. Note that block size is a property of *files* and does not
#' apply to in-memory `SpatRaster`s. Therefore, if you want to use this helper
#' in [tar_terra_tiles()] you may need to ensure the upstream target provided to
#' the `raster` argument is not in memory by setting `memory = "transient"`.
#'
#' `tile_grid()` allows specification of a number of rows and
#' columns to split the raster into.  E.g. nrow = 2 and ncol = 2 would create 4
#' tiles (because it specifies a 2x2 matrix, which has 4 elements).
#'
#' `tile_n()` creates (about) `n` tiles and prints the number of rows, columns,
#' and total tiles created.
#'
#' @param raster a SpatRaster object.
#' @param ncol integer; number of columns to split the SpatRaster into.
#' @param nrow integer; number of rows to split the SpatRaster into.
#' @param n integer; total number of tiles to split the SpatRaster into.
#' @param n_blocks_row integer; multiple of blocksize to include in each tile
#'   vertically.
#' @param n_blocks_col integer; multiple of blocksize to include in each tile
#'   horizontally.
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
#' tile_grid(r, ncol = 2, nrow = 2)
#' tile_blocksize(r)
#' tile_n(r, 8)
#'
#' \dontrun{
#' #usage with tar_terra_tiles
#' list(
#'     tar_terra_rast(
#'         my_map,
#'         terra::rast(system.file("ex/logo.tif", package = "terra"))
#'     ),
#'     tar_terra_tiles(
#'         name = rast_split,
#'         raster = my_map,
#'         tile_fun = tile_blocksize,
#'         description = "Each tile is 1 block"
#'     ),
#'     tar_terra_tiles(
#'         name = rast_split_2blocks,
#'         raster = my_map,
#'         tile_fun = \(x) tile_blocksize(
#'           x,
#'           n_blocks_row = 2,
#'           n_blocks_col = 1
#'           ),
#'         description = "Each tile is 2 blocks tall, 1 block wide"
#'     ),
#'     tar_terra_tiles(
#'         name = rast_split_grid,
#'         raster = my_map,
#'         tile_fun = \(x) tile_grid(x, ncol = 2, nrow = 2),
#'         description = "Split into 4 tiles in a 2x2 grid"
#'     ),
#'     tar_terra_tiles(
#'         name = rast_split_n,
#'         raster = my_map,
#'         tile_fun = \(x) tile_n(x, n = 6),
#'         description = "Split into 6 tiles"
#'     )
#' )
#' }
tile_grid <- function(raster, ncol, nrow) {
    check_is_integerish(ncol)
    check_is_integerish(nrow)
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
        \(i) tile_ext[i, ]
    )
    tile_list
}

#' @export
#' @rdname tile_helpers
tile_blocksize <- function(raster, n_blocks_row = 1, n_blocks_col = 1) {
    check_is_integerish(n_blocks_row)
    check_is_integerish(n_blocks_col)
    tile_ext <-
        terra::getTileExtents(
            raster,
            terra::fileBlocksize(raster)[1, ] * c(n_blocks_row, n_blocks_col)
        )
    n_tiles <- seq_len(nrow(tile_ext))
    tile_list <- lapply(
        n_tiles,
        \(i) tile_ext[i, ]
    )
    tile_list
}

#' @export
#' @rdname tile_helpers
tile_n <- function(raster, n) {
    check_is_integerish(n)
    sq <- sqrt(n)
    sq_round <- floor(sq)
    quotient <- n / sq_round
    is_even <- rlang::is_integerish(quotient)
    is_odd <- !is_even
    if (is_even) {
        nrow <- sq_round
        ncol <- n / nrow
    }
    if (is_odd) {
        nrow <- sq_round
        ncol <- ceiling(quotient) #round up
    }

    cli::cli_inform("creating {nrow} * {ncol} = {nrow*ncol} tile extents\n")
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
        \(i) tile_ext[i, ]
    )
    tile_list
}
