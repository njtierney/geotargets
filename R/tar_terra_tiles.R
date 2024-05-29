#' Split a raster into tiles that can be iterated over with dynamic branching
#'
#' This target factory is useful when a raster is too large or too high
#' resolution to work on in-memory. It can instead be split into tiles that can
#' be iterated over, potentially using parallel workers.
#'
#' @param raster a `SpatRaster` object to be split into tiles
#' @param template passed to the `y` argument of [terra::makeTiles()]—a
#'   `SpatRaster` or `SpatVector` defining the zones; or numeric specifying the
#'   number of rows and columns for each zone (1 or 2 numbers if the number of
#'   rows and columns is not the same)
#' @param tiles_dir path to a directory to save the tiles to disk
#' @param filetype character. File format expressed as GDAL driver names passed
#'   to [terra::makeTiles()]
#' @param gdal character. GDAL driver specific datasource creation options
#'   passed to [terra::makeTiles()]
#' @param ... additional arguments not yet used
#' @inheritParams targets::tar_target
#' @author Eric Scott
#' @returns A list of three targets, an upstream target that splits the raster
#'   into tiles and saves them with `terra::makeTiles()`, a files target that
#'   maps over the resulting paths with dynamic branching, and a downstream
#'   target that reads those files in with `terra::rast()`. The files and
#'   downstream targets are both patterns.
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
#'                 template = terra::rast(ncols = 2, nrows = 2, ext = ext(my_map)),
#'                 tiles_dir = tempdir()
#'             )
#'         )
#'     })
#'     targets::tar_manifest()
#'   })
#' }
tar_terra_tiles <- function(
        name,
        raster,
        template, #E.g. terra::rast(ncols = 3, nrows = 3)
        tiles_dir, #dir to save tiles to disk.  Can't be inside _targets/ store
        filetype = geotargets_option_get("gdal.raster.driver"),
        gdal = geotargets_option_get("gdal.raster.creation.options"),
        ...,
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
        cue = targets::tar_option_get("cue"),
        description = targets::tar_option_get("description")
) {
    name <- targets::tar_deparse_language(substitute(name))
    tar_terra_tiles_raw(
        name = name,
        raster = rlang::enexpr(raster),
        template = rlang::enexpr(template),
        tiles_dir = tiles_dir,
        filetype = filetype,
        gdal = gdal,
        ...,
        packages = packages,
        library = library,
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
        cue = cue,
        description = description
    )
}

#' @noRd
tar_terra_tiles_raw <- function(
        name,
        raster,
        template, #E.g. terra::rast(ncols = 3, nrows = 3)
        tiles_dir, #dir to save tiles to disk.  Can't be inside _targets/ store
        filetype = geotargets_option_get("gdal.raster.driver"),
        gdal = geotargets_option_get("gdal.raster.creation.options"),
        ...,
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
        cue = targets::tar_option_get("cue"),
        description = targets::tar_option_get("description")
) {
    targets::tar_assert_chr(name, "name must be a character.")
    targets::tar_assert_scalar(name, "name must have length 1.")
    filetype <- filetype %||% "GTiff"
    gdal <- gdal %||% character(0)

    name_tiles <- paste0(name, "_tile")
    name_files <- paste0(name, "_files")
    sym_tiles <- as.symbol(name_tiles)
    sym_files <- as.symbol(name_files)

    #target to create files
    upstream <- targets::tar_target_raw(
        name = name_tiles,
        command = rlang::expr(
                terra::makeTiles(
                    !!raster,
                    !!template,
                    filename = file.path(!!tiles_dir, !!name_tiles),
                    overwrite = TRUE,
                    filetype = !!filetype,
                    gdal = !!gdal
                )
        ),
        pattern = NULL,
        packages = packages,
        library = library,
        format = "rds",
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
        cue = targets::tar_cue(mode = "always"), #TODO find a way to make this not necessary.  If resulting files get deleted, this doesn't get invalidated when cue = cue because this target isn't format = 'file'.  Currently though, this means this potentially expensive step runs every time!
        description = description
    )

    #files target maps over the result of upstream with format = "file"
    files <- targets::tar_target_raw(
        name = name_files,
        command = as.expression(sym_tiles),
        pattern = as.expression(as.call(c(as.symbol("map"), sym_tiles))),
        packages = packages,
        library = library,
        format = "file", #TODO allow "file_fast" as an option—see tar_files_raw
        repository = repository,
        iteration = "list", #only list works (for now at least)
        error = error,
        memory = memory,
        garbage_collection = garbage_collection,
        deployment = deployment,
        priority = priority,
        storage = storage,
        retrieval = retrieval,
        cue = cue,
        description = description
    )

    #downstream target reads those files in as SpatRaster objects
    #TODO ideally this would only be outdated when the upstream target changes rather than the files target.  That way the tiles files could save to a tempdir() that the user doesn't touch and they could even be deleted after the downstream target runs to prevent there from being duplicates.

    downstream <- targets::tar_target_raw(
        name = name,
        command = as.expression(as.call(c(as.symbol("rast"), sym_files))),
        pattern = as.expression(as.call(c(as.symbol("map"), sym_files))),
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
        cue = cue, #TODO I think if we want to be able to delete the tiles stored outside of _targets/ in `tiles_dir`, then this needs to only be invalidated when the upstream target is invalidated and not care about the files target.  Then, the upstream target doesn't need to run "always" maybe?
        description = description
    )
    out <- list(upstream, files, downstream)
    names(out) <- c(name_tiles, name_files, name)
    out
}

