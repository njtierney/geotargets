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
    sym_tiles <- as.symbol(name_tiles)#TODO not sure if I need both of these
    sym_files <- as.symbol(name_files)

    #FIXME allow providing template that isn't a target if possible
    template <- quote(terra::rast(ncols = 3, nrows = 3))
    # raster <- enexpr(raster)

# browser()
    #Upstream target splits raster into tiles and returns vector of filenames
    # command <- substitute(
    #     make_tiles(
    #         raster = raster,
    #         template = template,
    #         tiles_dir = tiles_dir,
    #         filename = name_tiles,
    #         filetype = filetype,
    #         gdal = gdal
    #     ),
    #     # env = parent.frame()
    #     env = list(raster = raster, template = template, tiles_dir = tiles_dir, name = name_tiles, filetype = filetype, gdal = gdal) #this diffuses raster all the way to the command to create the raster object
    # )

    command <- rlang::expr(
        make_tiles(
            raster = !!raster,
            template = !!template,
            tiles_dir = !!tiles_dir,
            filename = !!name_tiles,
            filetype = !!filetype,
            gdal = !!gdal
        )
    )

    # #this is what the output should be:
    # make_tiles_fake <- function(raster) {
    #     c("my_tiles/myrast_tile1", "my_tiles/myrast_tile2", "my_tiles/myrast_tile3", "my_tiles/myrast_tile4")
    # }
    # command <- substitute(
    #     make_tiles_fake(raster),
    #     env = parent.frame()
    # )

    upstream <- targets::tar_target_raw(
        name = name_tiles,
        command = command,
        pattern = NULL,
        packages = packages,
        library = library,
        deps = "my_map", #TODO shouldn't need this
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
        cue = targets::tar_cue(mode = "always"),
        description = description
    )

    #files target maps over the result of upstream with format = "file"
    files <- tar_target_raw(
        name = name_files,
        command = as.expression(sym_tiles),
        pattern = as.expression(tarchetypes:::call_function("map", list(sym_tiles))),
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
    downstream <- targets::tar_target_raw(
        name = name,
        command = as.expression(tarchetypes:::call_function("rast", list(sym_files))),
        # command = as.expression(as.call(c(as.symbol("rast"), sym_files))),
        pattern = as.expression(tarchetypes:::call_function("map", list(sym_files))),
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
    out <- list(upstream, files, downstream)
    names(out) <- c(name_tiles, name_files, name)
    out
}

#' @export
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
        # template = {{template}}, #E.g. terra::rast(ncols = 3, nrows = 3)
        tiles_dir = tiles_dir, #dir to save tiles to disk.  Can't be inside _targets/ store
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

make_tiles <- function(raster, template, tiles_dir, filename, filetype, gdal) {
    terra::ext(template) <- terra::ext(raster)
    fs::dir_create(tiles_dir) #TODO use base R?

    terra::makeTiles(
        raster,
        template,
        filename = fs::path(tiles_dir, filename),
        overwrite = TRUE,
        filetype = filetype,
        gdal = gdal
    )
}
## This works! Why doesn't the upstream target work?
# raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
# template <- terra::rast(ncols = 2, nrows = 2)
# tiles_dir<- "my_tiles"
# filename <- "myrast_tile"
# filetype <- "GTiff"
# gdal <- ""
# make_tiles(raster, template, tiles_dir, filename, filetype, gdal)
