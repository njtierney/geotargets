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

    #Upstream target splits raster into tiles and returns vector of filenames

    #FIXME the upstream target isn't working with the error "cannot branch over empty target (rast_split_tile)" (that's `name_tiles` in this example)
    upstream <- targets::tar_target_raw(
        name = name_tiles,
        command = expression(make_tiles(
            raster = raster,
            template = template,
            tiles_dir = tiles_dir,
            name_tiles = name_tiles,
            filetype = filetype,
            gdal = gdal
        )),
        pattern = NULL,
        packages = packages,
        library = library,
        format = "file",
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

    #downstream target reads those files in as SpatRaster objects
    name_tiles_sym <- as.symbol(name_tiles)

    downstream <- targets::tar_target_raw(
        name = name,
        command = as.expression(tarchetypes:::call_function("terra::rast", name_tiles_sym)),
        pattern = as.expression(tarchetypes:::call_function("map", list(name_tiles_sym))),
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
        deployment = "main",
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
            ), resources),,
        storage = "main",
        retrieval = "main",
        cue = cue,
        description = description
    )
    out <- list(upstream, downstream)
    names(out) <- c(name_tiles, name)
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
        raster = raster,
        template = template, #E.g. terra::rast(ncols = 3, nrows = 3)
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

make_tiles <- function(raster, template, tiles_dir, name_tiles, filetype, gdal) {
    terra::ext(template) <- terra::ext(raster)
    fs::dir_create(tiles_dir) #TODO use base R?
    terra::makeTiles(
        raster,
        template,
        filename = fs::path(tiles_dir, name_tiles),
        overwrite = TRUE,
        filetype = filetype,
        gdal = gdal
    )
}
## This works! Why doesn't the upstream target work?
# raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
# template <- terra::rast(ncols = 2, nrows = 2)
# tiles_dir<- "my_tiles"
# name_tiles <- "myrast_tile"
# filetype <- "GTiff"
# gdal <- ""
# make_tiles(raster, template, tiles_dir, name_tiles, filetype, gdal)
