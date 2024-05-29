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
        cue = targets::tar_cue(mode = "always"),
        description = description
    )

    #files target maps over the result of upstream with format = "file"
    files <- targets::tar_target_raw(
        name = name_files,
        command = as.expression(sym_tiles),
        # pattern = as.expression(tarchetypes:::call_function("map", list(sym_tiles))),
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
    downstream <- targets::tar_target_raw(
        name = name,
        # command = as.expression(tarchetypes:::call_function("rast", list(sym_files))),
        command = as.expression(as.call(c(as.symbol("rast"), sym_files))),
        # pattern = as.expression(tarchetypes:::call_function("map", list(sym_files))),
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
