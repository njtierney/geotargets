#' Create a stars _stars_ Target
#'
#' Provides a target format for stars objects.
#'
#' @param driver character. File format expressed as GDAL driver names passed to [stars::write_stars()]. See [sf::st_drivers()].
#' @param options character. GDAL driver specific datasource creation options passed to [stars::write_stars()]
#' @param proxy logical. Passed to [stars::read_stars()]. If `TRUE` the target will be read as an object of class `stars_proxy`. Otherwise, the object is class `stars`.
#' @param mdim logical. Use the [Multidimensional Raster Data Model](https://gdal.org/user/multidim_raster_data_model.html) via [stars::write_mdim()]? Default: `FALSE`. Only supported for some drivers, e.g. `"netCDF"` or `"Zarr"`.
#' @param ncdf logical. Use the NetCDF library directly to read data via [stars::read_ncdf()]? Default: `FALSE`. Only supported for `driver="netCDF"`.
#' @param ... Additional arguments not yet used
#'
#' @inheritParams targets::tar_target
#' @seealso [targets::tar_target_raw()]
#' @export
#' @examplesIf rlang::is_installed("stars")
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#'  targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#'    library(geotargets)
#'    targets::tar_script({
#'      list(
#'        geotargets::tar_stars(
#'          stars_example,
#'          stars::read_stars(system.file("tif", "olinda_dem_utm25s.tif", package = "stars"))
#'        )
#'      )
#'    })
#'    targets::tar_make()
#'    x <- targets::tar_read(stars_example)
#'  })
#'}
tar_stars <- function(name,
                      command,
                      pattern = NULL,
                      proxy = FALSE,
                      mdim = FALSE,
                      ncdf = FALSE,
                      driver = geotargets_option_get("gdal.raster.driver"),
                      options = geotargets_option_get("gdal.raster.creation.options"),
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

    check_pkg_installed("stars")

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

    tar_stars_raw(
        name = name,
        command = command,
        pattern = pattern,
        proxy = proxy,
        mdim = mdim,
        ncdf = ncdf,
        driver = driver,
        options = options,
        packages = packages,
        library = library,
        repository = repository,
        iteration = "list", #the only option that works
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

#' @export
#' @rdname tar_stars
tar_stars_proxy <- function(name,
                            command,
                            pattern = NULL,
                            mdim = FALSE,
                            ncdf = FALSE,
                            driver = geotargets_option_get("gdal.raster.driver"),
                            options = geotargets_option_get("gdal.raster.creation.options"),
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

    check_pkg_installed("stars")

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

    tar_stars_raw(
        name = name,
        command = command,
        pattern = pattern,
        proxy = TRUE,
        mdim = mdim,
        ncdf = ncdf,
        driver = driver,
        options = options,
        ...,
        packages = packages,
        library = library,
        repository = repository,
        iteration = "list", #the only option that works
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


#' tar_stars method with no tidy eval etc.
#' @noRd
tar_stars_raw <- function(name,
                          command,
                          pattern = NULL,
                          proxy,
                          mdim = FALSE,
                          ncdf = FALSE,
                          driver = geotargets_option_get("gdal.raster.driver"),
                          options = geotargets_option_get("gdal.raster.creation.options"),
                          ...,
                          tidy_eval = targets::tar_option_get("tidy_eval"),
                          packages = targets::tar_option_get("packages"),
                          library = targets::tar_option_get("library"),
                          repository = targets::tar_option_get("repository"),
                          iteration = "list",
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

    driver <- driver %||% "GTiff"
    options <- options %||% character(0)

    # get drivers available for writing (depends on user's GDAL build)
    drv <- sf::st_drivers(what = "raster")
    drv <- drv[drv$write, ]

    driver <- rlang::arg_match0(driver, drv$name)

    .read_stars <- eval(substitute(
        function(path) {

        ## TODO: it appears envvar custom resources do not work in read function?
        READ_FUN <- stars::read_stars
        #  mdim <- as.logical(Sys.getenv("GEOTARGETS_GDAL_RASTER_MDIM", unset = FALSE))
        if (mdim) {
            READ_FUN <- stars::read_mdim
        }

        # ncdf <- as.logical(Sys.getenv("GEOTARGETS_USE_NCMETA", unset = FALSE))
        if (ncdf && requireNamespace("ncmeta")) {
            READ_FUN <- stars::read_ncdf
        }

        # proxy <- as.logical(Sys.getenv("GEOTARGETS_PROXY", unset = FALSE))
        READ_FUN(path, proxy = proxy)

    }, list(ncdf = ncdf, mdim = mdim, proxy = proxy)))

    # TODO: should multidimensional array use the same `options` as 2D?
    .write_stars <- function(object, path) {

        WRITE_FUN <- stars::write_stars

        mdim <- as.logical(Sys.getenv("GEOTARGETS_GDAL_RASTER_MDIM",
                                      unset = FALSE))
        if (mdim) {
            WRITE_FUN <- stars::write_mdim
        }

        dr <- Sys.getenv("GEOTARGETS_GDAL_RASTER_DRIVER")

        # stars requires character(0), not "", for no options set
        co <- Sys.getenv("GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS")
        co2 <- strsplit(co, ";")[[1]]

        WRITE_FUN(
            object,
            path,
            overwrite = TRUE,
            driver = dr,
            options = co
        )
    }

    targets::tar_target_raw(
        name = name,
        command = command,
        pattern = pattern,
        packages = packages,
        library = library,
        format = targets::tar_format(
            read = .read_stars,
            write = .write_stars,
            marshal = function(object) object, # Not currently used
            unmarshal = function(object) object
        ),
        repository = repository,
        iteration = iteration,
        error = error,
        memory = memory,
        garbage_collection = garbage_collection,
        deployment = deployment,
        priority = priority,
        resources = utils::modifyList(targets::tar_resources(
            custom_format = targets::tar_resources_custom_format(
                #these envvars are used in read and write functions of format
                envvars = c("GEOTARGETS_GDAL_RASTER_DRIVER" = driver,
                            "GEOTARGETS_GDAL_RASTER_CREATION_OPTIONS" =
                                paste0(options, collapse = ";"),
                            "GEOTARGETS_GDAL_RASTER_MDIM" = mdim,
                            "GEOTARGETS_PROXY" = proxy,
                            "GEOTARGETS_USE_NCMETA" = ncdf)
            )
        ), resources),
        storage = storage,
        retrieval = retrieval,
        cue = cue,
        description = description
    )
}
