geotargets.env <- new.env()

geotargets_env <- function() {
    geotargets.env
}

.onAttach <- function(lib, pkg) {
    geotargets.env$geotargets.gdal.raster.creation_options <- geotargets_option_get("gdal.raster.creation_options")
    geotargets.env$geotargets.gdal.raster.driver <- geotargets_option_get("gdal.raster.driver")

    geotargets.env$geotargets.gdal.vector.creation_options <- geotargets_option_get("gdal.vector.creation_options")
    geotargets.env$geotargets.gdal.vector.driver <- geotargets_option_get("gdal.vector.driver")
}
