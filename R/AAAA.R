geotargets.env <- new.env()

geotargets_env <- function() {
    geotargets.env
}

.onAttach <- function(lib, pkg) {
    geotargets.env$geotargets.raster.gdal_creation_options <- "ENCODING=UTF-8"
    geotargets.env$geotargets.raster.gdal_driver_name <- "GTiff"
}
