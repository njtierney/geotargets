geotargets.env <- new.env()

geotargets_env <- function() {
    geotargets.env
}

.onAttach <- function(lib, pkg) {
    geotargets.env$geotargets.gdal.raster.creation_options <- "ENCODING=UTF-8"
    geotargets.env$geotargets.gdal.raster.driver <- "GTiff"
}
