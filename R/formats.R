format_terra_rast <- function(filetype = c("GTiff", "netCDF")) {
    filetype <- match.arg(filetype)
    #TODO There's got to be a better way to do this by just passing the arg to writeRaster, but I can't figure it out
    if(filetype == "GTiff") {
        write_fun <- function(object, path) {
            terra::writeRaster(
                x = object,
                filename = path,
                overwrite = TRUE,
                filetype = "GTiff"
            )
        }
    } else if (filetype == "netCDF") {
        write_fun <- function(object, path) {
            terra::writeRaster(
                x = object,
                filename = path,
                overwrite = TRUE,
                filetype = "netCDF"
            )
        }
    }
    targets::tar_format(
        read = function(path) terra::rast(path),
        write = write_fun,
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}

