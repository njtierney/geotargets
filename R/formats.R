format_terra_rast <- function() {
    write_fun <- function(object, path) {
        terra::writeRaster(
            x = object,
            filename = path,
            overwrite = TRUE,
            filetype = "GTiff" #TODO would love to be able to control this with an argument to format_terra_rast()
        )
    }
    targets::tar_format(
        read = function(path) terra::rast(path),
        write = write_fun,
        marshal = function(object) terra::wrap(object),
        unmarshal = function(object) terra::unwrap(object)
    )
}

