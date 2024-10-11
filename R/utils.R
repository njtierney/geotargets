# helper for adding "geotargets." when options missing that.
geotargets_repair_option_name <- function(option_name) {
  if (!startsWith(option_name, "geotargets.")) {
    option_name <- paste0("geotargets.", option_name)
  }
    gsub("_", ".", tolower(option_name))
}

check_pkg_installed <- function(pkg, call = rlang::caller_env()) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(
        message = "package {.pkg {pkg}} is required",
        call = call
    )
  }
}

check_gdal_version <- function(min_version = "3.1") {
    terra_gdal <- numeric_version(terra::gdal(lib = "gdal"))
    version <- numeric_version(min_version)
    terra_gdal < min_version
}

check_gdal_shz <- function(min_version = "3.1",
                           call = rlang::caller_env()){
    if (check_gdal_version(min_version)) {
        cli::cli_abort(
            message = c(
                "Must have {.pkg GDAL} version {.val {min_version}} or greater",
                "i" = "To write a {.val .shz} file requires GDAL version \\
        {.val {min_version}} or greater"
            ),
            call = call
        )
    }
}

get_gdal_available_driver_list <- function(driver_type) {
    # get list of drivers available for writing depending on what the user's GDAL supports
    drv <- terra::gdal(drivers = TRUE)
    if (utils::packageVersion("terra") > "1.7-74") {
      drv <- drv[drv[[driver_type]] & grepl("write", drv$can), ]
    } else {
      drv <- drv[drv$type == driver_type & grepl("write", drv$can), ]
    }
    drv
}

semicolon_split <- function(env_vars){
    strsplit(env_vars, ";")[[1]]
}

semicolon_paste <- function(vec){
    paste0(vec, collapse = ";")
}

