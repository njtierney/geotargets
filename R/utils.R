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

gdal_version <- function() {
    numeric_version(terra::gdal(lib = "gdal"))
}

check_gdal_shz <- function(min_version = "3.1",
                           call = rlang::caller_env()) {
    if (gdal_version() < numeric_version(min_version)) {
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
  # get list of drivers available for writing based on what user's GDAL supports
  drv <- terra::gdal(drivers = TRUE)
  if (utils::packageVersion("terra") > "1.7-74") {
    drv <- drv[drv[[driver_type]] & grepl("write", drv$can), ]
  } else {
    drv <- drv[drv$type == driver_type & grepl("write", drv$can), ]
  }
  drv
}

check_user_resources <- function(resources,
                                 call = rlang::caller_env()) {
    if ("custom_format" %in% names(resources)) {
        cli::cli_abort(
            message = c(
                "{.val custom_format} cannot be supplied to targets created \\
                with {.fn tar_terra_rast}",
                "We see in {.code names(resources)}:",
                "{.val {names(resources)}}"
            ),
            call = call
        )
    }
}

check_is_integerish <- function(x,
                                call = rlang::caller_env(),
                                arg = rlang::caller_arg(x)){

    not_integerish <- !rlang::is_integerish(x)

    if (not_integerish) {
        cli::cli_abort(
            c(
                "{.arg {arg}} must be an integer.",
                "We see that {.arg {arg}} is: {.val {x}}"
            ),
            call = call
        )
    }
}
