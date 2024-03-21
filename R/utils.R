# helper for adding "geotargets." when options missing that.
geotargets_repair_option_name <- function(option_name) {
  if (!startsWith(option_name, "geotargets.")) {
    option_name <- paste0("geotargets.", option_name)
  }
}

check_pkg_installed <- function(pkg, call = rlang::caller_env()) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(
        message = "package {.pkg {pkg}} is required",
        call = call
    )
  }
}
