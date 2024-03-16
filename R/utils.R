# helper for adding "geotargets." when options missing that.
geotargets_repair_option_name <- function(option_name) {
  if (!startsWith(option_name, "geotargets.")) {
    option_name <- paste0("geotargets.", option_name)
  }
}
