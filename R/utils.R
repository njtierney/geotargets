replace_dot_zip <- function(x, replacement) {
  gsub(
    pattern = "\\.zip",
    replacement = replacement,
    x = basename(x)
  )
}

replace_dot_zip_with_shp <- function(x) {
  replace_dot_zip(x, ".shp")
}
