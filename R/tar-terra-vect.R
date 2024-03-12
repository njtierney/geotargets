# format_terra_vect_shapefile <- targets::tar_format(
#   read = function(path) {
#     terra::vect(
#       paste0(
#         "/vsizip/",
#         file.path(
#           path,
#           replace_dot_zip_with_shp(path)
#         )
#       )
#     )
#   },
#   write = function(object, path) {
#     terra::writeVector(
#       x = object,
#       filename = replace_dot_zip_with_shp(path),
#       filetype = "ESRI Shapefile",
#       overwrite = TRUE
#     )
#     zf <- list.files(
#       pattern = replace_dot_zip(
#         x = path,
#         replacement = ""
#       )
#     )
#     utils::zip(
#       zipfile = path,
#       files = zf
#     )
#     unlink(zf)
#   },
#   marshal = function(object) terra::wrap(object),
#   unmarshal = function(object) terra::unwrap(object)
# )

#' Targets format for terra vectors
#'
#' Provides targets format for `terra::vect` objects
#'
#' @inheritParams targets::tar_target
#'
#' @export
tar_terra_vect <- function(name,
                          command,
                          pattern = NULL,
                          packages = targets::tar_option_get("packages"),
                          tidy_eval = targets::tar_option_get("tidy_eval"),
                          library = targets::tar_option_get("library"),
                          repository = targets::tar_option_get("repository"),
                          iteration = targets::tar_option_get("iteration"),
                          error = targets::tar_option_get("error"),
                          memory = targets::tar_option_get("memory"),
                          garbage_collection = targets::tar_option_get("garbage_collection"),
                          deployment = targets::tar_option_get("deployment"),
                          priority = targets::tar_option_get("priority"),
                          resources = targets::tar_option_get("resources"),
                          storage = targets::tar_option_get("storage"),
                          retrieval = targets::tar_option_get("retrieval"),
                          cue = targets::tar_option_get("cue")) {
  name <- targets::tar_deparse_language(substitute(name))

  envir <- targets::tar_option_get("envir")

  command <- targets::tar_tidy_eval(
    expr = as.expression(substitute(command)),
    envir = envir,
    tidy_eval = tidy_eval
  )
  pattern <- targets::tar_tidy_eval(
    expr = as.expression(substitute(pattern)),
    envir = envir,
    tidy_eval = tidy_eval
  )

  format_terra_shapefile_zip <- tar_format(
      read = function(path) terra::vect(paste0("/vsizip/{", path, "}")),
      write = function(object, path) {
          terra::writeVector(
              x = object,
              filename = paste0(path, ".shz"),
              filetype = "ESRI Shapefile"
          )
          file.rename(paste0(path, ".shz"), path)
      },
      marshal = function(object) terra::wrap(object),
      unmarshal = function(object) terra::unwrap(object)
  )

  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = packages,
    library = library,
    format = format_terra_shapefile_zip,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}
