#' Manage geotargets Cache Files
#'
#' The `geotargets` cache is a folder created when the `use_cache` option is TRUE (see [geotargets_option_set()]). The cache directory can be customized using `geotargets_option_set(cache_dir = )` or environment variable `GEOTARGETS_CACHE_DIR`.
#'
#' Periodically you may want to purge files in the cache using `geotargets_destroy_cache()`.
#'
#' @param cache_dir character. Path to a directory to use as a cache
#' @param init logical. Re-create empty cache directory? Default: `FALSE`
#'
#' @return integer. 0 for success, 1 for failure, invisibly.
#' @export
#' @rdname geotargets-cache
#' @examples
#' \dontrun{
#'
#' # delete all cache files
#' geotargets_destroy_cache()
#'
#' }
geotargets_destroy_cache <- function(cache_dir = geotargets_option_get("cache.dir"), init = FALSE) {
    cache_dir <- cache_dir %||% "_geotargets"
    #invalidate targets that saved files to the cache_dir
    targets::tar_invalidate(targets::any_of(list.files(cache_dir)))
    res <- unlink(cache_dir, recursive = TRUE)
    if (init) geotargets_init_cache(cache_dir = cache_dir)
    invisible(res)
}

#' @rdname geotargets-cache
#' @export
geotargets_init_cache <- function(cache_dir = geotargets_option_get("cache.dir")) {
    #TODO: possibly write a README file with "do not edit by hand" in it??
    cache_dir <- cache_dir %||% "_geotargets"
    dir.create(cache_dir,
               showWarnings = FALSE,
               recursive = TRUE)
}
