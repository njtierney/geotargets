#' Manage geotargets Cache Files
#'
#' The `geotargets` cache is a folder containing one subfolder for each target created using `tar_terra_rast_wrap()` method. Each subfolder contains one or more files containing data and metadata to support a `terra` `SpatRaster` object.
#'
#' The cache directory can be customized using `geotargets_option_set("cache.dir")` or environment variable `GEOTARGETS_CACHE_DIR`.
#'
#' Periodically you may want to purge files in the cache using `geotargets_destroy_cache()`.
#'
#' @param name character. Target name (default: `NULL` will delete all target cache files)
#' @param init logical. Re-create empty cache directory? Default: `FALSE`
#'
#' @return integer. 0 for success, 1 for failure, invisibly.
#' @export
#' @rdname geotargets-cache
#' @examples
#' \dontrun{
#'
#' # delete cache folder for target named "foo"
#' geotargets_destroy_cache("foo")
#'
#' # create empty folder
#' geotargets_init_cache("foo")
#'
#' # delete and recreate folder
#' geotargets_destroy_cache("foo", init = TRUE)
#'
#' # delete all cache files
#' geotargets_destroy_cache()
#'
#' }
geotargets_destroy_cache <- function(name = NULL, init = FALSE) {
    cachedir <- geotargets_option_get("cache.dir")
    target_cache_dir <- file.path(cachedir %||% "geotargets_cache", name %||% "")
    res <- unlink(target_cache_dir, recursive = TRUE)
    if (init) geotargets_init_cache(name = name)
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
