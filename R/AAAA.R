# geotargets.env <- new.env()
#
# geotargets_env <- function() {
#     geotargets.env
# }

.onAttach <- function(lib, pkg) {
    geotargets_option_set()
}
