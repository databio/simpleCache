#' Show available caches.
#'
#' Lists any cache files in the cache directory.
#'
#' @param cacheSubDir Optional parameter to specify a subdirectory of the cache folder.
#' @export
#' @example
#' R/examples/example.R
listCaches = function(cacheSubDir="") {
	list.files(paste0(getOption("RCACHE.DIR"), cacheSubDir))
}

