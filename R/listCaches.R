#' Show available caches.
#'
#' Lists any cache files in the cache directory.
#'
#' @param cacheSubDir Optional parameter to specify a subdirectory of the cache folder.
#' @return \code{character} vector in which each element is the path to a file that 
#'         represents an available cache (within \code{getOption("RCACHE.DIR")})
#' @export
#' @example
#' R/examples/example.R
listCaches = function(cacheSubDir="") {
	list.files(paste0(getOption("RCACHE.DIR"), cacheSubDir))
}

