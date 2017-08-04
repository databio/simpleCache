#' Loads pre-made caches
#'
#' This function just takes a list of caches, and loads them. It's designed
#' for stuff you already cached previously, so it won't build any caches.
#'
#' @param cacheNames Vector of caches to load.
#' @param ... Additional parameters passed to simpleCache.
#' @export
loadCaches = function(cacheNames, ...) {
	for (i in 1:length(cacheNames)) {
		# By default, load these caches into the environment that
		# calls loadCaches (which is the grandparent, n=2, of the call to	
		# simpleCache.
		simpleCache(cacheNames[i], loadEnvir=parent.frame(n=2), ...)
	}
}


#' Show available caches.
#'
#' Lists any cache files in the cache directory.
#'
#' @param cacheSubDir Optional parameter to specify a subdirectory of the cache folder.
#' @export
#' @examples
#' availCaches()
availCaches = function(cacheSubDir="") {
	list.files(paste0(getOption("RCACHE.DIR"), cacheSubDir))
}


