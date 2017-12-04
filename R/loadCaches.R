#' Loads pre-made caches
#'
#' This function just takes a list of caches, and loads them. It's designed
#' for stuff you already cached previously, so it won't build any caches.
#'
#' @param cacheNames Vector of caches to load.
#' @param ... Additional parameters passed to \code{\link{simpleCache}}.
#' @export
#' @example
#' R/examples/example.R
loadCaches = function(cacheNames, ...) {
	for (i in 1:length(cacheNames)) {
		# By default, load these caches into the environment that
		# calls loadCaches (which is the grandparent, n=2, of the call to	
		# simpleCache.
		simpleCache(cacheNames[i], loadEnvir=parent.frame(n=2), ...)
	}
}
