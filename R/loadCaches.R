#' This function just takes a list of caches, and loads them. It's designed
#' for stuff you already cached previously, so it won't build any caches.
#'
#' @export
loadCaches =function(cacheNames, ...) {
	for (i in 1:length(cacheNames)) {
		simpleCache(cacheNames[i], ...);
	}
}
