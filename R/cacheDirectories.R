################################################################################
# CACHE DIRECTORY FUNCTIONS
################################################################################
#These are exported functions for interacting with global variables that
#specify default directories for 2 cache types: project caches and shared caches.

#cache dir setter functions
#' Sets a global variable specifying the default cache directory for simpleCache() calls.
#'
#' @export
setCacheDir = function(cacheDir) {
	options(RCACHE.DIR=cacheDir); 
}

#' Sets global variable specifying the default cache directory for simpleCacheShared() calls; this function is simply a helper alias for caching results that will be used across experiments.
#'
#' @export
setSharedCacheDir = function(globalCacheDir) {
	options(SHARE.RCACHE.DIR=globalCacheDir); 
}
#' Sets local cache build directory with scripts for building files.
#'
#' hello
#' @export
setCacheBuildDir = function(cacheBuildDir) {
	options(RBUILD.DIR=cacheBuildDir); 
}

#' Views cache dir global variables
#' @export
viewCacheDirs = function() {
	message("SHARE.RCACHE.DIR:\t", getOption("SHARE.RCACHE.DIR"))
	message("RCACHE.DIR:\t", getOption("RCACHE.DIR"))
	message("RBUILD.DIR:\t", getOption("RBUILD.DIR"))
}

