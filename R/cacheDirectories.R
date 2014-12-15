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
setSharedCacheDir = function(sharedCacheDir) {
	options(SHARE.RCACHE.DIR=sharedCacheDir); 
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
	message("SIMPLECACHE.ENV:\t", getOption("SIMPLECACHE.ENV"))
}

#' append a new Environment name (a character string) to a global option
#' which is a vector of such names. SimpleCache will search all of these
#' environments to check if a cache is previously loaded, before reloading it.
#' @export
addCacheSearchEnvironment = function(addEnv) {
	options(SIMPLECACHE.ENV=append(addEnv, getOption("SIMPLECACHE.ENV"))); 
}

#' Sets global option of cache search environments to NULL.
#' @export
resetCacheSearchEnvironment = function() {
	options(SIMPLECACHE.ENV=NULL); 
}



