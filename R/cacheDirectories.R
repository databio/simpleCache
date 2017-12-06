################################################################################
# CACHE DIRECTORY FUNCTIONS
################################################################################
# These are exported functions for interacting with global variables that
# specify default directories for 2 cache types: project caches and shared
# caches.

#' Sets a global variable specifying the default cache directory for
#' \code{\link{simpleCache}} calls.
#'
#' @param cacheDir Directory where caches should be stored
#' @export
#' @example
#' R/examples/example.R
setCacheDir = function(cacheDir) {
	options(RCACHE.DIR=cacheDir)
}

#' Set shared cache directory
#'
#' Sets global variable specifying the default cache directory for
#' \code{\link{simpleCacheShared}} calls; this function is simply a helper alias for caching
#' results that will be used across projects.
#'
#' @param sharedCacheDir Directory where shared caches should be stored
#' @export
setSharedCacheDir = function(sharedCacheDir) {
	options(RESOURCES.RCACHE=sharedCacheDir)
}

#' Sets local cache build directory with scripts for building files.
#'
#' @param cacheBuildDir Directory where build scripts are stored.
#' @export
setCacheBuildDir = function(cacheBuildDir) {
	options(RBUILD.DIR=cacheBuildDir)
}

#' View simpleCache options
#'
#' Views simpleCache global variables
#' @export
simpleCacheOptions = function() {
	message("RESOURCES.RCACHE:\t", getOption("RESOURCES.RCACHE"))
	message("RCACHE.DIR:\t", getOption("RCACHE.DIR"))
	message("RBUILD.DIR:\t", getOption("RBUILD.DIR"))
	message("SIMPLECACHE.ENV:\t", getOption("SIMPLECACHE.ENV"))
}

#' Add a cache search environment
#'
#' Append a new Environment name (a character string) to a global option
#' which is a vector of such names. SimpleCache will search all of these
#' environments to check if a cache is previously loaded, before reloading it.
#' 
#' @param addEnv Environment to append to the shared cache search list
#' @export
addCacheSearchEnvironment = function(addEnv) {
	options(SIMPLECACHE.ENV=append(addEnv, getOption("SIMPLECACHE.ENV")))
}

#' Sets global option of cache search environments to \code{NULL}.
#' 
#' @export
resetCacheSearchEnvironment = function() {
	options(SIMPLECACHE.ENV=NULL)
}

