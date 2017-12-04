################################################################################
# Helper aliases for common options

#' Alias to default to a shared cache folder.
#'
#' Helper alias for caching across experiments/people.
#' Just sets the cacheDir to the default SHARE directory 
#' (instead of the typical default PROJECT directory)

#' 
#' @param ... Parameters passed to \code{\link{simpleCache}}.
#' @export
simpleCacheShared = function(...) {
	# Since this is a function calling this, I have to set the loadEnvir here,
	# otherwise by default simpleCache will load the data into *this* environment,
	# which then gets prompty discarded -- not good. The downside is that this
	# function now couldn't take a custom loadEnvir.
	simpleCache(..., cacheDir=getOption("RESOURCES.RCACHE"), loadEnvir=parent.frame())
}

#' Helper alias for loading caches into the global environment.
#' simpleCache normally loads variables into the calling environment; this
#' ensures that the variables are loaded in the global environment.
#'
#' @param ... Parameters passed to \code{\link{simpleCache}}.
#' @export
simpleCacheGlobal = function(...) {
	simpleCache(..., loadEnvir=globalenv())
}

#' Helper alias for loading shared caches into the global environment.
#' 
#' @param ... Parameters passed to \code{\link{simpleCache}}.
#' @export
simpleCacheSharedGlobal = function(...) {
	simpleCache(..., cacheDir=getOption("RESOURCES.RCACHE"), loadEnvir=globalenv())
}

