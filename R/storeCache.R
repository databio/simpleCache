#' Stores as a cache an already-produced R object
#'
#' Sometimes you use significant computational power to create an object, but
#' you didn't cache it with simpleCache. Oops, maybe you wish you had, after the
#' fact. This function lets you store an object in the environment so it could
#' be loaded by future calls to simpleCache.
#'
#' This can be used in interactive sessions, but could also be used for another
#' use case: you have a complicated set of instructions (too much to pass as the
#' instruction argument to simpleCache), so you could just stick a call to
#' storeCache at the end.
#' 
#' @param cacheName    Unique name for the cache (and R object to be cached).
#' @param cacheDir 	The directory where caches are saved (and loaded from).
#'     Defaults to the global RCACHE.DIR variable
#' @param cacheSubDir You can specify a subdirectory within the cacheDir
#'     variable. Defaults to NULL.
#' @param recreate	Forces reconstruction of the cache
#' @export
storeCache = function(cacheName, cacheDir=getOption("RCACHE.DIR"),
	cacheSubDir=NULL, recreate=FALSE) {

	if(!is.null(cacheSubDir)) {
		cacheDir=paste0(cacheDir, cacheSubDir)
	}

	if (is.null(cacheDir)) {
		message(strwrap("You must set global option RCACHE.DIR with
		setSharedCacheDir(), or specify a cacheDir parameter directly to
		simpleCache()."))
		return(NA);
	}

	if(! "character" %in% class(cacheName)) {
		stop(strwrap("storeCache expects the cacheName variable to be a
		character vector."))
	}

	cacheDir = enforceTrailingSlash(cacheDir)
	if (!file.exists(cacheDir)) {
		dir.create(cacheDir, recursive=TRUE)
	}
	cacheFile = paste0(cacheDir, cacheName, ".RData")
	if(file.exists(cacheFile) & !recreate) {
		message("::Cache already exists (use recreate to overwrite)::\t", cacheFile)
		return (NULL)
	} else if (!exists(cacheName)) {
		message("::Object does not exist::\t", cacheName)
		return(NULL)
	} else {
		message("::Creating cache::\t", cacheFile)
		ret = get(cacheName)
		save(ret, file=cacheFile)
	}
}
