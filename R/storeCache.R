#' Sometimes you use significant computational power to create an object,
#' but you didn't cache it with simpleCache. Oops, maybe you wish you had, after
#' the fact. This function lets you store an object in the environment so
#' it could be loaded by future calls to simpleCache.
#'
#' This should just be used in interactive sessions.
#' 
#' @export
storeCache =function(cacheName, cacheDir=getOption("RCACHE.DIR"), cacheSubDir=NULL, recreate=FALSE) {
	if(!is.null(cacheSubDir)) {
		cacheDir=paste0(cacheDir, cacheSubDir);
	}

	if (is.null(cacheDir)) {
		message("You must set global option RCACHE.DIR with setSharedCacheDir(), or specify a cacheDir parameter directly to simpleCache().");
		return(NA);
	}
	if(! "character" %in% class(cacheName)) {
		stop("storeCache expects the cacheName variable to be a character vector.");
	}

	cacheDir=enforceTrailingSlash(cacheDir);
	if (!file.exists(cacheDir)) {
		dir.create(cacheDir, recursive=TRUE);
	}
	cacheFile = paste0(cacheDir, cacheName, ".RData")
	if(file.exists(cacheFile) & !recreate) {
		message("::Cache already exists (use recreate to overwrite)::\t", cacheFile);
		return (NULL);
	} else if (!exists(cacheName)) {
		message("::Object does not exist::\t", cacheName);
		return(NULL);
	} else {
		message("::Creating cache::\t", cacheFile);
		ret = get(cacheName);
		save(ret, file=cacheFile);
	}
}
