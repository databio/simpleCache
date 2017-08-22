#' Deletes caches
#'
#' Given a cache name, this function will attempt to delete the cache of that
#' name on disk.
#' @param cacheNames	Name(s) of the cache to delete
#' @param cacheDir Directory where caches are kept
#' @param force Force deletion without user prompt
#' @export
#' @example
#' R/examples/example.R
deleteCaches = function(cacheNames, cacheDir=getOption("RCACHE.DIR"),
	force=FALSE) {
	
	if (force) {
		response = "y"
	} else {
		response = readline("Are you sure you want to delete this cache? [y/N]")
	}

	if (tolower(response) == "yes" || tolower(response) == "y") {
		for (cacheName in cacheNames) {
			cacheFile = file.path(cacheDir, paste0(cacheName, ".RData"))
			message("Deleting ", cacheFile)
			unlink(cacheFile)
		}
	} else {
		message("User aborted cache delete.")
	}
}