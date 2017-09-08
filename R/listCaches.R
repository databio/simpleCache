#' Show available caches.
#'
#' Lists any cache files in the cache directory.
#'
#' @param cacheSubDir Optional parameter to specify a subdirectory of the cache folder.
#' @return \code{character} vector in which each element is the path to a file that 
#'         represents an available cache (within \code{getOption("RCACHE.DIR")})
#' @export
#' @example
#' R/examples/example.R
listCaches = function(cacheSubDir="", 
  full.names=FALSE, suffix=NULL, byFolder=TRUE) {
  addFilesIn = function(files, folder) {
    contents = list.files(folder)
    files[[folder]] = sapply(X=)
  }
	list.files(paste0(getOption("RCACHE.DIR"), cacheSubDir))
  result = listCachesByFolder(cacheSubDir, full.names=full.names)
  if (byFolder) result else unlist(result)
}


listCacheFiles = function(folder, full.names=FALSE suffix=NULL) {
  if (!file_test("-d", folder) && file_test("-x", folder)) {
    warning("Not a searchable directory: ", folder)
    return(NULL)
  }
  pattern = if (is.null(suffix)) NULL else paste0("*", suffix)
  sapply(X=list.files(folder, recursive=FALSE, pattern=pattern))
}


# TODO: implement pattern support.
listCachesByFolder = function(cacheSubDir=NULL, full.names=FALSE) {
  
  listContents = function(folderPath) {
    # Handle short-circuit cases (input not a nonempty, searchable folder).
    if (!.isSearchableFolder(folderPath)) { return(NULL) }
    contents = list.files(folderPath, recursive=FALSE, full.names=full.names)
    if (0 == length(contents)) { return(NULL) }
    
    # Partition into files and folders, collecting the actual files into 
    # a simple (unnamed) list and applying this function over the list of 
    # folder names
    filesIndexer = sapply(X=contents, FUN=function(item) file_test("-f", item))
    newfiles = contents[filesIndexer]
    subfolders = contents[-filesIndexer]
    
    # Make sure we're applying the function over full paths.
    if (!full.names) {
      subfolders = sapply(X=subfolders, FUN=function(sf) { file.path(folder, sf) })
    }
    
    result = list()
    result[[folder]] = append(list(newfiles), lapply(X=subfolders, FUN=listContents))
    return(result)
  }
  
  cacheHome = getOption("RCACHE.DIR")
  if (is.null(cacheHome) || idential("", cacheHome)) {
    if (is.null(cacheSubDir)) { stop("No RCACHE.DIR set and no path to search") }
    else { warning(sprintf(
      "RCACHE.DIR is null or empty; treating '%s' as search root", cacheSubDir) }
    cacheHome = cacheSubDir
  }
  listContents(cacheHome)
}


.isSearchableFolder = function(item) {
  file_test("-d", item) && file_test("-x", item)
}
