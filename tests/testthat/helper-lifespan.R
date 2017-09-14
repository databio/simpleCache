# Ancillary functions for cache lifespan tests.

buildTestFrame = function() { data.frame(matrix(1:9, nrow=3)) }

cleanLSTest = function() {
  unlink(file.path(tempdir(), "lifespan"), recursive=TRUE)
}

cleanCacheFolder = function() {
  cacheFolder = getOption("RCACHE.DIR")
  if (!.inTmpdir(cacheFolder)) {
    stop("Cache folder isn't temporary: ", cacheFolder)
  }
  do.call(what=file.remove, args=list.files(cacheFolder))
}

countCacheFiles = function() { length(list.files(getOption("RCACHE.DIR"))) }

lifespanTestsTmpdir = function() { file.path(tempdir(), "lifespan") }

setupLSTest = function() {
  dir.create(lifespanTestsTmpdir())
  setCacheDir(lifespanTestsTmpdir())
}

.inTmpdir = function(path) { substr(path, 1, length(tempdir())) == tempdir() }
