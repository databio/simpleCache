# Ancillary functions for cache lifespan tests.

# Build a small data frame to cache.
buildTestFrame = function() { data.frame(matrix(1:9, nrow=3)) }

# Remove test case's temp cache folder.
cleanLSTest = function() { unlink(lifespanTestsTmpdir(), recursive=TRUE) }

# Count the number of items in the cache folder.
countCacheItems = function() { length(list.files(getOption("RCACHE.DIR"))) }

# Generate path to temp folder for test case.
lifespanTestsTmpdir = function() { file.path(tempdir(), "lifespan") }

# Establish a temp folder and set the cache home location to it.
setupLSTest = function() {
  testdir = lifespanTestsTmpdir()
  if (!file.test("-d", testdir)) { dir.create(testdir) }
  setCacheDir(lifespanTestsTmpdir())
}

# Check that a path is in the temporary folder.
.inTmpdir = function(path) { substr(path, 1, length(tempdir())) == tempdir() }
