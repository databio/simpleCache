library(simpleCache)

context("error checking")


# Map option name to its setter.
kSetters = list(RCACHE.DIR=setCacheDir, RESOURCES.RCACHE=setSharedCacheDir, RBUILD.DIR=setCacheBuildDir)


# Test a cache dir setting in managed context fashion, resetting before and after test.
test_dir_default = function(cacheDirOptname) {
  resetCacheSearchEnvironment()
  test_that(sprintf("%s setter uses current folder for argument-less call", cacheDirOptname), {
    do.call(kSetters[[cacheDirOptname]], args=list())
    expect_equal(getwd(), getOption(cacheDirOptname))
  })
  resetCacheSearchEnvironment()
}


test_that("notifications and messages as expected", {
  
  # message if cache exists
  simpleCache("normSample", instruction = {rnorm(5e3, 0,1)}, cacheDir = tempdir(), recreate=TRUE)
  expect_message(simpleCache("normSample", instruction = {rnorm(5e3, 0,1)}, cacheDir = tempdir(), recreate=FALSE, noload = TRUE), "^::Cache exists")
  deleteCaches("normSample", force = TRUE)
  
  # storeCache should not accept non-character cacheName
  expect_error(storeCache(cacheName = normSample, recreate = TRUE, cacheDir = tempdir()), "storeCache expects the cacheName variable to be a character vector.")
  
  # message when cacheDir isn't defined
  expect_message(simpleCache("normSample", { rnorm(5e3, 0,1) }), regexp = "^No cacheDir specified.")
  
  # error when buildDir is empty without instruction
  expect_error(simpleCache("normSample", cacheDir = tempdir(), buildDir = tempdir(), recreate = TRUE), "::Error::\tNo instruction or RBuild file provided.")
  
  # error when buildEnvir includes "instruction"
  expect_error(simpleCache("normSample", { rnorm(5e3, 0,1) }, buildEnvir = list(instruction="foo"), recreate=TRUE, cacheDir = tempdir()), "Can't provide a variable named 'instruction' in buildEnvir")
  
  # error when instruction and buildDir are null
  expect_error(simpleCache("normSample", instruction = NULL, buildDir = NULL, cacheDir = tempdir(), recreate=TRUE))
  
  # error when cacheName is not character
  expect_error(simpleCache(12345, instruction = { rnorm(5e3, 0,1) }, buildDir = NULL, cacheDir = tempdir(), recreate=TRUE))
  
  # message when return is NULL
  expect_message(simpleCache("normSample", instruction = {normSample <- NULL}, recreate = TRUE, cacheDir = tempdir()), "NULL value returned, no cache created")
  
  # we must clean up any temporary caches we make
  deleteCaches("normSample", force=TRUE, cacheDir = tempdir())
  
})

test_that("Caching respects files existing", {
	setCacheDir(tempdir())
	set.seed(1)
	simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate=TRUE)
	expect_equal(signif(normSample[1], 6), -0.626454)

	simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate=TRUE)
	expect_equal(signif(normSample[1], 6), -1.51637)

	# Should not evaluate
	simpleCache("normSample", { rnorm(5e3, 0,1) })
	expect_equal(signif(normSample[1], 6), -1.51637)


	# These delete cache should force the reload to recreate cache
	deleteCaches("normSample", force=TRUE)
	simpleCache("normSample", { rnorm(5e3, 0,1) }, reload=TRUE)
	expect_equal(signif(normSample[1], 6), -0.804332)

	# we must clean up any temporary caches we make
	deleteCaches("normSample", force=TRUE)

})

context("basic functionality")

test_that("timer works", {
  
  setCacheDir(tempdir())
  timeout <- capture_messages(simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate=TRUE, timer = TRUE))[2]
  expect_match(timeout, "<[0-9][0-9]h [0-9][0-9]m [0-9].[0-9]s>")
  
  # we must clean up any temporary caches we make
  deleteCaches("normSample", force=TRUE)
  
})

test_that("cache can be created without loading", {
  
  expect_null(simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate = TRUE, noload = TRUE, cacheDir = tempdir()))
  
  expect_true("normSample.RData" %in% listCaches())
  
  # we must clean up any temporary caches we make
  deleteCaches("normSample", force=TRUE)
  
})

test_that("object can be stored as cache", {

  normSample2 <<- rnorm(5e3,0,1)
  
  expect_message(storeCache("normSample2", cacheDir = NULL, recreate = TRUE), "^You must set global option RCACHE.DIR")
  
  expect_message(storeCache("normSample2", cacheDir = tempdir(), recreate = TRUE), "^::Creating cache::")
  
  expect_message(storeCache("normSample2", cacheDir = tempdir(), recreate = FALSE), "^::Cache already exists")
  
  # we must clean up any temporary caches we make
  deleteCaches("normSample2", force=TRUE)

})

test_that("option setting works", {
  
  # set all options
  setCacheDir(tempdir())
  setSharedCacheDir(tempdir())
  setCacheBuildDir(tempdir())
  addCacheSearchEnvironment("cacheEnv")
  
  # Windows uses double slashes, which get consumed weirdly by grep;
  # This command will replace double slashes with quadruple slashes,
  # which behave correctly in grep.
  grep_tempdir = gsub("\\\\", "\\\\\\\\", tempdir())
  # capture output and check
  options_out <- capture_messages(simpleCacheOptions())
  
  expect_true(grepl(grep_tempdir, options_out[1]))
  expect_true(grepl(grep_tempdir, options_out[2]))
  expect_true(grepl(grep_tempdir, options_out[3]))
  expect_true(grepl("cacheEnv", options_out[4]))
  
  # reset the cache search option
  resetCacheSearchEnvironment()
  
  # check to make sure it is gone
  options_out <- capture_messages(simpleCacheOptions())
  expect_true(!grepl("cacheEnv", options_out[4]))
  
})

test_that("Cache dir fetch works", {
  options(RCACHE.DIR = NULL)
  expect_true(is.null(getCacheDir()))
  setCacheDir(tempdir())
  expect_false(is.null(getCacheDir()))
  expect_equal(getCacheDir(), tempdir())
})

# Test each cache directory option setter.
for (optname in names(kSetters)) { test_dir_default(optname) }


test_that("objects pass through in buildEnvir", {
  
  setCacheDir(tempdir())
  
  set.seed(1)
  simpleCache("piSample", { pi^x }, buildEnvir = list(x=2), recreate=TRUE, timer = TRUE)
  rm(piSample)
  
  simpleCache("piSample", reload = TRUE)
  
  expect_equal(signif(piSample, 3), 9.87)
  
  # we must clean up any temporary caches we make
  deleteCaches("piSample", force=TRUE)
  
})

test_that("caches can be loaded", {
  
  setCacheDir(tempdir())
  
  simpleCache("loadSample", { rnorm(5e3, 0,1) }, recreate=TRUE)
  loadCaches("loadSample")
  
  expect_true("loadSample" %in% ls())
  
  # we must clean up any temporary caches we make
  deleteCaches("loadSample", force=TRUE)
  
})
context("misc")

test_that("listCaches returns name of given cache", {
  
  setCacheDir(tempdir())
  set.seed(1)
  simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate=TRUE)
  expect_true("normSample.RData" %in% listCaches())
  
  # we must clean up any temporary caches we make
  deleteCaches("normSample", force=TRUE)
  
})

