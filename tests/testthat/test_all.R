library(simpleCache)

context("error checking")

test_that("error checking behaves as expected", {
  
  # message when cacheDir isn't defined
  expect_message(simpleCache("normSample", { rnorm(5e3, 0,1) }), regexp = "^No cacheDir specified.")
  
  # warning when instruction is not expression
  expect_warning(simpleCache("normSample", instruction = "rnorm(5e3, 0,1)", cacheDir = tempdir(), recreate=TRUE))
  
  # error when instruction and buildDir are null
  expect_error(simpleCache("normSample", instruction = NULL, buildDir = NULL, cacheDir = tempdir(), recreate=TRUE))
  
  # error when cacheName is not character
  expect_error(simpleCache(12345, instruction = { rnorm(5e3, 0,1) }, buildDir = NULL, cacheDir = tempdir(), recreate=TRUE))
  
  # we must clean up any temporary caches we make
  deleteCaches("normSample", force=TRUE)
  
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
  
  # we must clean up any temporary caches we make
  deleteCaches("normSample", force=TRUE)
  
})

test_that("option setting works", {
  
  # set all options
  setCacheDir(tempdir())
  setSharedCacheDir(tempdir())
  setCacheBuildDir(tempdir())
  
  # capture output and check
  options_out <- capture_messages(viewCacheDirs())
  
  expect_true(grepl(tempdir(), options_out[1]))
  expect_true(grepl(tempdir(), options_out[2]))
  expect_true(grepl(tempdir(), options_out[3]))
  
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

