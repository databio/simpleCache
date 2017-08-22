library(simpleCache)

context("Test_that")

test_that("Caching respects files existing", {
	setCacheDir(tempdir())
	set.seed(1)
	simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate=TRUE)
	expect_equal(signif(normSample[1], 6), -0.626454)

	simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate=TRUE)
	expect_equal(signif(normSample[1], 6), -1.51637)

	# Should not evaluate:
	simpleCache("normSample", { rnorm(5e3, 0,1) })
	expect_equal(signif(normSample[1], 6), -1.51637)


	# These delete cache should force the reload to recreate cache
	deleteCaches("normSample", force=TRUE)
	simpleCache("normSample", { rnorm(5e3, 0,1) }, reload=TRUE)
	expect_equal(signif(normSample[1], 6), -0.804332)

	# we must clean up any temporary caches we make
	deleteCaches("normSample", force=TRUE)

})

