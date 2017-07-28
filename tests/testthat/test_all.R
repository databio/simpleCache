library(simpleCache)

context("Test_that")

test_that("Caching respects files existing", {
	set.seed(1)
	setCacheDir("~")
	simpleCache("normSample", " rnorm(5e6, 0,1) ", recreate=TRUE)
	expect_equal(signif(normSample[1], 6), -0.626454)

	simpleCache("normSample", { rnorm(5e6, 0,1) }, recreate=TRUE)
	expect_equal(signif(normSample[1], 6), -0.229762)

	# Should not evaluate:
	simpleCache("normSample", { rnorm(5e6, 0,1) })
	expect_equal(signif(normSample[1], 6), -0.229762)
})

