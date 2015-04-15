library(simpleCache)

context("Context here...")

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


# R lazy function evaluation notes:
#myx = function (yesno, expr) {
#	if (yesno) expr
#	else expr = substitute(expr)
#	return(expr)
#}

#a = myx (FALSE, {rnorm(1e5)} )
#a
#class(a)
#eval(a)

#expression
