# Tests for enforcing cache lifespan requirement.

context("lifespan")

my_test_that = function(description, instruction) {
  setupLSTest()
  test_that(description, instruction)
  cleanLSTest()
}

# Negative control
my_test_that("Cache file isn't replaced if no lifespan is specified and recreate=FALSE", {
  expect_equal(0, countCacheFiles())
  simpleCache("testDF", recreate=FALSE, instruction={ buildTestFrame() })
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_equal(1, countCacheFiles())
  t0 = file.info(fp)$ctime
  simpleCache("testDF", recreate=FALSE, instruction={ buildTestFrame() })
  t1 = file.info(fp)$ctime
  expect_equal(1, countCacheFiles())
  expect_equal(t0, t1)
})

# Another sort of control
my_test_that("Cache file is replaced if no lifespan is specified and recreate=TRUE", {
  
})

# Specificity
my_test_that("Cache remains unchanged if younger than explicit lifespan", {
  
})

# Sensitivity
my_test_that("Cache is replaced if older than explicit lifespan, with ", {

})

# Explicit recreate argument trumps cache lifespan to determine recreation.
my_test_that("Cache is replaced if recreate=TRUE even if cache is fresh", {

})
