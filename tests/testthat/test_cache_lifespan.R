# Tests for enforcing cache lifespan requirement.

context("lifespan")

my_test_that = function(description, instruction) {
  setupLSTest()
  test_that(description, instruction)
  cleanLSTest()
}

mySimpleCache = function(...) { simpleCache(..., noload=TRUE) }

# Negative control
my_test_that("Cache file isn't replaced if no lifespan is specified and recreate=FALSE", {
  expect_equal(0, countCacheFiles())
  mySimpleCache("testDF", recreate=FALSE, instruction={ buildTestFrame() })
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_equal(1, countCacheFiles())
  t0 = file.info(fp)$ctime
  mySimpleCache("testDF", recreate=FALSE, instruction={ buildTestFrame() })
  expect_equal(1, countCacheFiles())
  t1 = file.info(fp)$ctime
  expect_equal(t0, t1)
})

# Another sort of control
my_test_that("Cache file is replaced if no lifespan is specified and recreate=TRUE", {
  expect_equal(0, countCacheFiles())
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_equal(1, countCacheFiles())
  t0 = file.info(fp)$ctime
  mySimpleCache("testDF", recreate=TRUE, instruction={ buildTestFrame() })
  expect_equal(1, countCacheFiles())
  t1 = file.info(fp)$ctime
  expect_true(t1 > t0)
})

# Specificity
my_test_that("Cache remains unchanged if younger than explicit lifespan", {
  expect_equal(0, countCacheFiles())
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_equal(1, countCacheFiles())
  t0 = file.info(fp)$ctime
  mySimpleCache("testDF", lifespan=0.5, instruction={ buildTestFrame() })
  expect_equal(1, countCacheFiles())
  t1 = file.info(fp)$ctime
  expect_true(t1 == t0)
})

# Sensitivity
my_test_that("Cache is replaced if older than explicit lifespan", {
  expect_equal(0, countCacheFiles())
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_equal(1, countCacheFiles())
  t0 = file.info(fp)$ctime
  mySimpleCache("testDF", lifespan=0, instruction={ buildTestFrame() })
  expect_equal(1, countCacheFiles())
  t1 = file.info(fp)$ctime
  expect_true(t1 > t0)
})

# Explicit recreate argument trumps cache lifespan to determine recreation.
my_test_that("Cache is replaced if recreate=TRUE even if cache is fresh", {
  expect_equal(0, countCacheFiles())
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_equal(1, countCacheFiles())
  t0 = file.info(fp)$ctime
  mySimpleCache("testDF", recreate=TRUE, lifespan=0, instruction={ buildTestFrame() })
  expect_equal(1, countCacheFiles())
  t1 = file.info(fp)$ctime
  expect_true(t1 >= t0)
})
