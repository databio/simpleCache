# test_cache_lifespan.R
# Tests for enforcing cache lifespan requirement.

# The pattern/them for each test is something like:
# 1. Ensure the test case has a fresh, clean folder.
# 2. Create a dummy cache.
# 3. Check that only 1 file is in the cache.
# 4. Grab the cache timestamp.
# 5. Make another simpleCache call.
# 6. Again check that there's a single cache file and grab the timestamp.
# 7. Compare timestamps.

context("lifespan")

# Provide clean cache folder (pre-set) for each test case.
my_test_that = function(description, instruction) {
  setupLSTest()
  test_that(description, instruction)
  cleanLSTest()
}

# Control loading behavior for these tests to focus on lifespan/recreate effects.
mySimpleCache = function(...) { simpleCache(..., noload=TRUE) }

# Negative control
my_test_that("Cache file isn't replaced if no lifespan is specified and recreate=FALSE", {
  expect_equal(0, countCacheItems())
  mySimpleCache("testDF", recreate=FALSE, instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  t0 = file.info(fp)$ctime
  mySimpleCache("testDF", recreate=FALSE, instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  t1 = file.info(fp)$ctime
  expect_equal(t0, t1)
})

# Another sort of control
my_test_that("Cache file is replaced if no lifespan is specified and recreate=TRUE", {
  expect_equal(0, countCacheItems())
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_false(file_test("-f", fp))
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  expect_true(file_test("-f", fp))
  t0 = file.info(fp)$mtime
  Sys.sleep(1)    # Delay so that our time comparison can work.
  mySimpleCache("testDF", recreate=TRUE, instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  t1 = file.info(fp)$mtime
  expect_true(t1 > t0)
})

# Specificity
my_test_that("Cache remains unchanged if younger than explicit lifespan", {
  expect_equal(0, countCacheItems())
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_false(file_test("-f", fp))
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  expect_true(file_test("-f", fp))
  t0 = file.info(fp)$mtime
  mySimpleCache("testDF", lifespan=0.5, instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  t1 = file.info(fp)$mtime
  expect_true(t1 == t0)
})

# Sensitivity
my_test_that("Cache is replaced if older than explicit lifespan", {
  expect_equal(0, countCacheItems())
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_false(file_test("-f", fp))
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  expect_true(file_test("-f", fp))
  t0 = file.info(fp)$mtime
  Sys.sleep(1)    # Time difference comparison reliability.
  mySimpleCache("testDF", lifespan=0, instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  t1 = file.info(fp)$mtime
  expect_true(t1 > t0)
})

# Explicit recreate argument trumps cache lifespan to determine recreation.
my_test_that("Cache is replaced if recreate=TRUE even if cache is fresh", {
  expect_equal(0, countCacheItems())
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_false(file_test("-f", fp))
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  expect_true(file_test("-f", fp))
  expect_equal(1, countCacheItems())
  t0 = file.info(fp)$mtime
  Sys.sleep(1)    # Time difference comparison reliability.
  mySimpleCache("testDF", recreate=TRUE, lifespan=0, instruction={ buildTestFrame() })
  expect_equal(1, countCacheItems())
  t1 = file.info(fp)$mtime
  expect_true(t1 > t0)
})

my_test_that("simpleCache can pick up option specifying max cache age.", {
  options(MAX.CACHE.AGE = 0)
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_false(file_test("-f", fp))
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  expect_true(file_test("-f", fp))
  t0 = file.info(fp)$mtime
  Sys.sleep(1)    # Time difference comparison reliability.
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  t1 = file.info(fp)$mtime
  expect_true(t1 > t0)
})

my_test_that("Direct lifespan specification is preferred to background option", {
  options(MAX.CACHE.AGE = 1)
  fp = file.path(getOption("RCACHE.DIR"), "testDF.RData")
  expect_false(file_test("-f", fp))
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  expect_true(file_test("-f", fp))
  t0 = file.info(fp)$mtime
  Sys.sleep(1)
  mySimpleCache("testDF", instruction={ buildTestFrame() })
  expect_equal(t0, file.info(fp)$ctime)    # Cache is fresh via MAX.CACHE.AGE.
  Sys.sleep(1)    # Time difference comparison reliability.
  mySimpleCache("testDF", lifespan=0, instruction={ buildTestFrame() })
  t1 = file.info(fp)$mtime
  expect_true(t1 > t0)    # Cache is stale via lifespan.
})
