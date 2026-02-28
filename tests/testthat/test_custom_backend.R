library(simpleCache)

context("custom backend")

rds_backend = list(
  save = function(obj, file) saveRDS(obj, file),
  load = function(file) readRDS(file),
  ext  = ".rds"
)

test_that("default backend produces .RData files", {
  td = tempfile("cache_"); dir.create(td)
  setCacheDir(td)
  on.exit({ options(RCACHE.BACKEND = NULL); unlink(td, recursive=TRUE) })

  simpleCache("defTest", { 42 }, recreate=TRUE)
  expect_true(file.exists(file.path(td, "defTest.RData")))
  expect_equal(defTest, 42)
  deleteCaches("defTest", force=TRUE)
})

test_that("custom backend round-trip, list, store, and delete", {
  td = tempfile("cache_"); dir.create(td)
  setCacheDir(td)
  options(RCACHE.BACKEND = rds_backend)
  on.exit({ options(RCACHE.BACKEND = NULL); unlink(td, recursive=TRUE) })

  # Create and verify
  simpleCache("rdsTest", { 1:10 }, recreate=TRUE)
  expect_true(file.exists(file.path(td, "rdsTest.rds")))
  expect_equal(rdsTest, 1:10)

  # Reload from disk
  rm(rdsTest, envir=.GlobalEnv)
  simpleCache("rdsTest", reload=TRUE)
  expect_equal(rdsTest, 1:10)

  # storeCache
  storeObj <<- list(a=1)
  storeCache("storeObj", cacheDir=td, recreate=TRUE)
  expect_true(file.exists(file.path(td, "storeObj.rds")))

  # listCaches sees only current backend
  caches = listCaches()
  expect_true("rdsTest.rds" %in% caches)

  # deleteCaches removes correct file
  deleteCaches("rdsTest", force=TRUE)
  expect_false(file.exists(file.path(td, "rdsTest.rds")))
  deleteCaches("storeObj", force=TRUE)
})

test_that("invalid backend and ext normalization", {
  on.exit(options(RCACHE.BACKEND = NULL))

  options(RCACHE.BACKEND = list(save=identity, load=identity))
  expect_error(.getBackend(), "must contain")

  options(RCACHE.BACKEND = list(save="x", load=identity, ext=".x"))
  expect_error(.getBackend(), "must be functions")

  options(RCACHE.BACKEND = list(save=identity, load=identity, ext="rds"))
  expect_equal(.getBackend()$ext, ".rds")
})
