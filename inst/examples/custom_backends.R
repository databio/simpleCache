# Custom serialization backends for simpleCache
#
# simpleCache supports pluggable serialization via the RCACHE.BACKEND option.
# Set it to a list with save, load, and ext elements. When NULL (the default),
# simpleCache uses base R save()/load() with .RData files.

library(simpleCache)
setCacheDir(tempdir())

# --- Example 1: qs2 (fast, compact) ---
if (requireNamespace("qs2", quietly = TRUE)) {
  options(RCACHE.BACKEND = list(
    save = function(obj, file) qs2::qs_save(obj, file),
    load = function(file) qs2::qs_read(file),
    ext  = ".qs2"
  ))

  simpleCache("qs2_demo", { rnorm(1e4) }, recreate = TRUE)
  message("qs2 cache: ", file.path(tempdir(), "qs2_demo.qs2"))
  deleteCaches("qs2_demo", force = TRUE)
  options(RCACHE.BACKEND = NULL)
} else {
  message("Skipping qs2 example (qs2 not installed)")
}

# --- Example 2: saveRDS/readRDS (single-object RDS format) ---
options(RCACHE.BACKEND = list(
  save = function(obj, file) saveRDS(obj, file),
  load = function(file) readRDS(file),
  ext  = ".rds"
))

simpleCache("rds_demo", { rnorm(1e4) }, recreate = TRUE)
message("rds cache: ", file.path(tempdir(), "rds_demo.rds"))
deleteCaches("rds_demo", force = TRUE)
options(RCACHE.BACKEND = NULL)
