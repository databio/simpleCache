# choose location to store caches
cacheDir = system.file("cache", package="simpleCache")
cacheDir
setCacheDir(cacheDir)

# build some caches
simpleCache("normSample", { rnorm(5e3, 0,1) }, recreate=TRUE, timer=TRUE)
simpleCache("normSample", { rnorm(5e3, 0,1) })
simpleCache("normSample", { rnorm(5e3, 0,1) }, reload=TRUE)

# storing a cache after-the-fact
normSample2 = rnorm(10, 0, 1)
storeCache("normSample2")

# what's available?
listCaches()

# load a cache
simpleCache("normSample")

# load multiples caches
loadCaches(c("normSample", "normSample2"), reload=TRUE)
