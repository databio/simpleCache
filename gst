[1mdiff --git a/R/examples/example.R b/R/examples/example.R[m
[1mindex b983605..c15a7ec 100644[m
[1m--- a/R/examples/example.R[m
[1m+++ b/R/examples/example.R[m
[36m@@ -1,5 +1,5 @@[m
 # choose location to store caches[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 cacheDir[m
 setCacheDir(cacheDir)[m
 [m
[1mdiff --git a/R/simpleCache.R b/R/simpleCache.R[m
[1mindex 21c52a4..5e91a9a 100755[m
[1m--- a/R/simpleCache.R[m
[1m+++ b/R/simpleCache.R[m
[36m@@ -112,9 +112,11 @@[m [msimpleCache = function(cacheName, instruction=NULL, buildEnvir=NULL,[m
 		cacheDir = file.path(cacheDir, cacheSubDir)[m
 	}[m
 	if (is.null(cacheDir)) {[m
[31m-		message(strwrap("You must set global option RCACHE.DIR with setSharedCacheDir(),[m
[31m-		or specify a cacheDir parameter directly to simpleCache()."))[m
[31m-		return(NA)[m
[32m+[m		[32mmessage(strwrap("No cacheDir specified. You should set global option[m
[32m+[m		[32mRCACHE.DIR with setCacheDir(), or specify a cacheDir parameter directly[m
[32m+[m		[32mto simpleCache(). With no other option, simpleCache will use tempdir():[m
[32m+[m		[32m", initial="", prefix=" "), tempdir())[m
[32m+[m		[32mcacheDir = tempdir()[m
 	}[m
 	if (!"character" %in% class(cacheName)) {[m
 		stop("simpleCache expects the cacheName variable to be a character[m
[1mdiff --git a/man/listCaches.Rd b/man/listCaches.Rd[m
[1mindex 04fca4a..afc1101 100644[m
[1m--- a/man/listCaches.Rd[m
[1m+++ b/man/listCaches.Rd[m
[36m@@ -18,7 +18,7 @@[m [mLists any cache files in the cache directory.[m
 }[m
 \examples{[m
 # choose location to store caches[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 cacheDir[m
 setCacheDir(cacheDir)[m
 [m
[1mdiff --git a/man/loadCaches.Rd b/man/loadCaches.Rd[m
[1mindex af50ae8..b247e1b 100644[m
[1m--- a/man/loadCaches.Rd[m
[1m+++ b/man/loadCaches.Rd[m
[36m@@ -17,7 +17,7 @@[m [mfor stuff you already cached previously, so it won't build any caches.[m
 }[m
 \examples{[m
 # choose location to store caches[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 cacheDir[m
 setCacheDir(cacheDir)[m
 [m
[1mdiff --git a/man/setCacheDir.Rd b/man/setCacheDir.Rd[m
[1mindex e52ecd8..de719b4 100644[m
[1m--- a/man/setCacheDir.Rd[m
[1m+++ b/man/setCacheDir.Rd[m
[36m@@ -16,7 +16,7 @@[m [msimpleCache() calls.[m
 }[m
 \examples{[m
 # choose location to store caches[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 cacheDir[m
 setCacheDir(cacheDir)[m
 [m
[1mdiff --git a/man/simpleCache.Rd b/man/simpleCache.Rd[m
[1mindex d8fb792..b85688b 100644[m
[1m--- a/man/simpleCache.Rd[m
[1m+++ b/man/simpleCache.Rd[m
[36m@@ -108,7 +108,7 @@[m [musing the parameter buildEnvir (just provide a list of named variables).[m
 }[m
 \examples{[m
 # choose location to store caches[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 cacheDir[m
 setCacheDir(cacheDir)[m
 [m
[1mdiff --git a/man/storeCache.Rd b/man/storeCache.Rd[m
[1mindex dfe1f3e..76b5a66 100644[m
[1m--- a/man/storeCache.Rd[m
[1m+++ b/man/storeCache.Rd[m
[36m@@ -32,7 +32,7 @@[m [mstoreCache at the end.[m
 }[m
 \examples{[m
 # choose location to store caches[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 cacheDir[m
 setCacheDir(cacheDir)[m
 [m
[1mdiff --git a/vignettes/clusterCaches.Rmd b/vignettes/clusterCaches.Rmd[m
[1mindex 1475fd0..0accc90 100644[m
[1m--- a/vignettes/clusterCaches.Rmd[m
[1m+++ b/vignettes/clusterCaches.Rmd[m
[36m@@ -18,7 +18,7 @@[m [mTo do this, first, create a `batchtools` registry. You can follow more detailed[m
 [m
 ```{r Try it out, eval=FALSE}[m
 library(simpleCache)[m
[31m-setCacheDir(getwd())[m
[32m+[m[32msetCacheDir(tempdir())[m
 [m
 registry = batchtools::makeRegistry(NA)[m
 templateFile = system.file("templates/slurm-advanced.tmpl", package = "simpleCache")[m
[1mdiff --git a/vignettes/sharingCaches.Rmd b/vignettes/sharingCaches.Rmd[m
[1mindex 569d87c..8b64414 100644[m
[1m--- a/vignettes/sharingCaches.Rmd[m
[1m+++ b/vignettes/sharingCaches.Rmd[m
[36m@@ -16,7 +16,7 @@[m [mTo solve this problem, `simpleCache` uses a second global option, `SHARE.RCACHE.[m
 [m
 ```{r Try it out}[m
 library(simpleCache)[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 setSharedCacheDir(cacheDir)[m
 simpleCacheShared("normSample", "rnorm(1e7, 0,1)", recreate=TRUE)[m
 simpleCacheShared("normSample", "rnorm(1e7, 0,1)")[m
[1mdiff --git a/vignettes/simpleCacheIntroduction.Rmd b/vignettes/simpleCacheIntroduction.Rmd[m
[1mindex f39ec0d..e32d8fc 100644[m
[1m--- a/vignettes/simpleCacheIntroduction.Rmd[m
[1m+++ b/vignettes/simpleCacheIntroduction.Rmd[m
[36m@@ -20,7 +20,7 @@[m [mBut before we start creating caches, it's important to tell `simpleCache` where[m
 [m
 ```{r Try it out}[m
 library(simpleCache)[m
[31m-cacheDir = system.file("cache", package="simpleCache")[m
[32m+[m[32mcacheDir = tempdir()[m
 setCacheDir(cacheDir)[m
 simpleCache("normSample", { rnorm(1e7, 0,1) })[m
 ```[m
