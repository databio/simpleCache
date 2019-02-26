# Change log
All notable changes to this project will be documented in this file.

## simpleCache [0.4.1] -- 2019-02-26

	- fixes unit tests on windows
	- fixes lifespan bug that used creation time instead of modification time
	- allow arg-less directory setting to default to current working dir

## simpleCache [0.4.0] -- 2017-12-20

	- adds a lifespan arg to simpleCache() to create auto-expiring caches
	- remove unnecessary parse argument to simpleCache()
	- viewCacheDirs() renamed to simpleCacheOptions()
	
## simpleCache [0.3.1] -- 2017-08-21

	- fixed a bug in unit tests that left behind a test cache in user home dir.
	- changes cache building to happen in parent.frame()
	- repaired vignette so R code is displayed properly
	- added deleteCaches() function and docs
	- reduced size of unit test cache for speed increase

## simpleCache [0.3.0] -- 2017-08-21

	- switched default cache dir to tempdir()
	- changed availCaches() to listCaches()
	- changes cache building to happen in globalenv(), so that any loaded
	  packages are available for cache building


## simpleCache [0.2.1] -- 2017-07-30

	- added examples

## simpleCache [0.2.0] -- 2017-07-30

	- support for batchjobs parallel processing
	- docs, prep for submission to CRAN

## simpleCache [0.0.1]

	- long-term stable version
