simpleCache: R caching for restartable analysis
-----------------------------------------------

<a href="https://travis-ci.org/vpnagraj/simpleCache"><img src="https://travis-ci.org/vpnagraj/simpleCache.svg?branch=master" alt="Travis CI status"></img>
</a>

`simpleCache` is an R package providing functions for caching R objects. Its purpose is to encourage writing reusable, restartable, and reproducible analysis pipelines for projects with massive data and computational requirements.

Like its name indicates, `simpleCache` is intended to be simple. You choose a location to store your caches, and then provide the function with nothing more than a cache name and instructions (R code) for how to produce the R object. While simple, `simpleCache` also provides some advanced options like environment assignments, recreating caches, reloading caches, and even cluster compute bindings (to SLURM) making it flexible enough for use in large-scale data analysis projects.

--------------------------------------------------------------------------------
### Installing simpleCache
Install the development version directly from github with devtools

```
require(devtools)
install_github("nsheff/simpleCache") #public
```

To install a local copy:
```
packageFolder = "~/Rn/simpleCache";
install.packages(packageFolder, repos=NULL)
```

--------------------------------------------------------------------------------
### Running simpleCache

`simpleCache` comes with a single primary function that will do almost everything you need. I have produced some [R vignettes](vignettes/) to get you started. 

* [An introduction to simpleCache](vignettes/simpleCacheIntroduction.Rmd)
* [Sharing caches across projects](vignettes/sharingCaches.Rmd)

--------------------------------------------------------------------------------
### simpleCache Philosophy

The use case I had in mind for `simpleCache` is that you find yourself constantly recalculating the same R object in several different scripts, or repeatedly in the same script, every time you open it and want to continue that project. SimpleCache is well-suited for interactive analysis, allowing you to pick up right where you left off in a new R session, without having to recalculate everything. It is equally useful in automatic pipelines, where separate scripts may benefit from loading, instead of recalculating, the same R objects produced by other scripts.

R provides some base functions (`save`, `serialize`, and `load`) to let you save and reload such objects, but these low-level functions are a bit cumbersome. `simpleCache` simply provides a convenient, user-friendly interface to these functions, streamlining the process. For example, a single `simpleCache` call will check for a cache and load it if it exists, or create it if it does not. With the base R `save` and `load` functions, you can't just write a single function call and then run the same thing every time you start the script -- even this simple use case requires additional logic to check for an existing cache. SimpleCache just does all this for you.

They thing to keep in mind with simpleCache is that **the cache name is paramount**. SimpleCache assumes that your name for an object is a perfect identifier for that object; in other words, don't cache things that you plan to change.













