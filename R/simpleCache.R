## Package documentation
#' Provides intuitive functions for caching R objects, encouraging faster
#' reproducible and restartable R analysis
#' 
#' @references \url{https://github.com/databio/simpleCache}
#' @docType package
#' @name simpleCache
#' @author Nathan Sheffield
NULL

################################################################################

#' Create a new cache or load a previously created cache.
#'
#' Given a unique name for an  R object, and instructions for how to make that
#' object, use the simpleCache function to create and cache or load the object.
#' This should be used for computations that take a long time and generate a
#' table or something used repeatedly (in other scripts, for example). Because
#' the cache is tied to the object name, there is some danger of causing
#' troubles if you misuse the caching system. The object should be considered
#' static. 
#' 
#' You should pass a bracketed R code snippet like \code{rnorm(500)} as the
#' instruction, and simpleCache will create the object. Alternatively, if the
#' code to create the cache is large, you can put an R script called object.R in
#' the \code{\link[=setCacheBuildDir]{RBUILD.DIR}} (the name of the file *must* match the name of the object it
#' creates *exactly*). If you don't provide an instruction, the function sources

#' RBUILD.DIR/object.R and caches the result as the object. This source file
#' *must* create an object with the same name of the object. If you already have
#' an object with the name of the object to load in your current environment,
#' this function will not try to reload the object; instead, it returns the
#' local object. In essence, it assumes that this is a static object, which you
#' will not change. You can force it to load the cached version instead with
#' "reload".
#' 
#' Because R uses lexical scope and not dynamic scope, you may need to pass some
#' environment variables you use in your instruction code. You can use this
#' using the parameter buildEnvir (just provide a list of named variables).
#' 
#' @param cacheName	Unique name for the cache. Be careful.
#' @param instruction  Quoted R code to be evaluated. The returned value of this
#'     code is what will be cached under the cacheName.
#' @param buildEnvir   You may choose to provide additional variables necessary
#'     for evaluating the code in instruction.
#' @param reload   forces re-loading the cache, even if it exists in the env.
#' @param recreate forces reconstruction of the cache
#' @param noload   noload is useful for: you want to create the caches, but not
#'     load them if they aren't there (like a cache creation loop).
#' @param cacheDir The directory where caches are saved (and loaded from).
#'			Defaults to the global \code{\link[=setCacheDir]{RCACHE.DIR}} variable
#' @param cacheSubDir You can specify a subdirectory within the \code{cacheDir}
#' 			variable. Defaults to \code{NULL}.
#' @param assignToVariable By default, \code{simpleCache} assigns the cache to a
#'     variable named \code{cacheName}; you can overrule that here.
#' @param loadEnvir    Into which environment would you like to load the
#'     variable? Defaults to \code{\link[base]{parent.frame}}.
#' @param searchEnvir  a vector of environments to search for the already loaded
#'     cache.
#' @param timer Report how long it took to create the cache?
#' @param buildDir Location of Build files (files with instructions for use If
#'		the instructions argument is not provided). Defaults to \code{\link[=setCacheBuildDir]{RBUILD.DIR}} global option.
#' @param parse By default, \code{simpleCache} will guess whether you want to parse the
#'     instruction, based on whether it is quoted. You can overwrite the guess
#'     with this parameter; but this may disappear in the future. In general,
#'     you should note quote, but use \{ \} around your instructions.
#' @param nofail By default, \code{simpleCache} throws an error if the instructions
#'     fail. Use this option to convert this error into a warning. No cache will
#'     be created, but simpleCache will not then hard-stop your processing. This
#'     is useful, for example, if you are creating a bunch of caches and it's ok
#'     if some of them do not complete.
#' @param batchRegistry A \code{batchtools} registry object (built with
#'      \code{\link[batchtools]{makeRegistry}}). If provided, this cache will be created on
#'     the cluster using your batchtools configuration
#' @param batchResources A list of variables to provide to batchtools for
#'     cluster resource managers. Used as the \code{res} argument to
#'     \code{\link[batchtools]{batchMap}}
#' @param pepSettings Experimental untested feature.
#' @param  ignoreLock   internal parameter used for batch job submission; don't
#'     touch.
#' @export
#' @example
#' R/examples/example.R
simpleCache = function(cacheName, instruction=NULL, buildEnvir=NULL,
	reload=FALSE, recreate=FALSE, noload=FALSE,
	cacheDir=getOption("RCACHE.DIR"), cacheSubDir=NULL, timer=FALSE,
	buildDir=getOption("RBUILD.DIR"), assignToVariable=NULL,
	loadEnvir=parent.frame(), searchEnvir=getOption("SIMPLECACHE.ENV"),
	parse=NULL, nofail=FALSE, batchRegistry=NULL,
	batchResources=NULL, pepSettings=NULL, ignoreLock=FALSE) {

	# Because R evaluates arguments lazily (only when they are used),
	# it will not evaluate the instruction if I first wrap it in a
	# primitive substitute call. Then I can evaluate conditionally
	# (if the cache needs to be recreated)
	instruction = substitute(instruction)
	if (is.null(parse)) {
		if ("character" %in% class(instruction)) {

			parse = TRUE
			warning(strwrap("Detected a character instruction; consider wrapping
			in {} instead of quotes."))
		} else {
			parse = FALSE
		}
	}
	if (!is.null(cacheSubDir)) {
		cacheDir = file.path(cacheDir, cacheSubDir)
	}
	if (is.null(cacheDir)) {
		message(strwrap("No cacheDir specified. You should set global option
		RCACHE.DIR with setCacheDir(), or specify a cacheDir parameter directly
		to simpleCache(). With no other option, simpleCache will use tempdir():
		", initial="", prefix=" "), tempdir())
		cacheDir = tempdir()
	}
	if (!"character" %in% class(cacheName)) {
		stop("simpleCache expects the cacheName variable to be a character
		vector.")
	}
	
	if (!file.exists(cacheDir)) {
		dir.create(cacheDir, recursive=TRUE)
	}
	cacheFile = file.path(cacheDir, paste0(cacheName, ".RData"))
	lockFile = file.path(cacheDir, paste0(cacheName, ".lock"))
	if (ignoreLock) {
		# remove the lock file when this function call is complete.
		on.exit(file.remove(lockFile))
	}
	submitted = FALSE
	# Check if cache exists in any provided search environment.
	searchEnvir = append(searchEnvir, ".GlobalEnv")  # Assume global env.
	cacheExists = FALSE
	cacheWhere = NULL

	for ( curEnv in searchEnvir ) {
		if(exists(cacheName, where=get(curEnv))) {
			cacheExists = TRUE
			cacheWhere = curEnv
			break
		}
	}
	

	ret = NULL # The default, in case the cache construction fails.

	if(cacheExists & !reload & !recreate) {
		message("::Object exists (in ", cacheWhere, ")::\t", cacheName)
		#return(get(cacheName))
		#return()
		ret = get(cacheName, pos = get(cacheWhere))
	} else if (file.exists(lockFile) & !ignoreLock) {
		message("::Cache processing (lock file exists)::\t", lockFile)
		#check for slurm log...

		if (!is.null(batchRegistry)) {
			# Grabbing log from batchtools
			# 1 is the job id.
			message(paste(batchtools::getLog(1, reg=batchRegistry), collapse="\n"))
		}
		if (!is.null(pepSettings)) { 
			# TODO: retrieve log
			stop("PEP settings submission is not yet implemented")
			
		}

		return()

	} else if(file.exists(cacheFile) & !recreate & !noload) {
		message("::Loading cache::\t", cacheFile)
		load(cacheFile)
	} else if(file.exists(cacheFile) & !recreate) {
		message("::Cache exists (no load)::\t", cacheFile)
		return(NULL)
	} else {
		message("::Creating cache::\t", cacheFile)

		tryCatch( { # Intercept any errors with creating this cache.

		if(is.null(instruction)) {
				if (is.null(buildDir)) {
					stop(strwrap("::Error::\tIf you do not provide an
					instruction argument, you must set global option RBUILD.DIR
					with setCacheBuildDir, or specify a buildDir parameter
					directly to simpleCache()."))
				}
				RBuildFile = file.path(buildDir, paste0(cacheName, ".R"))

				if (!file.exists(RBuildFile)) {
					stop("::Error::\tNo instruction or RBuild file provided.")
				}

				if (timer) { tic() }
				source(file.path(buildDir, paste0(cacheName, ".R")), local=FALSE)
				if (timer) { toc() }
				ret = get(cacheName)
		} else {
			
			if (is.null(buildEnvir)) {
				if (timer) { tic() }
				if ( ! is.null(batchRegistry) ) {
					# Submit to cluster using batchtools 

					 if (! requireNamespace("batchtools", quietly=TRUE)) {
						stop("Install batchtools for cluster submission...")
					} 
					if (is.null(batchResources)) { 
						stop("You must provide both batchRegistry and batchResources.")
					}
					message("Submitting job to cluster")
					# You have to wrap `instruction` in substitute() so it won't be evaluated,
					# then you have to wrap that in list so it won't be misinterpreted
					# by batchMap as multiple arguments, causing extra jobs.
					args = list(cacheName=cacheName,
						instruction=list(substitute(instruction)),
						cacheDir=cacheDir, ignoreLock=TRUE)

						ids = batchtools::batchMap(
												fun=simpleCache, 
												args=args,
												reg=batchRegistry)

					# lock cache so it won't be loaded prematurely or double-written
					file.create(lockFile)
  					batchtools::submitJobs(ids=ids, reg=batchRegistry, res=batchResources)

					message("Done submitting to cluster")
					submitted = "batch"
				} else if ( ! is.null(pepSettings) ) {
					stop("PEP settings submission is not yet implemented")
					# Build a simpleCache command
					#simpleCacheCode = paste0("simpleCache('", cacheName, "',
						# instruction='", paste0(deparse(instruction), collapse="\n"), "',
						# recreate=", recreate, ", 
						# cacheDir='", cacheDir,"',
						# ignoreLock=TRUE)")
					#if (slurmParams$jobName=="test") { slurmParams$jobName=cacheName } 
					#with(slurmParams, buildSlurmScript(
						# simpleCacheCode, preamble, submit, hpcFolder, 
						# jobName, mem, cores, partition, timeLimit, sourceProjectInit))
				} else {
					# No cluster submission request, so just run it here!
					# "ret," for return, is the name the cacheName is stored under.
					if (parse) {
						ret = eval(parse(text=instruction), envir=parent.frame())
					} else {
						# Here we do the evaluation in the parent frame so that 
						# it will have access to any packages the user has loaded
						# that may be required to run the code. Otherwise, it will
						# run in the simpleCache namespace which could lack these
						# packages (or have a different search path hierarchy),
						# leading to failures. The `substitute` call here ensures
						# the code isn't evaluated at argument stage, but is retained
						# until it makes it to the `eval` call.
						ret = eval(instruction, envir=parent.frame())
					}
				}
				if (timer) { toc() }
			} else {
				# Build environment was provided.
				# we must place the instruction in the environment to build from
				if (exists("instruction", buildEnvir)) {
					stop("Can't provide a variable named 'instruction' in buildEnvir")
				}
				buildEnvir$instruction = instruction
				be = as.environment(buildEnvir)
				# As described above, this puts global package functions into 
				# scope so instructions can use them.
				parent.env(be) = parent.frame()
				if (timer) { tic() }
				if (parse) {
					ret = with(be, eval(parse(text=instruction)))
				} else {
					#ret = with(buildEnvir, evalq(instruction))
					ret = with(be, eval(instruction))

				}
				if (timer) { toc() }
			}
		}

		# tryCatch
		}, error = function(e) { if (nofail) warning(e) else stop(e) })

		if (submitted == "batch") {
			message("Job submitted, check for cache.")
			return()
		} else if (is.null(ret)) {
			message("NULL value returned, no cache created")
			return() #so we don't assign NULL to the object.
		} else {
			save(ret, file=cacheFile)
		}
	}
	if (noload) {
		rm(ret)
		gc()
		return()
	}
	if(is.null(assignToVariable)) {
		assignToVariable = cacheName
	}
	assign(assignToVariable, ret, envir=loadEnvir)
	
	#return() #used to return ret, but not any more
}
