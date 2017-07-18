## Package documentation
#' Provides intuitive functions for caching R objects, encouraging faster
#' reproducible and restartable R analysis
#'
#' simpleCache provides a function (simpleCache())
#' 
#' @references \url{https://github.com/nsheff/}
## @import if you import any packages; here.
#' @import data.table
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
#' You should pass a bracketed R code snippet like `{ rnorm(500) }` as the
#' instruction, and simpleCache will create the object. Alternatively, if the
#' code to create the cache is large, you can put an R script called object.R in
#' the RBUILD.DIR (the name of the file *must* match the name of the object it
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
#'			Defaults to the global RCACHE.DIR variable
#' @param cacheSubDir You can specify a subdirectory within the cacheDir
#' 			variable. Defaults to NULL.
#' @param assignToVariable By default, simpleCache assigns the cache to a
#'     variable named cacheName; you can overrule that here.
#' @param loadEnvir    Into which environment would you like to load the
#'     variable? Defaults to parent.frame.
#' @param searchEnvir  a vector of environments to search for the already loaded
#'     cache.
#' @param timer Report how long it took to create the cache?
#' @param buildDir Location of Build files (files with instructions for use If
#'		the instructions argument is not provided). Defaults to RBUILD.DIR
#'		global option.
#' @param parse By default, simpleCache will guess whether you want to parse the
#'     instruction, based on whether it is quoted. You can overwrite the guess
#'     with this parameter; but this may disappear in the future. In general,
#'     you should note quote, but use {} around your instructions.
#' @param nofail By default, simpleCache throws an error if the instructions
#'     fail. Use this option to convert this error into a warning. No cache will
#'     be created, but simpleCache will not then hard-stop your processing. This
#'     is useful, for example, if you are creating a bunch of caches and it's ok
#'     if some of them do not complete.
#' @param batchRegistry A batchtools registry object (built with
#'     batchtools::makeRegistry()). If provided, this cache will be created on
#'     the cluster using your batchtools configuration
#' @param batchResources A list of variables to provide to batchtools for
#'     cluster resource managers. Used as the `res` argument to
#'     batchtools::batchMap()
#' @param pepSettings Experimental untested feature.
#' @param  ignoreLock   internal parameter used for batch job submission; don't
#'     touch.
#' @export

simpleCache = function(cacheName, instruction=NULL, buildEnvir=NULL,
	reload=FALSE, recreate=FALSE, noload=FALSE,
	cacheDir=getOption("RCACHE.DIR"), cacheSubDir=NULL, timer=FALSE,
	buildDir=getOption("RBUILD.DIR"), assignToVariable=NULL,
	loadEnvir=parent.frame(), searchEnvir=getOption("SIMPLECACHE.ENV"),
	slurmParams=NULL, parse=NULL, nofail=FALSE, batchRegistry=NULL,
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
		message(strwrap("You must set global option RCACHE.DIR with setSharedCacheDir(),
		or specify a cacheDir parameter directly to simpleCache()."))
		return(NA)
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
			message(paste(batchtools::getLog(1, reg=registry), collapse="\n"))
		}
		if (!is.null(pepSettings)) { 
			# TODO: retrieve log
			slurmLog = file.path(slurmParams$hpcFolder, paste0(cacheName, ".log"))
			message(slurmLog)
			utils::tail(readLines(slurmLog), 10) 
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
						error("Install batchtools for cluster submission...")
					} 
					if (is.null(batchResources)) { 
						error("You must provide both batchRegistry and batchResources.")
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
					error("PEP settings submission is not yet implemented")
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
						ret = eval(parse(text=instruction))
					} else {
						ret = eval( instruction )
					}
				}
				if (timer) { toc() }
			} else {
				# Build environment was provided.
				if (timer) { tic() }
				if (parse) {
					ret = with(buildEnvir, eval(parse(text=instruction)))
				} else {
					ret = with(buildEnvir, eval(instruction))
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


#' Create or load a cache from the web.
#'
#' Given a URL, this function will download the file and cache it, or load it if it has been previously downloaded.
#' BETA -- not finished.
#' TODO -- update with searchEnvir feature of simpleCache.
#'
#' @param object Name of cache.
#' @param url Web location of the text file to cache.
#' @param env See documentation at simpleCache()
#' @param reload See documentation at simpleCache()
#' @param recreate See documentation at simpleCache()
#' @param noload See documentation at simpleCache()
#' @param cacheDir See documentation at simpleCache()
#' @param cacheSubDir See documentation at simpleCache()
#' @param loadEnvir See documentation at simpleCache()
#' @param assignToVariable See documentation at simpleCache()
#' @export
downloadCache = function(object, url, env=NULL, reload=FALSE, recreate=FALSE,
	noload=FALSE, cacheDir=getOption("RCACHE.DIR"), cacheSubDir="download",
	loadEnvir=parent.frame(), assignToVariable=NULL) {

	# downloadCache will use data.table's fread function for reading
	# in downloads:
	requireNamespace("data.table")
	
	# Deal with path joins and ensure that the target cache directory exists.
	if(!is.null(cacheSubDir)) {
		cacheDir = file.path(cacheDir, cacheSubDir)
	}
	if (is.null(cacheDir)) {
		message(strwrap("You must set global option(RCACHE.DIR) or specify a
		local cacheDir parameter."))
		return(NA)
	}

	if (!file.exists(cacheDir)) {
		dir.create(cacheDir)
	}

	cacheFile = file.path(cacheDir, object)
	message("Cache file: ", cacheFile)

	if(exists(object) & !reload & !recreate) {
		message("::Object exists::\t", object)
		return(get(object))
	} else if(file.exists(cacheFile) & !recreate & !noload) {
		message("::Loading cache::\t", cacheFile)
		ret = data.table::fread(cacheFile)
	} else if(file.exists(cacheFile) & !recreate) {
		message("::Cache exists (no load)::\t", cacheFile)
		return (NULL)
	} else {
		message(":Creating cache::\t", cacheFile)
		if(is.null(url)) {
			message(strwrap("You must set global option(RBUILD.DIR) or specify a
			local buildDir parameter (or provide an instruction argument)."))
				return(NA)
		} else {
			#"ret," for return, is the name the object is stored under.
			command = paste0("wget -O ", cacheFile, " '", url, "'")
			sysResult = system(command, intern=TRUE)
			message(command, sysResult)
			ret = data.table::fread(cacheFile)
		}
	}

	if (noload) { 
		rm(ret)
		gc()
		return()
	}
	if(is.null(assignToVariable)) { 
		assignToVariable=object
	}
	assign(assignToVariable, ret, envir=loadEnvir)
	return() #used to return ret, but not any more
}


