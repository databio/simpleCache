#PACKAGE DOCUMENTATION
#' Provides intuitive functions for caching R objects, encouraging faster reproducible and restartable R analysis 
#'
#' simpleCache provides a function (simpleCache())
#' 
#' @references \url{http://github.com/sheffien}
## @import if you import any packages; here.
#' @docType package
#' @name simpleCache
#' @author Nathan Sheffield
NULL

################################################################################
#FUNCTION DOCUMENTATION - simpleCache() main function
#' Create or load a previously created cache.
#'
#'
#' Given an R object with a unique name, and instructions for how to make
#' that object, use the simpleCache function to cache the object.
#' This should be used for computations that take a long time and generate
#' a table or something used repeatedly (in other scripts, for example).
#' Because it is tied to the object name, there is some danger of causing troubles
#' if you misuse the caching system. The object should be considered static.
#' You can pass R code that creates the object either as a string to
#' the "instruction" parameter (if the code is short), or you can put an R
#' script called object.R in the RBUILD.DIR (the name of the file *must* match
#' the name of the object it creates *exactly*). If you don't provide instruction,
#' the function sources RBUILD.DIR/object.R and caches the result as the object.
#' This source file *must* create an object with the same name of the object.
#' If you already have an object with the name of the object to load in your
#' current environment, this function will not try to reload the object; instead,
#' it returns the local object.
#'
#' In essence, it assumes that this is a static object, which you will not change.
#' You can force it to load the cached version instead with "reload"
#' Because R uses lexical scoping and not dynamic scoping...
#' because of lexical scope, you may need to pass some environment variables you use in your instruction (function call). You can use this using the parameter env (just provide a list of named variables).
#' @param cacheName	Unique name for the cache. Be careful.
#' @param instruction	Quoted R code to be evaluated. The returned value of this code is what will be cached under the cacheName.
#' @param buildEnvir	You may choose to provide additional variables necessary for evaluating the code in instruction.
#' @param reload	forces re-loading the cache, even if it exists in the env.
#' @param recreate	forces reconstruction of the cache
#' @param noload	noload is useful for: you want to create the caches, but not load them if they aren't there (like a cache creation loop).
#' @param cacheDir The directory where caches are saved (and loaded from).
#'			Defaults to the global RCACHE.DIR variable
#' @param cacheSubDir You can specify a subdirectory within the cacheDir 
#' 			variable. Defaults to NULL.
#' @param assignToVariable	By default, simpleCache assigns the cache to a variable named cacheName; you can overrule that here.
#' @param loadEnvir	Into which environment would you like to load the variable?
#' @param searchEnvir	a vector of environments to search for the already loaded cache.
#' @param	slurmParams	**EXPERIMENTAL FEATURE** a list with parameter settings for SLURM submission. By default, this is NULL, meaning the cache will be created in the current R session. If you provide a slurmParams object, simpleCache assumes you want to submit a job to the cluster instead.
#' @param	ignoreLock	 internal parameter used for slurm submission; don't touch.
#' @param timer Report how long it took to create the cache?
#' @param buildDir Location of Build files (files with instructions for use
#'		If the instructions argument is not provided). Defaults to
#'		RBUILD.DIR global option.
#' @param parse By default, simpleCache will guess whether you want to
#' parse the instruction, based on whether it is quoted. You can overwrite
#' the guess with this parameter; but this may disappear in the future. In
#' general, you should note quote, but use {} around your instructions.
#' @export
simpleCache = function(cacheName, instruction=NULL, buildEnvir=NULL, reload=FALSE, recreate=FALSE, noload=FALSE, cacheDir=getOption("RCACHE.DIR"), cacheSubDir=NULL, timer=FALSE, buildDir=getOption("RBUILD.DIR"), assignToVariable=NULL, loadEnvir=parent.frame(), searchEnvir=getOption("SIMPLECACHE.ENV"), slurmParams=NULL, ignoreLock=FALSE, parse=NULL) {
	# Because R evaluates arguments lazily (only when they are used),
	# it will not evaluate the instruction if I first wrap it in a
	# primitive substitute call. Then I can evaluate conditionally
	# (if the cache needs to be recreated)
	instruction = substitute(instruction)
	if (is.null(parse)) {
		if ("character" %in% class(instruction)) {
			parse=TRUE;
			#message("Detected a character instruction; consider wrapping in {} instead of quotes.");
		} else {
			parse=FALSE;
		}
	}
	if(!is.null(cacheSubDir)) {
		cacheDir=paste0(cacheDir, cacheSubDir);
	}
	if (is.null(cacheDir)) {
		message("You must set global option RCACHE.DIR with setSharedCacheDir(), or specify a cacheDir parameter directly to simpleCache().");
		return(NA);
	}
	if(! "character" %in% class(cacheName)) {
		stop("simpleCache expects the cacheName variable to be a character vector.");
	}

	cacheDir=enforceTrailingSlash(cacheDir);
	if (!file.exists(cacheDir)) {
		dir.create(cacheDir, recursive=TRUE);
	}
	cacheFile = paste0(cacheDir, cacheName, ".RData")
	lockFile = paste0(cacheDir, cacheName, ".lock")
	if (ignoreLock) { #otherwise use a trycatch... ?
		on.exit(file.remove(lockFile));
	}
	submitted=FALSE;
	#check if cache exists in any provided search environment.
	searchEnvir = append(searchEnvir, ".GlobalEnv");	#assume global env.
	cacheExists = FALSE; cacheWhere = NULL;
	for ( curEnv in searchEnvir ) {
		if(exists(cacheName, where=get(curEnv))) {
			cacheExists = TRUE;
			cacheWhere = curEnv;
			break;
		}
	} #for

	if(cacheExists & !reload & !recreate) {
		message("::Object exists (in ", cacheWhere, ")::\t", cacheName);
		#return(get(cacheName));
		#return();
		ret = get(cacheName, pos=get(cacheWhere));
	} else if (file.exists(lockFile) & !ignoreLock) {
		message("::Cache processing (lock file exists)::\t", lockFile)
		#check for slurm log...
		if (!is.null(slurmParams)) { slurmLog = paste0(slurmParams$hpcFolder, "/", cacheName, ".log"); message(slurmLog); tail(readLines(slurmLog), 10) }

		return ();
	} else if(file.exists(cacheFile) & !recreate & !noload) {
		message("::Loading cache::\t", cacheFile);
		load(cacheFile);
	} else if(file.exists(cacheFile) & !recreate) {
		message("::Cache exists (no load)::\t", cacheFile);
		return (NULL);
	} else {
		message("::Creating cache::\t", cacheFile);
		if(is.null(instruction)) {
				if (is.null(buildDir)) {
					stop("::Error::\tIf you do not provide an instruction argument, you must set global option RBUILD.DIR with setCacheBuildDir, or specify a buildDir parameter directly to simpleCache().");
				}
				RBuildFile = paste0(buildDir, cacheName, ".R");
				if (!file.exists(RBuildFile)) {
					stop("::Error::\tNo instruction or RBuild file provided.");
				}
				if (timer) { tic(); }
				source(paste0(buildDir, cacheName, ".R"), local=FALSE);
				if (timer) { toc(); }
				ret = get(cacheName);
		} else {
			#"ret," for return, is the name the cacheName is stored under.
			if (is.null(buildEnvir)) {
				if (timer) { tic(); }
				if (is.null(slurmParams)) {
					#tryCatch(
					if (parse) {
						ret = eval(parse(text=instruction));	
					} else {
						ret = eval( instruction )
					}
				} else {
					#submit to slurm!
					message("Submitting job to cluster");
					#Build a simpleCache command
					simpleCacheCode = paste0("simpleCache('", cacheName, "', instruction='", instruction, "', recreate=", recreate, ", ignoreLock=TRUE)");
					#lock
					if (slurmParams$jobName=="test") { slurmParams$jobName=cacheName; } #change default job name.			
					file.create(lockFile);
with(slurmParams, buildSlurmScript(simpleCacheCode, preamble, submit, hpcFolder, jobName, mem, cores, partition, timeLimit, sourceProjectInit))
#lockregion?
					submitted = "slurm";
				}
				if (timer) { toc(); }
			} else {
				if (timer) { tic(); }
				if (parse) {
					ret = with(buildEnvir, eval(parse(text=instruction)));
				} else {
					ret = with(buildEnvir, eval(instruction))
				}
				if (timer) { toc(); }
			}
		}
		if (submitted=="slurm") {
			#message("Job submitted to Slurm; check for cache.")
			return();
		} else if (is.null(ret)) {
			message("NULL value returned; no cache created");
			return(); #so we don't assign NULL to the object.
		} else {
			save(ret, file=cacheFile);
			#if (ignoreLock) {
			#	on.exit(file.remove(lockFile));
			#}
		}
	}
	if (noload) { rm(ret); gc(); return(); }
	if(is.null(assignToVariable)) { assignToVariable=cacheName; }
	assign(assignToVariable, ret, envir=loadEnvir);
	#return(); #used to return ret, but not any more
}

#' Debugging function... I can probably delete it.
#' @param instruction R code to run.
#' @export
testExec = function(instruction) {
	eval(parse(text=instruction))
}


################################################################################
# Helper aliases for common options

#' Helper alias for caching across experiments/people.
#' Just sets the cacheDir to the default SHARE directory 
#' (instead of the typical default PROJECT directory)
#' 
#' @param ... Parameters passed to simpleCache().
#' @export
simpleCacheShared = function(...) {
	simpleCache(..., cacheDir=getOption("SHARE.RCACHE.DIR"))
}

#' Helper alias for loading caches into the global environment.
#' simpleCache normally loads variables into the calling environment; this
#' ensures that the variables are loaded in the global environment.
#'
#' @param ... Parameters passed to simpleCache().
#' @export
simpleCacheGlobal = function(...) {
	simpleCache(..., loadEnvir=globalenv());
}

#' Helper alias for loading shared caches into the global environment.
#' 
#' @param ... Parameters passed to simpleCache().
#' @export
simpleCacheSharedGlobal = function(...) {
	simpleCache(..., cacheDir=getOption("SHARE.RCACHE.DIR"), loadEnvir=globalenv());
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
downloadCache = function(object, url, env=NULL, reload=FALSE, recreate=FALSE, noload=FALSE, cacheDir=getOption("RCACHE.DIR"), cacheSubDir="download", loadEnvir=parent.frame(), assignToVariable=NULL) {

	# downloadCache will use data.table's fread function for reading
	# in downloads:
	requireNamespace("data.table")
	if(!is.null(cacheSubDir)) {
		cacheDir=paste0(cacheDir, cacheSubDir);
	}
	if (is.null(cacheDir)) {
		message("You must set global option(RCACHE.DIR) or specify a local cacheDir parameter.");
		return(NA);
	}
	cacheDir=enforceTrailingSlash(cacheDir);
	if (!file.exists(cacheDir)) {
		dir.create(cacheDir);
	}
	cacheFile = paste0(cacheDir, object)

	if(exists(object) & !reload & !recreate) {
		message("::Object exists::\t", object);
		return(get(object));
	} else if(file.exists(cacheFile) & !recreate & !noload) {
		message("::Loading cache::\t", cacheFile);
		ret = data.table::fread(cacheFile);
	} else if(file.exists(cacheFile) & !recreate) {
		message("::Cache exists (no load)::\t", cacheFile);
		return (NULL);
	} else {
		message("::Creating cache::\t", cacheFile);
		if(is.null(url)) {
			message("You must set global option(RBUILD.DIR) or specify a local buildDir parameter (or provide an instruction argument).");
				return(NA);
		} else {
			#"ret," for return, is the name the object is stored under.
			command = paste0("wget -O ", cacheFile, " '", url, "'");
			sysResult = system(command, intern=TRUE);
			message(command, sysResult);
			ret = data.table::fread(cacheFile);
		}
	}
	if (noload) { rm(ret); gc(); return(); }
	if(is.null(assignToVariable)) { assignToVariable=object; }
	assign(assignToVariable, ret, envir=loadEnvir);
	return(); #used to return ret, but not any more
}


