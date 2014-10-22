#PACKAGE DOCUMENTATION
#' Provides intuitive functions for caching R objects, encouraging faster reproducible and restartable R analysis 
#'
#' simpleCache provides a function (simpleCache())
#' 
#' @references \url{http://github.com/sheffield}
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
#' @param buildEnv	You may choose to provide additional variables necessary for evaluating the code in instruction.
#' @param reload	forces re-loading the cache, even if it exists in the env.
#' @param recreate	forces reconstruction of the cache
#' @param noload	noload is useful for: you want to create the caches, but not load them if they aren't there (like a cache creation loop).
#' @param assignToVariable	By default, simpleCache assigns the cache to a variable named cacheName; you can overrule that here.
#' @export
simpleCache = function(cacheName, instruction=NULL, buildEnvir=NULL, reload=FALSE, recreate=FALSE, noload=FALSE, cacheDir=getOption("RCACHE.DIR"), cacheSubDir=NULL, buildDir=getOption("RBUILD.DIR"), assignToVariable=NULL, loadEnvir=parent.frame()) {
	if(!is.null(cacheSubDir)) {
		cacheDir=paste0(cacheDir, cacheSubDir);
	}
	if (is.null(cacheDir)) {
		message("You must set global option RCACHE.DIR with setSharedCacheDir(), or specify a cacheDir parameter directly to simpleCache().");
		return(NA);
	}
	cacheDir=enforceTrailingSlash(cacheDir);
	if (!file.exists(cacheDir)) {
		dir.create(cacheDir);
	}
	cacheFile = paste0(cacheDir, cacheName, ".RData")
	if(exists(cacheName) & !reload & !recreate) {
		message("::Object exists::\t", cacheName);
		#return(get(cacheName));
		#return();
		ret=get(cacheName);
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
					message("If you do not provide an instruction argument, you must set global option RBUILD.DIR with setCacheBuildDir, or specify a buildDir parameter directly to simpleCache().");
					return(NA);
				}
				source(paste0(buildDir, cacheName, ".R"), local=FALSE);
				ret = get(cacheName);
		} else {
			#"ret," for return, is the name the cacheName is stored under.
			ret = with(buildEnvir, eval(parse(text=instruction)));
		}
		if (is.null(ret)) {
			message("NULL value returned; no cache created");
		} else {
			save(ret, file=cacheFile);
		}
	}
	if (noload) { rm(ret); gc(); return(); }
	if(is.null(assignToVariable)) { assignToVariable=cacheName; }
	assign(assignToVariable, ret, envir=loadEnvir);
	#return(); #used to return ret, but not any more
}

################################################################################
# Helper aliases for common options

#' Helper alias for caching across experiments/people.
#' 
#' @export
simpleCacheShared = function(...) {
	simpleCache(..., cacheDir=getOption("SHARE.RCACHE.DIR"))
}

#' Helper alias for loading caches into the global environment.
#' 
#' @export
simpleCacheGlobal = function(...) {
	simpleCache(..., loadEnvir=globalenv());
}


#helper alias for caching with variable variable names
#Previously I used this:
#assign(variable, simpleCache(variable, instruction=paste0("assign('", variable, "', ",  instruction, ")"), ...), envir=.GlobalEnv);
#but why do I need an "assign" in the simpleCache instruction?
#varCache = function(loadCacheName, instruction, assignToVariableName=NULL, cacheEnvir=globalenv(), ...) {
#	if(is.null(assignToVariableName)) { assignToVariableName=loadCacheName; }
#	simpleCacheReturn = simpleCache(loadCacheName, instruction=instruction, ...);
#	if (is.null(simpleCacheReturn)) {
#		return(NULL); 
#	} else {
#		assign(assignToVariableName, simpleCacheReturn, envir=cacheEnvir);
#	}
#	#return(get(variable));
#}


#' Create or load a cache from the web.
#'
#' Given a URL, this function will download the file and cache it, or load it if it has been previously downloaded.
#'
#' @export
downloadCache = function(object, url, env=NULL, reload=FALSE, recreate=FALSE, noload=FALSE, cacheDir=getOption("RCACHE.DIR"), cacheSubDir="download", loadEnvir=environment()) {
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
		ret = fread(cacheFile);
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
			ret = fread(cacheFile);
		}
	}
	if (noload) { rm(ret); gc(); return(); }
	if(is.null(assignToVariable)) { assignToVariable=object; }
	assign(assignToVariable, ret, envir=loadEnvir);
	return(); #used to return ret, but not any more
}


#cache dir setter functions
#' Sets a global variable specifying the default cache directory for simpleCache() calls.
#'
#' hello
#' @export
setCacheDir = function(cacheDir) {
	options(RCACHE.DIR=cacheDir); 
}

#' Sets global variable specifying the default cache directory for simpleCacheShared() calls; this function is simply a helper alias for caching results that will be used across experiments.
#'
#' hello
#' @export
setSharedCacheDir = function(globalCacheDir) {
	options(SHARE.RCACHE.DIR=globalCacheDir); 
}
#' Sets local cache build directory with scripts for building files.
#'
#' hello
#' @export
setCacheBuildDir = function(cacheBuildDir) {
	options(RBUILD.DIR=cacheBuildDir); 
}

#' Views cache dir variables
#' @export
viewCacheDirs = function() {
	message("SHARE.RCACHE.DIR:\t", getOption("SHARE.RCACHE.DIR"))
	message("RCACHE.DIR:\t", getOption("RCACHE.DIR"))
	message("RBUILD.DIR:\t", getOption("RBUILD.DIR"))
}

################################################################################
# UTILITY FUNCTIONS
################################################################################

#check for, and fix, trailing slash. if necessary
enforceTrailingSlash = function(folder) {
	enforceEdgeCharacter(folder, appendChar="/");
}
enforceEdgeCharacter = function(string, prependChar="", appendChar="") {
	if (string=="" | is.null(string)) {
		return(string);
	}
	if(!is.null(appendChar)) {
		if (substr(string,nchar(string), nchar(string)) != appendChar) { # +1 ?
			string = paste0(string, appendChar);
			}
	}
	if (!is.null(prependChar)) {
		if (substr(string,1,1) != prependChar) { # +1 ?
			string = paste0(prependChar, string);
		}
	}
	return(string);
}



