################################################################################
# UTILITY FUNCTIONS
################################################################################
# These are functions copied over from my repository of utilities used
# by this package. They are repeated here simply for portability, so this
# package can be deployed on systems without access to my utilities. 
# Any changes should probably be backported to the primary functions rather 
# than in these convenience duplications.
#
# These functions should probably remain interior to the package (not exported)
#

enforceEdgeCharacter = function(string, prependChar="", appendChar="") {
	if (string=="" | is.null(string)) {
		return(string)
	}
	if(!is.null(appendChar)) {
		if (substr(string,nchar(string), nchar(string)) != appendChar) { # +1 ?
			string = paste0(string, appendChar);
			}
	}
	if (!is.null(prependChar)) {
		if (substr(string,1,1) != prependChar) { # +1 ?
			string = paste0(prependChar, string)
		}
	}
	return(string)
}


#' Determine if a cache file is sufficiently old to warrant refresh.
#' 
#' \code{.tooOld} accepts a maximum cache age and checks for an option with 
#' that setting under \code{MAX.CACHE.AGE} if such an argument isn't passed.
#' If the indicated file exists and is older than the threshold passed or 
#' set as an option, the file is deemed "stale." If an age threshold is 
#' provided, no check for an option is performed. If the file does not 
#' exist or there's not an age threshold directly passed or set as an option, 
#' the result is \code{FALSE}.
#' 
#' @param pathCacheFile Path to file to ask about staleness.
#' @param lifespan Maximum file age before it's "stale."
#' @return \code{TRUE} if the file exists and its age exceeds 
#'         \code{lifespan} if given or 
#'         \code{getOption("MAX.CACHE.AGE")} if no age threshold is passed 
#'         and that option exists; \code{FALSE} otherwise.
.tooOld = function(pathCacheFile, lifespan=NULL) {
	if (!file_test("-f", pathCacheFile)) { return(FALSE) }
	if (is.null(lifespan)) { lifespan = getOption("MAX.CACHE.AGE") }
	if (is.null(lifespan)) { return(FALSE) }
	cacheTime = file.info(pathCacheFile)$ctime
	cacheAge = difftime(Sys.time(), cacheTime, units="days")
	as.numeric(cacheAge) > as.numeric(lifespan)
}


# MATLAB-style timing functions to start/stop timer.
# These functions were based on an idea by some helpful soul on
# Stackoverflow that I can no longer recall...

#' This function takes a time in seconds and converts it to a more
#' human-readable format, showing hours, minutes, or seconds, depending
#' on how long the time is. Used by my implementation of tic()/toc().
#' @param timeInSec	numeric value of time measured in seconds.
secToTime = function(timeInSec) {
	hr = timeInSec %/% 3600 #hours
	min = timeInSec %% 3600 %/% 60 #minutes
	sec = timeInSec %% 60 #seconds
	return(paste0(sprintf("%02d", hr), "h ", sprintf("%02d", min), "m ",
	sprintf("%02.01f", signif(sec, 3)), "s"))
}

#' Start a timer
#' @param gcFirst Garbage Collect before starting the timer?
#' @param type Type of time to return, 
#' can be 'elapsed', 'user.self', or 'sys.self'
tic = function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")) {
	type <- match.arg(type)
	assign(".type_simpleCache", type, envir=baseenv())
	if(gcFirst) gc(FALSE)
	tic <- proc.time()[type]         
	assign(".tic_simpleCache", tic, envir=baseenv())
	invisible(tic)
}

#' Check the time since the current timer was started with tic()
toc = function() {
	type <- get(".type_simpleCache", envir=baseenv())
	toc <- proc.time()[type]
	tic <- get(".tic_simpleCache", envir=baseenv())
	timeInSec = as.numeric(toc-tic);
	message("<", secToTime(timeInSec), ">", appendLF=FALSE)
	invisible(toc)
}
