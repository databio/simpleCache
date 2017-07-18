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
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")) {
	type <- match.arg(type)
	assign(".type_simpleCache", type, envir=baseenv())
	if(gcFirst) gc(FALSE)
	tic <- proc.time()[type]         
	assign(".tic_simpleCache", tic, envir=baseenv())
	invisible(tic)
}

#' Check the time since the current timer was started with tic()
toc <- function() {
	type <- get(".type_simpleCache", envir=baseenv())
	toc <- proc.time()[type]
	tic <- get(".tic_simpleCache", envir=baseenv())
	timeInSec = as.numeric(toc-tic);
	message("<", secToTime(timeInSec), ">", appendLF=FALSE)
	invisible(toc)
}
