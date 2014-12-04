################################################################################
# UTILITY FUNCTIONS
################################################################################
#These are functions copied over from my repository of utilities that are used
#by this package. They are repeated here simply for portability, so this package
#can be deployed on systems without access to my utilities. Any changes should
#probably be reflected in the primary functions rather than in these 
#convenience duplications.
#
#These functions should probably remain interior to the package (not exported)
#


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



