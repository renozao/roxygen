# Functions to load package-specific roxygen settings
# 
# Author: Renaud Gaujoux
###############################################################################

#' @include parse-registry.R
NULL

# returns the path to a package/user roxygen profile 
roxygen_profile <- function(base_path){
	file.path(base_path, "man-roxygen", 'profile')
}

# Load package-specific roxygen settings
load_roxygen_profile <- function(file){
	
	# check that the file is there
	if( !file.exists(file) ) 
		stop("Roxygen profile file '", file, "' does not exist.")
	
	source(profile)
}
