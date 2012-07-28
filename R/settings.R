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

isLoadingProfile <- local({
	.value <- FALSE
	function(value){
		if( missing(value) ) .value
		else{
			old <- .value
			.value <<- value
			old
		}
		
	}
})

# Load package-specific roxygen settings
#' @importFrom tools md5sum
load_roxygen_profile <- local({
	.cacheMD5 <- NULL
	.cacheREG <- NULL
	function(base_path, vanilla = NA){
		
		file <- roxygen_profile(base_path)
		
		if( isTRUE(vanilla) ){
			parser.registry(registry_profile_vanilla)
			return( TRUE )
		}else if( !file.exists(file) ){
			if( identical(vanilla, FALSE) )
				stop("Could not apply roxygen profile file '", file, "': file does not exist.")
			return( FALSE )
		}
		
		# one needs to apply the package's profile
		hash <- tools::md5sum(file)
		# re-load cached registries if the profile file did not change
		if( identical(hash, .cacheMD5) ){
			parser.registry(.cacheREG)
			return( TRUE )
		}
		
		# backup old registry
		old <- parser.registry()
		# source profile
		message("Loading package profile '", file, "'")
		isLoadingProfile(TRUE)
		on.exit( isLoadingProfile(FALSE), add = TRUE)
		source(file, local = TRUE)
		# update cache
		.cacheMD5 <<- hash
		.cacheREG <<- parser.registry(list=TRUE)
		# return TRUE to specify that the registry has changed 
		TRUE
	}
})
