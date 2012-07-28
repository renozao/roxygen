if (!exists("preref.parsers")) {
  # Preref parser table
  preref.parsers <- new.env(parent=emptyenv())
  # Srcref parser table
  srcref.parsers <- new.env(parent=emptyenv())
}

find_target <- function(target, registry, error=TRUE, regname=NULL){
	if( !is.character(target) ) return( target )
	
	track <- NULL
	ctarget <- target
	while( is.character(end <- registry[[ctarget]]) ){
		# update backtrack
		track <- c(track, ctarget)
		# check not already visited
		if( end %in% track ){
			stop("Circular refrence for target '", target, "'",
				if( !is.null(regname) ) paste('in', regname), 
				': ', str_c(track, collapse=' -> '))
		}
		# follow alias
		ctarget <- end
	}
	# check if really found
	if( is.null(end) ){
		if( error ){
			stop("Target '", target, "' does not exist",
					if( !is.null(regname) ) paste('in', regname), 
					'.')
		}else return( NULL )
	}
	end
}

#' Register parsers.
#'
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @export
#' @keywords internal
#' @rdname register-parser
register.preref.parser <- function(key, parser) {
  
  # verbose message when reading profile
  if( isLoadingProfile() ){
	  # resolve alias
	  action <- if( !is.null(preref.parsers[[key]]) ) "Redefining" else "Registering" 
	  message("  ", action, " preref parser '", key,"'", 
			  if( is.character(parser) ) str_c(" -> '", parser, "'") )
	  parser <- find_target(parser, preref.parsers)
  }
  
  preref.parsers[[key]] <- parser
}

#' @export
#' @rdname register-parser
register.srcref.parser <- function(key, parser) {
	
  # verbose message when reading profile
  if( isLoadingProfile() ){
    action <- if( !is.null(preref.parsers[[key]]) ) "Redefining" else "Registering" 
    message("  ", action, " srcref parser '", key,"'")
	parser <- find_target(parser, srcref.parsers)
  }
  
  srcref.parsers[[key]] <- parser
}

#' Register many parsers at once.
#'
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
#' @export
#' @keywords internal
#' @rdname register-parsers
register.preref.parsers <- function(parser, ...) {
  for (key in c(...)) {
    register.preref.parser(key, parser)
  }
}

#' @export
#' @rdname register-parsers
register.srcref.parsers <- function(parser, ...) {
  for (key in c(...)) {
    register.srcref.parser(key, parser)
  }
}

#' \code{store.parser.registry} backup/restore the parser registries
#' as/from a list.
#' 
#' @param registry list of parser registries to restore
#' @param list logical that indicates if the the original environment 
#' of each regitry should be returned (\code{FALSE}), or if these 
#' should be returned as lists.
#'   
#' @export
#' @rdname register-parsers
parser.registry <- function(registry, list=FALSE){
	
	# return the registries as a list of lists
	if( missing(registry) ){
		# return the original environment if requested
		savefun <- if( list ) as.list else identity
		# build list of registries
		l <- list(preref.parsers = savefun(preref.parsers)
				, srcref.parsers = savefun(srcref.parsers)
			)
		return(l)
	}
	
	# get current registry for restoration
	old <- parser.registry()
	
	stopifnot( is.list(registry) )
	# set the registry
	sapply(names(registry), function(regname){
		
		reg <- registry[[regname]]
		# convert into an environment if necessary
		if( !is.environment(reg) ) reg <- list2env(reg, parent = emptyenv())
		# assign in roxygen2 environment
		assign(regname, reg, envir = topenv())
		
	})

	# return old registry list
	old
}

# immediate local backup of vanilla registry 
# store the environments so that they get populated at (lazy-)load time
registry_profile_vanilla <- parser.registry()

