if (!exists("preref.parsers")) {
  # Preref parser table
  preref.parsers <- new.env(parent=emptyenv())
  # Srcref parser table
  srcref.parsers <- new.env(parent=emptyenv())
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
  preref.parsers[[key]] <- parser
}

#' @export
#' @rdname register-parser
register.srcref.parser <- function(key, parser) {
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
#'   
#' @export
#' @rdname register-parsers
parser.registry <- function(registry){
	
	# return the registries as a list of lists
	if( missing(registry) ){
		l <- list(preref.parsers = as.list(preref.parsers)
				, srcref.parsers = as.list(srcref.parsers)
			)
		return(l)
	}	
	
	stopifnot( is.list(registry) )
	# set the registry
	sapply(names(registry), function(reg){
		assign(reg, list2env(registry[[reg]], parent=emptyenv()), envir=topenv())
	})
}
