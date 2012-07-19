#' Parse a source file containing roxygen directives.
#'
#' @param file string naming file to be parsed
#' @return List containing parsed directives
#' @keywords internal
#' @export
parse.file <- function(file, env, env_hash) {
  srcfile <- srcfile(file)
  
  parse_cache$compute(c(env_hash, readLines(file, warn = FALSE)), {
    src_refs <- attributes(parse(srcfile$filename, srcfile = srcfile))$srcref
    pre_refs <- prerefs(srcfile, src_refs)

    if (length(src_refs) == 0) return(list())

    src_parsed <- lapply(src_refs, parse.srcref, env = env)
    pre_parsed <- lapply(pre_refs, parse.preref)

    stopifnot(length(src_parsed) == length(pre_parsed))

    mapply(c, pre_parsed, src_parsed, SIMPLIFY = FALSE)
  })
}

# globally cached current package name used to create the package environment
# and to determine if S4 definitions should be looked within the package 
roxygen_pkgname <- local({
	.local <- NULL
	function(value){
		if( missing(value) ) .local
		else{
			old <- .local
			.local <<- value
			old
		}
	}
})

# setup proper package namespace (as in devtools)
roxygen_pkgenv <- function(env, pkg, default){
	
    if( !is.null(pkg) ){
  		name <- str_c('package:', pkg)
  		if (!is.loaded(pkg)) {
        	attach(env, name = name)
  		}
  		env <- as.environment(name)
    }
    else{
		roxygen_pkgname(default)
    	setPackageName(default, env)
    }
	# return environment
	env
}

#' Parse many files at once.
#'
#' @param \dots files to be parsed
#' @return List containing parsed directives
#' @seealso \code{\link{parse.file}}
#' @keywords internal
#' @export
#' @importFrom digest digest
parse.files <- function(paths) {
  # Source all files into their own environment so that parsing code can
  # access them.
  env <- new.env(parent = parent.env(globalenv()))
  env_hash <- suppressWarnings(digest(env))
  
  # setup proper package namespace (as in devtools)
  env <- roxygen_pkgenv(env, roxygen_pkgname(), 'roxygen_test')

  lapply(paths, sys.source, chdir = TRUE, envir = env)
  on.exit(cleanup_s4(env))
  
  unlist(lapply(paths, parse.file, env = env, env_hash = env_hash), 
    recursive = FALSE)
}


cleanup_s4 <- function(env) {
  classes <- getClasses(env)
  generics <- getGenerics(env)

  lapply(classes, removeClass, where = env)
  lapply(generics@.Data, removeMethods, where = env)
  
  pkg_gen <- generics@.Data[generics@package == roxygen_pkgname()]
  lapply(pkg_gen, removeGeneric, where = env)
  
  invisible(TRUE)
}

#' Text-parsing hack using tempfiles for more facility.
#'
#' @param text stringr containing text to be parsed
#' @return The parse tree
#' @keywords internal
#' @export
parse.text <- function(text) {
  file <- tempfile()
  writeLines(text, file)
  on.exit(unlink(file))
  parse.files(file)
}
