"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Given argument list, produce usage string for it.
# 
# Adapted from \code{\link{prompt}}.
#
# @param f function, or name of function, as string
# @return a string
usage <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- deparse(arg, backtick = TRUE, width.cutoff = 500L)
    text <- str_replace_all(text, fixed("%"), "\\%")
    text <- str_replace_all(text, fixed(" "), "\u{A0}")
    Encoding(text) <- "UTF-8"    
    
    str_c("\u{A0}=\u{A0}", paste(text, collapse = "\n"))
  }

  arg_values <- vapply(args, arg_to_text, character(1))
  
  paste(names(args), arg_values, collapse = ", ", sep = "")
}

# Does the string contain no matter, but very well [:space:]?
# @param string the string to check
# @return TRUE if the string contains words, otherwise FALSE
is.null.string <- function(string) {
  str_length(str_trim(string)) == 0
}


subs <- matrix(ncol = 2, byrow = T, c(
  '[]', 'sub',
  '<-', 'set',
  '!', 'not',
  '"', 'quote',
  '#', 'hash',
  '$', 'cash',
  '%', 'grapes',
  '&', 'and',
  '|', 'or',
  "'", 'single-quote',
  '(', 'open-paren',
  ')', 'close-paren',
  '*', 'star',
  '+', 'plus',
  ',', 'comma',
  '/', 'slash',
  ':', 'colon',
  ';', 'semi-colon',
  '<', 'less-than',
  '=', 'equals',
  '>', 'greater-than',
  '?', 'p',
  '@', 'at',
  '[', 'open-brace',
  '\\', 'backslash',
  ']', 'close-brace',
  '^', 'hat',
  '`', 'tick',
  '{', 'open-curly',
  '}', 'close',
  '~', 'twiddle'
))
subs[, 2] <- str_c("-", subs[, 2])

nice_name <- function(x) {
  for(i in seq_len(nrow(subs))) {
    x <- str_replace_all(x, fixed(subs[i, 1]), subs[i, 2])
  }
  x <- str_replace(x, "-+", "-")
  x
}


is_valid_rdname <- function(filename){
	str_detect(basename(filename), "^[a-zA-Z][a-zA-Z0-9_.-]*\\.Rd$")
}
make_valid_rdname <- function(filename, show_match=FALSE){
	
	# do nothing if the filename is already valid
	if( is_valid_rdname(filename) ) return(filename)
	
	# split problematic parts 
	name <- basename(filename)
	m <- str_match_all(name, "([^a-zA-Z]?)([^a-zA-Z0-9_.-]*)([a-zA-Z0-9_.-]*)([^a-zA-Z0-9_.-]*)")
	# for debugging
	if( show_match ) print(m)
	# replace problematic parts
	sapply(m, function(x){
		x[1,2] <- "xxx"
		if( nrow(x) > 1 ){
			x[1:(nrow(x)-1),3] <- "_"
			x[1:(nrow(x)-1),5] <- "_"
		}
		paste(x[,2], x[,3], x[,4], x[,5], sep='', collapse='')
	})	
	
}

roxygen_stop <- function(..., srcref = NULL) {
  stop(..., srcref_location(srcref), call. = FALSE)
}

roxygen_warning <- function(..., srcref = NULL) {
  warning(..., srcref_location(srcref), call. = FALSE)
}

srcref_location <- function(srcref = NULL) {
  if (is.null(srcref)) return()
  str_c(" in block ", basename(srcref$filename), ":", srcref$lloc[1])
}


#' Citing Package References
#' 
#' Create a citation string from package specific BibTex entries, suitable to 
#' be used in Rd files.
#' The entries are looked in a file named REFERNCES.bib in the package's root 
#' directory (i.e. inst/ in development mode).
#'  
#' @param key character vector of BibTex keys
#' @param bibentry a bibentry object, a Bibtex filename or a string like 
#' \code{"package:PKGNAME"}, where to look for entries.
#' @param ... extra parameters passed to \code{format.bibentry}
#' @return a character string containing the text formated BibTex entries 
#' @keywords internal
#' @import bibtex
cite <- function(key, bibentry, ...){
	# detect package name if necessary
	if( missing(bibentry) ){
		pkg <- Sys.getenv('R_PACKAGE_NAME')
		if( is.null(pkg) )
			pkg <- Sys.getenv('R_INSTALL_PKG')
		if( is.null(pkg) )
			stop("Could not identify package")
		bibentry <- paste('package:', pkg, sep='')
	}
	
	# load relevant Bibtex file
	bibs <- if( is(bibentry, 'bibentry') ) bibentry
			else if( is.character(bibentry) ){
				p <- str_match(bibentry, "^package:(.*)")[,2]
				if( is.na(p) ) bibtex::read.bib(file=bibentry)
				else bibtex::read.bib(package=p)
			}else
				stop("Invalid argument `bibentry`: expected bibentry object or character string [", class(bibentry), "]")
	
	if( !is.character(key) )
		stop("Invalid argument `key`: must be a character vector.")
	
	# extract the Bibtex keys
	k <- sapply(bibs, function(x) x$key)
	# format the entries
	paste(format(bibs[k %in% key], ...), collapse="\n\n")
}
