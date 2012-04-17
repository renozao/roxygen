# Translate a tag and expressions into an Rd expression;
# multiple expressions take their own braces.
#
# Tags have two methods: \code{merge} and \code{format}.  Currently for all
# tags, merge just combines all values, and format selects from these to 
# display the tags in the appropriate way. 
#
new_tag <- function(tag, values) {
  if (is.null(values)) return()
  
  subc <- str_c(tag, "_tag")
  list(structure(list(tag = tag, values = values), class = c(subc, "rd_tag")))
}

is.rd_tag <- function(x) inherits(x, "rd_tag")

#' @S3method print rd_tag
print.rd_tag <- function(x, ...) {
  cat(format(x), "\n", sep='')
}

# Translate a tag and values into an Rd expression; multiple values get their
# own braces.
rd_tag <- function(tag, ..., space = FALSE) {
  if (space) {
    values <- str_c("\n", str_c(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }
  # Turn non-breaking spaces back into regular spaces
  values <- str_replace_all(values, fixed("\u{A0}"), " ")
  str_c("\\", tag, str_c("{", values, "}", collapse = ""), "\n")                         
}

#' @S3method format rd_tag
format.rd_tag <- function(x, ...) stop("Unimplemented format")

#' @S3method merge rd_tag
merge.rd_tag <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))  
  new_tag(x$tag, c(x$values, y$values))
}

# Tags that repeat multiple times --------------------------------------------

#' @S3method format keyword_tag
#' @S3method format alias_tag
format_rd <- function(x, ...) {
  vapply(sort(unique(x$values)), rd_tag, tag = x$tag, 
    FUN.VALUE = character(1), USE.NAMES = FALSE)
}
format.keyword_tag <- format_rd
format.alias_tag <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_rd(x, ...)
}

# Tags that keep the first occurence -----------------------------------------
format_first <- function(x, ...) {
  rd_tag(x$tag, x$values[1])
} 
#' @S3method format name_tag
#' @S3method format title_tag
#' @S3method format docType_tag
#' @S3method format format_tag
#' @S3method format encoding_tag
format.name_tag <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_first(x, ...)
}
format.title_tag <- format_first
format.docType_tag <- format_first
format.format_tag <- format_first
format.encoding_tag <- format_first

# Tags collapse their values into a single string ----------------------------

format_collapse <- function(x, ..., indent = 2, exdent = 2) {
  values <- str_c(x$values, collapse = "\n\n")
  rd_tag(x$tag, str_wrap(values, width = 60, indent = indent, 
    exdent = exdent), space = TRUE)
} 
#' @S3method format author_tag
#' @S3method format concept_tag
#' @S3method format description_tag
#' @S3method format details_tag
#' @S3method format note_tag
#' @S3method format references_tag
#' @S3method format seealso_tag
#' @S3method format source_tag
#' @S3method format usage_tag
#' @S3method format value_tag
format.author_tag <- format_collapse
format.concept_tag <- format_collapse
format.description_tag <- format_collapse
format.details_tag <- format_collapse
format.note_tag <- format_collapse

# Lookup References from Bibliography Files
#
# - inst/REFERENCES.bib if present
# - tags @bibliography: will have populate global variable 'bibfiles'
lookupBibentry <- function(keys){
	
	emptybib <- function(){ x <- list(); class(x) <- 'bibentry'; x}
	
	bibcache <- roxygenGlobal('bibentries')
	if( is.null(bibcache) ) bibcache <- emptybib()
	on.exit( roxygenGlobal('bibentries', bibcache) )
	
	refbibs <- roxygenGlobal('REFERENCES')	
	on.exit( roxygenGlobal('REFERENCES', refbibs), add=TRUE )
	pkgdir <- roxygenGlobal('package.dir')
	reffile <- file.path(pkgdir, 'inst/REFERENCES.bib')
	# load default library if necessary
	if( is.null(refbibs) ){
		refbibs <- 
		if( length(reffile)>0 && file.exists(reffile) ){
			message("Loading bibliography file '", reffile, "'")
			read.bib(reffile)
		}
		else emptybib()		
	}
	
	bibfiles <- roxygenGlobal('bibfiles')
	on.exit( roxygenGlobal('bibfiles', bibfiles), add=TRUE)
	
	getEntry <- function(key, bibentry){
		k <- unlist(bibentry$key)
		bibentry[k %in% key]		
	}
	
	sapply(keys, function(k){
		#message("Lookup for key '", k, "'")
		cit <- getEntry(k, refbibs)		
		if( length(cit) == 0 ){ # try loading from other bibfiles
			cit <- getEntry(k, bibcache)
			if( length(cit) == 0 ){
				for(i in seq_along(bibfiles)){
					f <- names(bibfiles)[i]
					if( !bibfiles[i] ){
						message("Loading bibliography file '", f, "' ... ", appendLF=FALSE)
						suppressWarnings(suppressMessages(capture.output(bibs <- read.bib(f))))
						message("OK")
						bibfiles[i] <<- TRUE
						if( length(bibs)>0 )
							bibcache <<- c(bibcache, bibs)
					}					
					cit <- getEntry(k, bibcache)
					if( length(cit) != 0 ) break;				
				}
			}
			# add the entry to file REFERENCES.bib if necessary
			if( length(cit) != 0 && length(reffile)>0 ){
				message("Adding entry '", k, " to REFERENCES.bib ... ", appendLF=FALSE)
				write.bib(cit, file=reffile, append=TRUE, verbose=FALSE)
				message("OK")
			}
		}		
		if( length(cit) == 0 ) ""
		else format(cit)
	})
}

format.references_tag <- function(x, ...){	
	# detect citations from full references
	refs <- x$values		
	info <- str_match(refs, "^@cite:([^:]+):([0-9]+)-([0-9]+):(.*)")
	keys <- info[,5]
	ikeys <- which(!is.na(keys))	
	if( length(ikeys) > 0  ){
		keys <- keys[ikeys]
		res <- lookupBibentry(keys)		
		# check for unfound keys
		nf <- which(res == "")
		if( length(nf) > 0 ){
			# extract srcref info
			info <- info[ikeys[1],]
			srcref <- list(filename=info[2], lloc=c(info[3],0,info[4],0))
			roxygen_warning("Bibtex entrie(s) not found: ", paste(keys[nf], collapse=', '), srcref=srcref)
			res[nf] <- str_c("\\cite{??", keys[nf], "??}")
			#res[-nf] <- res[-nf]
		}
		x$values <- res
	}
	x$values <- unlist(x$values)
	#print(x$values)
	# collapse all references
	format_collapse(x)
}
format.seealso_tag <- format_collapse
format.source_tag <- format_collapse
format.usage_tag <- function(x, ...) format_collapse(x, ..., exdent = 4)
format.value_tag <- format_collapse


# Tags that don't have output ------------------------------------------------

format_null <- function(x, ...) NULL

#' @S3method format family_tag
format.family_tag <- format_null
format.inheritParams_tag <- format_null
format.formals_tag <- format_null

# Tags with special errors or other semantics --------------------------------

#' @S3method format arguments_tag
format.arguments_tag <- function(x, ...) {
  names <- names(x$values)
  dups <- duplicated(names)
  
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  rd_tag("arguments", str_wrap(items, width = 60, exdent = 2, indent = 2),
    space = TRUE)
}

#' @S3method format section_tag
format.section_tag <- function(x, ...) {
  names <- vapply(x$values, "[[", "name", FUN.VALUE = character(1))

  contents <- vapply(x$values, "[[", "content", FUN.VALUE = character(1))
  contents <- str_wrap(str_trim(contents), width = 60, exdent = 2, indent = 2)
  
  setions <- str_c("\\section{", names, "}{\n", contents, "\n}\n", 
    collapse = "\n")
}

#' @S3method format examples_tag
format.examples_tag <- function(x, ...) {
  values <- str_c(x$values, collapse = "\n")
  rd_tag(x$tag, values, space = TRUE)  
}


#' @S3method format newcommand_tag
format.newcommand_tag <- function(x, ...){
	cmds <- sapply(x$values, function(cmd){
		# check for arguments
		if( grepl("^((\\[[0-9]+\\])|(\\{))", cmd) ) cmd
		else str_c("{", cmd, "}")
	})
	str_c("\\", x$tag, "{\\", str_trim(names(x$values)), "}", cmds, collapse="\n")	
}
#' @S3method format renewcommand_tag
format.renewcommand_tag <- format.newcommand_tag

#' @S3method format S4method_tag
format.S4method_tag <- function(x, ...) {	
	if( length(x$values) == 0 )	return()
	
	doformat <- function(...){
		s <- capture.output(show(...))
		print(s)
		paste(s[2:(length(s)-2)], collapse="\n")
	}
	
	# initialize result string	
	rd <- new_rd_file()
	if( length(x$values) == 1 ){
		doc <- x$value[[1]]
		# documenting a single S4 method: document the object as normally		
		if( is.null(doc$title) ){
			doc$title <- 
			if( !is.null(doc$S4generic) ) str_c("Generic Function ", doc$S4generic)
			else str_c("Method ", doc$S4method, ",", str_c(doc$signature, collapse=","))
		}
		# use title from first generic
		add_tag(rd, new_tag('title', doc$title))				
		if( !is.null(doc$description) ) 
			add_tag(rd, new_tag('description', doc$description))				
		if( !is.null(doc$details) )
			add_tag(rd, new_tag('details', doc$details))
		# add S4 usage if S4 method (not generic) 
		if( is.null(doc$S4generic) ) 
			add_tag(rd, new_tag('usage', str_c("\\S4method{", doc$S4method, "}{", str_c(doc$signature, collapse=","), "}(", doc$S4call, ")")))
	}else{		
		# documenting multiple S4 methods:
		# - use \description{...} for the generics 
		# - use \section{Method}{...} for the description of S4 methods
		
		# check if any generic is documented
		wgeneric <- sapply(x$values, function(data) !is.null(data$S4generic))		
		genericDef <- x$values[wgeneric]
		# build vector with the names of the generics that are document here  
		genericDefName <- character()
		genericNames <- unique(sapply(x$values, '[[', 'S4method'))		
		stopifnot( length(genericNames) > 0 )
		plural <- if( length(genericNames) > 1 ) 's'
		# add description and title from the generic(s)		
		if( length(genericDef)>0 ){
			# fill vector of generic names
			genericDefName <- unique(sapply(genericDef, '[[', 'S4generic'))			
			# title
			title <- genericDef[[1]]$title %||% 
					str_c("Generic function", plural, " ", str_c("`", genericDefName, "`", collapse=', '))				
				
			# merge descriptions
			desc <- 
			if( length(genericNames) == 1 ) genericDef[[1]]$description %||% title
			else sapply(genericDef, function(d) str_c("\\code{", d$S4generic, '}: ', d$description %||% d$title))
			# merge details
			details <- 
			sapply(genericDef, function(d){
				if( is.null(d$details) ) return(NA)
				str_c("\\code{", d$S4generic, '}: ', d$details )
			})
			details <- details[!is.na(details)]
			#
	
			# use title from first generic
			add_tag(rd, new_tag('title', title))				
			add_tag(rd, new_tag('description', desc))				
			if( length(details) > 0 )
				add_tag(rd, new_tag('details', details))
		}else{ # add default description			
			desc <- str_c("This file contains the documentation for S4 methods defined for the generic", plural, " "
					, str_c("\\code{", genericNames, "}", collapse=", "), ".")
			title <- str_c("Methods for Generic", plural, ' '
							, str_c("`", genericNames, "`", collapse=", "))
			add_tag(rd, new_tag('title', title))
			add_tag(rd, new_tag('description', desc))					
		}
				
		# generate a 'Methods' section that describes each S4 method
		methods <- x$values[!wgeneric]
		if( length(methods) > 0 ){
			
			# usage
			lapply(methods, function(doc){				
				add_tag(rd, new_tag('usage', str_c("\\S4method{", doc$S4method, "}{", str_c(doc$signature, collapse=","), "}(", doc$S4call, ")")))
			})			
					
			# short description of each S4 method
			contents <- lapply(methods, function(data){
				d <- data$description %||% data$title				
				if( is.null(d) ) return(NA)
				str_c("\\item{", data$S4alias, "}{"
					, str_c(data$title, data$description, data$details, sep="\n\n")
					, "\n"
					#, if( data$link ) str_c("See details in \\code{\\link{", data$S4alias,"}}.")
					, if( !data$S4method %in% genericDefName ) str_c("See generic \\code{\\link{", data$S4method,"}}.")
					, "}\n")
			})
			contents <- contents[!is.na(contents)]
			if( length(contents) > 0 ){
				contents <- paste(contents, collapse="\n")
				contents <- str_c("\n\\describe{\n", contents, "\n}\n")
				add_tag(rd, process.section(NULL, str_c('Methods:', contents)))				
			}			
		}
	}
	
	format(rd)
}
