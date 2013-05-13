# Translate a tag and expressions into an Rd expression;
# multiple expressions take their own braces.
#
# Tags have two methods: \code{merge} and \code{format}.  Currently for all
# tags, merge just combines all values, and format selects from these to 
# display the tags in the appropriate way. 
#
new_tag <- function(tag, values, rdID=NULL) {
  if (is.null(values)) return()
  
  subc <- str_c(tag, "_tag")
  list(structure(list(tag = tag, values = values
  	, rdID = rdID)
  	, class = c(subc, "rd_tag")))
}

is.rd_tag <- function(x) inherits(x, "rd_tag")

#' @S3method print rd_tag
print.rd_tag <- function(x, ...) {
  cat(format(x), "\n")
  cat("rdID(s):", toString(x$rdID), "\n")
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
  values <- str_replace_all(values, fixed("\u{201c}"), "\"")
  values <- str_replace_all(values, fixed("\u{201d}"), "\"")
  str_c("\\", tag, str_c("{", values, "}", collapse = ""), "\n")                         
}

#' @S3method format rd_tag
format.rd_tag <- function(x, ...) stop("Unimplemented format")

#' @S3method merge rd_tag
merge.rd_tag <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))  
  new_tag(x$tag, c(x$values, y$values), c(x$rdID, y$rdID))
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

format_collapse <- function(x, ..., indent = 2, exdent = 2, cite = FALSE) {
  values <- str_c(x$values, collapse = "\n\n")
  # substitute citation keys
  if( cite ) values <- format_cite(values)
  rd_tag(x$tag, str_wrap(values, width = 60, indent = indent, 
    exdent = exdent), space = TRUE)
} 

format_collapse_cite <- function(...) format_collapse(..., cite = TRUE)
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
format.description_tag <- format_collapse_cite
format.details_tag <- format_collapse_cite
format.note_tag <- format_collapse_cite

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
	x$values <- unique(unlist(x$values))
	#print(x$values)
	# collapse all references
	format_collapse_cite(x)
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
format.srcref_tag <- format_null
format.rdID_tag <- format_null
format.inline_tag <- format_null

# Tags with special errors or other semantics --------------------------------

#' @S3method format arguments_tag
format.arguments_tag <- function(x, ...) {
  names <- names(x$values)
  dups <- duplicated(names)
  
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  rd_tag("arguments", str_wrap(items, width = 60, exdent = 2, indent = 2),
    space = TRUE)
}

#' @S3method format slot_tag
format.slot_tag <- function(x, ...) {
  names <- names(x$values)
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  format(process.section("section", str_c("Slots:\\describe{\n\n", items, "\n\n}\n"))[[1]])
}


#' @S3method format section_tag
format.section_tag <- function(x, ...) {
  names <- vapply(x$values, "[[", "name", FUN.VALUE = character(1))

  contents <- vapply(x$values, "[[", "content", FUN.VALUE = character(1))
  contents <- str_wrap(str_trim(contents), width = 60, exdent = 2, indent = 2)
  
  # substitute citation keys
  contents <- format_cite(contents)
  
  setions <- str_c("\\section{", names, "}{\n", contents, "\n}\n", 
    collapse = "\n")
}

#' @S3method format examples_tag
format.examples_tag <- function(x, ...) {
  values <- str_c(x$values, collapse = "\n")
  # TODO: auto-wrap with width 100 
  # [see changes in R CMD check: http://developer.r-project.org/blosxom.cgi/R-devel/NEWS/2013/05/09#n2013-05-09]
  #values <- formatR::tidy.source(text=values, width.cutoff = 100L)
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
	
	.local <- function(x, fname){
		value <- sapply(x$values, function(x){
			# skip empty descriptions
			if( length(x$introduction) == 0L ) return(NA)
			desc <- str_c(c(unlist(x$introduction), unlist(x$links)), collapse="\n\n")
			str_c("\n\\item{",fname,"}{\\code{signature("
					, str_c(names(x$signature), ' = "', x$signature, '"', collapse=", ")
					,")}: ",desc,"\n}")
		})
		value <- str_c(value[!is.na(value)], collapse="\n")
		# return NA if all are NA
		if( nchar(value) == 0L ) return(NA)
		value
	}
	
	# look for multiple generics
	f <- sort(unique(names(x$values)))
	res <- sapply(f, function(fname){
			x$values <- x$values[names(x$values) == fname]
			.local(x, fname)
	})
	
	res <- str_c(res[!is.na(res)], collapse="\n\n")
	if( nchar(res) == 0L ) return()
	format(process.section("section", str_c("Methods:\\describe{\n", res, "\n\n}\n"))[[1]])
}

#' @S3method format demo_tag
format.demo_tag <- function(x, ...){
	
	if( !length(x$values) ) return()
	
	filename <- x$values[[1]]$filename
	demoname <- basename(filename)
	
	# determine title
	desc <- vapply(x$values, "[[", "desc", FUN.VALUE = character(1))
	desc <- desc[!is.na(desc)]
	title <- if( length(desc) ) desc[1L] else demoname  
	
	# stick code together
	code <- vapply(x$values, "[[", "code", FUN.VALUE = character(1))
	code <- code[!is.na(code)]
	code <- paste(code, collapse = "\n\n")
	if( !nchar(code) ){
		roxygen_warning("Demo '", demoname,":", title, "' is empty.")
		return()
	}
	
	# create demo directory if nor present
	demodir <- normalizePath(dirname(filename), mustWork=FALSE)
	if( !file.exists(demodir) ){
		dir.create(demodir)
	}
	# (re)-write demo file
	filename <- str_c(filename, ".R")
	message("Writing demo file '", filename, "' ... ", appendLF=FALSE)
	write(code, file=filename)
	# load demo index file
	dIndex <- file.path(demodir, '00Index')
	idx <- matrix(character(), 0L, 2L)
	if( file.exists(dIndex) ){
		idx <- readLines(dIndex)
		idx <- str_split_fixed(idx, "\t", 2)
	}
	demoline <- c(demoname, title)
	ifile <- which(idx[,1] == demoname)
	if( !length(ifile) ) idx <- rbind(idx, demoline) 
	else idx[ifile, ] <- demoline
	dimnames(idx) <- NULL
	# write demo index
	write.table(idx, file=dIndex, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
	message('OK [', nrow(idx), ' demo(s)]')
	# return nothing
	return()
}
