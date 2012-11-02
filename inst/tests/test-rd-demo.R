context("Rd - demo")
roc <- rd_roclet()

test_that("@demo is correctly processed", {
	clear_caches()
	
	txt <- "
#' A function
#' 
#' @demo 
#'
#' # do something
#' print(10)
#'
#' @examples
#' print(1)
a <- function(){}
			
#' Another function
#'
#' @demo This is a demo
#' # do something else
#' plot(1:10)
#'
#' @rdname a
#'
b <- function(z){}
			
#' Test
#'
#' @demo This is another demo
#' message('Hello')
#' plot(sin(1:60/10))
#'
d <- function(x){}
"
	tmp <- tempfile('test-rd-demo')
	dir.create(tmp)
	dir.create(file.path(tmp, 'man'))
	owd <- setwd(tmp)
	on.exit(setwd(owd))
	on.exit(unlink(tmp, recursive=TRUE), add=TRUE) 	
	
	out <- roc_proc_text(roc, txt)
	roc_output(roc, out, '.')
	
	# check index file
	demodir <- file.path(tmp, 'demo')
	idxfile <- file.path(demodir, '00Index')
	expect_true(file.exists(idxfile), "Index file is created")
	l <- readLines(idxfile)
	expect_identical(paste(l, collapse="\n"), "a.R This is a demo\nd.R This is another demo", "Content of Index file is correct")
	# check demo files
	df <- file.path(demodir, 'a.R')
	expect_true(file.exists(df), "First demo file is created")
	code <- paste(readLines(df), collapse="\n")
	expect_identical(code, "# do something\nprint(10)\n\n# do something else\nplot(1:10)"
					, "Content of first demo file is correct")
	df <- file.path(demodir, 'd.R')
	expect_true(file.exists(df), "Second demo file is created")
	code <- paste(readLines(df), collapse="\n")
	expect_identical(code, "message('Hello')\nplot(sin(1:60/10))"
			, "Content of second demo file is correct")
	TRUE
})
