# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

findEndOfTag <- function(string, is_code = FALSE) {
    .Call('roxygen2_findEndOfTag', PACKAGE = 'roxygen2', string, is_code)
}

rdComplete <- function(string, is_code = FALSE) {
    .Call('roxygen2_rdComplete', PACKAGE = 'roxygen2', string, is_code)
}

leadingSpaces <- function(lines) {
    .Call('roxygen2_leadingSpaces', PACKAGE = 'roxygen2', lines)
}

tokenise_block <- function(lines, file = "", offset = 0L) {
    .Call('roxygen2_tokenise_block', PACKAGE = 'roxygen2', lines, file, offset)
}

find_includes <- function(path) {
    .Call('roxygen2_find_includes', PACKAGE = 'roxygen2', path)
}

splitByWhitespace <- function(string) {
    .Call('roxygen2_splitByWhitespace', PACKAGE = 'roxygen2', string)
}

wrapString <- function(string, width = 80L, indent = 2L) {
    .Call('roxygen2_wrapString', PACKAGE = 'roxygen2', string, width, indent)
}

