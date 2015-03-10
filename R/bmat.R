##' Matrices
##'
##' Like \code{mat}, \code{bmat} creates a matrix from the given set of values. 
##' These values, however, must be represented by a character string.
##' 
##' @param x A data vector, character string, or a list.
##' @param rows Logical. If TRUE (the default) the matrix is filled by rows, 
##'             otherwise the matrix is filled by columns.
##' @param sep Separator string. Values within each row/column of x are 
##'            separated by this string. Default is \code{","}.
##' @param ... Aditional optional arguments.
##' 
##' @details
##' Be careful when using \code{R} functions within character strings in the 
##' call to \code{bmat}. The character string is split according to ';' and then
##' ','. For example, \code{mat('rnorm(10)')} will work, but 
##' \code{mat('rnorm(10, mean = 3)')} will result in an error. A work around 
##' would be to use \code{mat('3 + rnorm(10, mean = 3)')}.
##' 
##' @examples
##' A1 <- mat('1, 2; 5, 6')
##' A2 <- mat('3, 4; 7, 8')
##' A3 <- mat('9, 10, 11, 12')
##' A <- bmat('A1, A2; A3')
bmat <- function(x, rows = TRUE, sep = ",", ...) {
  
  # Split into pieces
  pieces <- unlist(strsplit(x, split = ";"))
  
  # Parse, evaluate, and combine pieces
  bind1 <- if (rows) cbind else rbind
  bind2 <- if (rows) rbind else cbind
  combined <- lapply(pieces, function(x) {
    do.call(bind1, lapply(strsplit(x, split = sep)[[1]], 
                          function(y) eval(parse(text = y))))
  })
  do.call(bind2, combined) 
  
}