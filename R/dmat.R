#' Data Frames
#' 
#' Like \code{mat}, \code{dmat} creates a data frame from the given set of 
#' values. These values can be represented by a data vector, a character
#' string, or a list of vectors.
#' 
#' @param x A data vector, character string, or a list.
#' @param ... Aditional optional arguments passed on to \code{mat}.
#' 
#' @return A \code{dataframe}.
#' 
#' @seealso \code{\link{mat}}, \code{\link{bmat}}.
#' 
#' @examples
#' dmat('1e-01, 2+5, 3, 4, 5; 6, 7, 8, 9^2, pi', rows = FALSE)
#' z <- list(a = 1:10, b = 11:20, c = 21:30)
#' dmat(z)  # list elements form rows
#' dmat(z, rows= FALSE)  # list elements form columns
#' 
#' @export
dmat <- function(x, ...) {
  as.data.frame(mat(x, ...))  # FIXME: data.frame or as.data.frame?
}