#' Matrix Rank
#' 
#' Compute the rank of a matrix using the singular value decomposition (SVD)
#' method.
#' 
#' @param x an object that inherits from class \code{"matrix"}.
#' @param tol Threshold below which SVD values are considered zero.
#' @details
#' The singular value decomposition method simply computes the SVD of \code{x}
#' and returns the number of singular values of \code{x} that are greater than
#' \code{tol}. See the function \code{\link{rankMatrix}} in package 
#' \code{\link{Matrix}} for alternative methods.
#' @export
#' @examples 
#' matrix_rank(1:5)
#' matrix_rank(randn(2, 2))
#' matrix_rank(cbind(c(1, 1, 1), c(2, 2, 2)))
#' matrix_rank(ones(3, 3))
#' matrix_rank(zeros(3, 5))
matrix_rank <- function(x, tol) {
  UseMethod("matrix_rank")
}


#' @rdname matrix_rank
#' @method matrix_rank default
#' @export
matrix_rank.default <- function(x, tol) {
  x <- atleast_2d(x)
  sval <- svd(x, 0, 0)$d
  if (missing(tol)) {
    tol <- max(dim(x)) * .Machine$double.eps * max(sval)
  }
  sum(sval > tol)
}


#' @rdname matrix_rank
#' @method matrix_rank matrix
#' @export
matrix_rank.matrix <- function(x, tol) {
  sval <- svd(x, 0, 0)$d
  if (missing(tol)) {
    tol <- max(dim(x)) * .Machine$double.eps * max(sval)
  }
  sum(sval > tol)
}


#' @rdname matrix_rank
#' @method matrix_rank data.frame
#' @export
matrix_rank.data.frame <- function(x, tol) {
  x <- data.matrix(x)
  sval <- svd(x, 0, 0)$d
  if (missing(tol)) {
    tol <- max(dim(x)) * .Machine$double.eps * max(sval)
  }
  sum(sval > tol)
}
