#' Shorten a Vector
#' 
#' Shorten a vector using \code{...} notation.
#' 
#' @keywords internal
add_dots <- function(x, pos = 3) {
  if (length(x) >= pos + 2) {
    c(x[seq_len(pos-1)], "...", x[length(x)])
  } else {
    x
  }
}


#' Describe a Matrix
#' 
#' Prints a short description about a matrix.
#' 
#' @keywords internal
desc_mat <- function(x) {
  # paste(paste(dim(x), collapse = " by "), "matrix of", paste0(typeof(x), "s"))
  paste(paste(dim(x), collapse = " x "), "matrix of", paste0(typeof(x), "s:"))
}


#' Add a Second Dimension
#' 
#' Force a one-dimensional vector to have a second dimension equal to one.
#' 
#' @keywords internal
atleast_2d <- function(x) {
  if (is.null(dim(x))) {
    attr(x, "dim") <- c(length(x), 1)
  }
  x
}
