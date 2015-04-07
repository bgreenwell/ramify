#' Shorten a Vector
#' 
#' Shorten a vector using \code{...} notation.
#' 
#' @keywords internal
add_dots <- function(x, pos = 3) {
  if (length(x) >= pos + 2) {
    c(x[1:(pos-1)], "...", x[length(x)])
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
  dims <- paste(dim(x), collapse = " by ")
  paste(dims, "matrix of", paste0(typeof(x), "s"))
}