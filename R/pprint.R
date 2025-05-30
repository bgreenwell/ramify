pprint.matrix <- function(mat, digits=2) {
  # Ensure the input is a matrix
  if (!is.matrix(mat)) {
    stop("Input is not a matrix")
  }
  
  # Handle edge cases for empty matrices correctly
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(mat)
  }
  
  # Format entries of the matrix correctly separating real and imaginary parts
  formatted_matrix <- vapply(seq_along(mat), function(index) {
    x <- mat[index]
    if (is.complex(x)) {
      real_part <- format(Re(x), digits=digits)
      imag_part <- format(abs(Im(x)), digits=digits)
      sign <- ifelse(Im(x) < 0, "-", "+")
      return(paste0(real_part, sign, imag_part, "i"))
    } else {
      return(format(x, digits=digits))
    }
  }, character(1))
  
  # Reshape the formatted vector back to the original matrix dimensions
  dim(formatted_matrix) <- dim(mat)
  
  return(formatted_matrix)
}

# Example usage and documentation
#' Pretty Print Matrix with Proper Formatting for Complex Numbers
#'
#' This function formats a matrix with proper handling of complex numbers, 
#' ensuring both real and imaginary parts are displayed with appropriate signs.
#'
#' @param mat A matrix that may contain complex numbers.
#' @param digits Number of significant digits for formatting numbers.
#' @return Formatted matrix with string representations.
#' @examples
#' pprint.matrix(matrix(c(1+1i, 2, 3-2i, 4), nrow=2), digits=2)