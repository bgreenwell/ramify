pprint.matrix <- function(x, digits = 3, debug = FALSE) {
  # Format each element of the matrix with specified precision
  x2 <- apply(x, c(1, 2), function(y) {
    format(y, digits = digits, nsmall = digits, trim = TRUE)
  })
  
  # Optionally print the formatted matrix if debug is TRUE
  if (debug) {
    print(x2)
  }
  
  return(x2)  # Return the formatted matrix
}

# Example test cases can be added separately using a library like `testthat`
# Test for matrices with different types of numbers (real, imaginary, complex)