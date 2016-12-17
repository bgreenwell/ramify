#' Matrices
#'
#' Like \code{matrix}, \code{mat} creates a matrix from the given set of 
#' values. However, these values can also be represented by a character string, 
#' or a list of vectors. Initially inspired by 
#' \href{http://docs.scipy.org/doc/numpy-1.10.0/reference/generated/numpy.matrix.html}{NumPy's matrix function}.
#' 
#' @param x A data vector, character string, or a list.
#' @param rows Logical. If \code{TRUE} (the default) the matrix is filled by rows, 
#'             otherwise the matrix is filled by columns.
#' @param sep Separator string. Values within each row/column of x are 
#'            separated by this string. Default is \code{","}.
#' @param eval Logical indicating whether or not the character string contains R 
#'   expressions that need to be evaluated. Default is \code{FALSE}. See examples 
#'   below for usage.
#' @param ... Aditional optional arguments to be passed on to \code{matrix}.
#' @return A matrix (i.e., an object of class \code{"matrix"}).
#' @seealso \code{\link{bmat}}, \code{\link{dmat}}, \code{\link{matrix}}.
#' @export
#' @examples
#' # Creating a matrix from a character string
#' mat("1, 2, 3, 4; 5, 6, 7, 8")  # ";" separates rows
#' mat("1, 2, 3, 4; 5, 6, 7, 8", rows = FALSE)  # ";" separates columns
#' mat("1 2 3 4; 5 6 7 8", sep = "")  # use spaces instead of commas
#' mat(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2, byrow = TRUE)  # works like matrix too
#'
#' # Character strings containing R expressions
#' mat("rnorm(3); rnorm(3)")
#' mat("rnorm(3); rnorm(3)", eval = TRUE)
#' mat("1, 2, 3; 4, 5, pi")
#' mat("1, 2, 3; 4, 5, pi", eval = TRUE)
#'
#' # Creating a matrix from a list
#' z1 <- list(1:5, 6:10)
#' z2 <- list(a = 1:5, b = 6:10)
#' mat(z1)
#' mat(z2)  # preserves names as row names
#' mat(z2, rows = FALSE)  # preserves names as column names
mat <- function(x, ...) {
  UseMethod("mat")
}


#' @rdname mat
#' @method mat default
#' @export
mat.default <- function(x, ...) {
  matrix(x, ...)  # default to base matrix function
}


#' @rdname mat
#' @method mat character
#' @export
mat.character <- function(x, rows = TRUE, sep = ",", eval = FALSE, ...) {

  # Gather rows and individual values
  vecs <- unlist(strsplit(x, split = ";"))  # column/row vectors
  char_vals <- if (!is.null(sep)) {
    trimws(unname(unlist(lapply(trimws(vecs), strsplit, split = sep))))
  } else {
    vecs
  }

  # Extract matrix values
  vals <- if (eval) {
    eval(parse(text = paste0("c(", paste0(char_vals, collapse = ","), ")")))
  } else {
    if (all(grepl("^\\d*(\\.\\d+)?$", char_vals))) {  # convert to numeric
      as.numeric(char_vals)  # much faster!
    } else {  # keep as character
      char_vals
    }
  }

  # Form matrix from parsed values by calling R's built-in matrix function
  if (rows) {
    matrix(vals, nrow = length(vecs), byrow = TRUE, ...)
  } else {
    matrix(vals, ncol = length(vecs), byrow = FALSE, ...)
  }

}
# mat.character <- function(x, rows = TRUE, sep = getOption("mat.sep"), 
#                           ...) {
#   
#   # Gather rows and individual values
#   vecs <- unlist(strsplit(x, split = ";"))  # column/row vectors
#   char_vals <- if (!is.null(sep)) {
#     trimws(unname(unlist(lapply(trimws(vecs), strsplit, split = sep))))
#   } else {
#     vecs
#   }
#   
#   # Conver to numeric
#   if (all(grepl("^\\d*(\\.\\d+)?$", char_vals))) {
#     # vals <- unlist(lapply(vals, function(x) eval(parse(text = x))))
#     num_vals <- as.numeric(char_vals)  # much faster!
#   } else {
#     num_vals <- eval(parse(text = paste0("c(", 
#                                          paste0(char_vals, collapse = ","), 
#                                          ")")))
#   }
#   # num_vals <- unlist(lapply(char_vals, function(x) eval(parse(text = x))))
#   
#   # Form matrix from parsed values by calling R's built-in matrix function
#   if (rows) {
#     matrix(num_vals, nrow = length(vecs), byrow = TRUE, ...)
#   } else {
#     matrix(num_vals, ncol = length(vecs), byrow = FALSE, ...)
#   }
#   
# }


#' @rdname mat
#' @method mat list
#' @export
mat.list <- function(x, rows = TRUE, ...) {
  
  # Check element types
  if (!all(sapply(x, class) %in% c("numeric", "integer"))) {
    stop("Each element must be of type 'numeric' or 'integer'.", call. = FALSE)
  }
  
  # Check length of each element
  if (!all(sapply(x, length) >= 1) && length(unique(sapply(x, length))) != 1) {
    stop("Each element must contain at least one value.", call. = FALSE)
  }
  
  # Form matrix by combining elements
  if (rows) do.call(rbind, x) else do.call(cbind, x)
  
}
