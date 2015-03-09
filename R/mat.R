##' Matrices
##'
##' Like \code{matrix}, \code{mat} creates a matrix from the given set of 
##' values. These values can be represented by a data vector, a character,
##' or a list of vectors.
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
##' call to \code{mat}. The character string is split according to ';' and then
##' ','. For example, \code{mat('rnorm(10)')} will work, but 
##' \code{mat('rnorm(10, mean = 3)')} will result in an error. A work around would
##' be to use \code{mat('3 + rnorm(10, mean = 3)')}.
##' 
##' @examples
##' ## Using character vectors
##' mat('1, 2, 3; 4, 5, 6')
##' mat('1, 2, 3; 4, 5, 6', rows = TRUE)
##' mat("1 2 3; 4 5 6", sep = "")
##' (m <- mat(paste('exp(', 1:9, ')')))
##' resize(m, nrow = 3)
##' resize(m, nrow = 3, byrow = FALSE)
##' matrix(exp(1:9), 3, 3)
##' 
##' ## Using a list
##' mat(list(1:3, 4:6, 7:9))
##' mat(list(1:3, 4:6, 7:9), rows = TRUE)
mat <- function(x, ...) {
  UseMethod("mat")
}

##' @rdname mat
##' @export
##' @method mat default
mat.default <- function(x, ...) {
  matrix(x, ...)  # default to base matrix function
}

##' @rdname mat
##' @export
##' @method mat character
mat.character <- function(x, rows = TRUE, sep = ",", ...) {
  
  ## Gather rows and individual values
  vecs <- unlist(strsplit(x, split = ";"))  # column/row vectors
  char_vals <- unname(unlist(lapply(vecs, strsplit, split = sep)))
  num_vals <- unlist(lapply(char_vals, function(x) eval(parse(text = x))))
  
  ## Form matrix from parsed values by calling R's built-in matrix function
  if (rows) {
    matrix(num_vals, nrow = length(vecs), byrow = TRUE)
  } else {
    matrix(num_vals, ncol = length(vecs), byrow = FALSE)
  }
  
}

##' @rdname mat
##' @export
##' @method mat list
mat.list <- function(x, rows = TRUE, ...) {
  
  ## Check element types
  if (!all(sapply(x, class) %in% c("numeric", "integer"))) {
    stop("Each element must be of type 'numeric' or 'integer'.", call. = FALSE)
  }
  
  ## Check length of each element
  if (!all(sapply(x, length) >= 1) && length(unique(sapply(x, length))) != 1) {
    stop("Each element must contain at least one value.", call. = FALSE)
  }
  
  ## Form matrix by combining elements
  if (rows) do.call(rbind, x) else do.call(cbind, x)
  
}

##' Data Frames
##' 
##' Like \code{mat}, \coded{mat} creates a data frame from the given set of 
##' values. These values can be represented by a data vector, a character,
##' string, or a list of vectors.
##' 
##' @param x A data vector, character string, or a list.
##' @param ... Aditional optional arguments passed on to \code{mat}.
##' 
##' @return A \code{dataframe}.
##' 
##' @export
dmat <- function(x, ...) {
  data.frame(mat(x, ...))
}