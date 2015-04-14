#' Rectangular 2-D Grid
#' 
#' Creates matrices for vectorized evaluations of 2-D scalar/vector fields over 
#' 2-D grids.
#' 
#' @param x Numeric vector representing the first coordinate of the grid.
#' @param y Numeric vector representing the second coordinate of the grid.
#' @return a list of matrices, each having class \code{c("matrix", "mat")}.
#' @seealso \code{\link{expand.grid}}, \code{\link{contour}}.
#' @export
#' @examples
#' x <- seq(-5, 5, by = 0.1)
#' y <- seq(-5, 5, by = 0.1)
#' mg <- meshgrid(x, y)
#' z1 <- sin(mg[[1]]^2 + mg[[2]]^2) / (mg[[1]]^2 + mg[[2]]^2)
#' z2 <- mg[[1]] * exp(-mg[[1]]^2 - mg[[2]]^2)
#' contour(x, y, z1)
#' filled.contour(x, y, z2, xlim = c(-2, 2), ylim = c(-2, 2))
meshgrid <- function(x, y = x) {
  lenx <- length(x)
  leny <- length(y)
  list(matrix(rep(x, each = leny), nrow = leny, ncol = lenx),
       matrix(rep(y, times = lenx), nrow = leny, ncol = lenx))
#   list(as.mat(matrix(rep(x, each = leny), nrow = leny, ncol = lenx)),
#        as.mat(matrix(rep(y, times = lenx), nrow = leny, ncol = lenx)))
}


#' Row/Column Max/Min Indices
#'
#' Returns the indices of the maximum or minimum values along an axis.
#' 
#' @param x A matrix.
#' @param rows If \code{TRUE} (the default) the indices of each row max/min is
#'             returned. 
#' @return A vector of indices.
#' @export
#' @examples
#' m <- mat("94, 20, 44; 40, 92, 51; 27, 69, 74")
#' argmax(m)
#' argmin(m)
argmax <- function(x, rows = TRUE) {
  if (rows) {
    apply(x, MARGIN = 1, which.max)
  } else {
    apply(x, MARGIN = 2, which.max)
  }
}

#' @rdname argmax
#' @export
argmin <- function(x, rows = TRUE) {
  if (rows) {
    apply(x, MARGIN = 1, which.min)
  } else {
    apply(x, MARGIN = 2, which.min)
  }
}


#' Identity Matrix
#' 
#' Creates an \code{nrow}-by-\code{ncol} identity matrix.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @return A \code{nrow}-by-\code{ncol} identity matrix of class 
#'         \code{c("matrix", "mat")}.
#' @seealso \code{\link{diag}}.
#' @export
#' @examples
#' eye(4)  # 4-by-4 identity matrix
#' eye(4, 4)  # 4-by-4 identity matrix
#' eye(3, 5)  # 3-by-5 identity matrix
#' eye(5, 3)  # 5-by-3 identity matrix
eye <- function(nrow = 1, ncol = nrow) {
  m <- diag(1L, nrow, ncol)
  class(m) <- c("matrix", "mat")
  m
}


#' Fill a Matrix
#'
#' Create a matrix filled with the value \code{x}.
#' 
#' @param x The (single) value to fill the matrix with.
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @param ... Further dimensions of the array.
#' @return A matrix or array filled with the value \code{x}. If the result is a
#'         matrix, it will have class \code{c("matrix", "mat")}.
#' @seealso \code{\link{ones}}, \code{\link{zeros}}, \code{\link{falses}}, 
#'   \code{\link{trues}}, \code{\link{mat}}, \code{\link{matrix}}.
#' @export
#' @examples
#' fill(pi, 3, 5)  # 3-by-5 matrix filled with the value of pi
#' fill(pi, 3, 5, 2, 2)  # 3-by-5-by-2-by-2 array filled with the value of pi
#' pi * ones(3, 5)
fill <- function(x, nrow = 1, ncol = 1, ...) {
  if (length(list(...)) == 0) {
    m <- matrix(x, nrow = nrow, ncol = ncol)
    class(m) <- c("matrix", "mat")
    m
  } else {
    array(x, dim = c(nrow, ncol, unlist(list(...))))
  }
}


#' @rdname fill
#' @export
falses <- function(nrow = 1, ncol = 1, ...) {
  fill(FALSE, nrow = nrow, ncol = ncol, ...)
}

#' @rdname fill
#' @export
trues <- function(nrow = 1, ncol = 1, ...) {
  fill(TRUE, nrow = nrow, ncol = ncol, ...)
}

#' @rdname fill
#' @export
ones <- function(nrow = 1, ncol = 1, ...) {
  fill(1L, nrow = nrow, ncol = ncol, ...)
}

#' @rdname fill
#' @export
zeros <- function(nrow = 1, ncol = 1, ...) {
  fill(0L, nrow = nrow, ncol = ncol, ...)
}


#' Flatten Matrices/Arrays
#'
#' Flatten (i.e., collapse) a matrix or array to one dimension.
#' 
#' @param x A matrix object.
#' @param across Character string specifying whether to flatten the matrix 
#'   across \code{"rows"} (default) or \code{"columns"}. This option is ignored
#'   for multi-way arrays.
#' @return A numeric vector.
#' @seealso \code{\link{mat}}.
#' @export
#' @examples
#' m <- mat("2, 4, 6, 8; 10, 12, 14, 16")
#' flatten(m)
#' flatten(m, across = "columns")
flatten <- function(x, across = c("rows", "columns")) {
  if (is.matrix(x)) {
    if (is.mat(x)) class(x) <- "matrix"  # remove "mat" class
    across <- match.arg(across)
    if (across == "rows") x <- t(x)
  } 
  dim(x) <- NULL  # remove dimension attribute
  x
}


#' Matrix Inverse
#' 
#' Calculates the inverse of a square matrix.
#' 
#' @param x A square numeric or complex matrix
#' @param ... Additional optional arguments.
#' @seealso \code{\link{solve}}.
#' @details See the documentation for the \code{base} function 
#'         \code{\link{solve}}.
#' @export
#' @examples
#' m <- 3 * eye(5)
#' inv(m)
inv <- function(x, ...) {
  if (!is.matrix(x)) {
    stop('Argument should be a matrix.', call. = FALSE)
  }
  if (dim(x)[1L] != dim(x)[2L]) {
    stop('Argument should be a square matrix.', call. = FALSE)
  }
  b <- diag(1, nrow(x))
  colnames(b) <- rownames(x)
  m <- solve(x, b, ...)
  class(m) <- c("matrix", "mat")
  m
}


#' Concatenate Matrices
#' 
#' Concatenate matrices along the first or second dimension.
#' 
#' @param ... Vectors or matrices.
#' @return A matrix of class \code{c("matrix", "mat")} formed by combining the
#'          \code{...} arguments column-wise (\code{hcat}) or row-wise 
#'          (\code{vcat}).
#' @seealso \code{\link{bmat}}, \code{\link{cbind}}, \code{\link{rbind}}.
#' @export
#' @examples
#' m1 <- mat("1, 2, 3; 4, 5, 6")
#' m2 <- mat("7, 8, 9; 10, 11, 12")
#' hcat(m1, m2)  # same as 'bmat("m1, m2")'
#' vcat(m1, m2)  # same as 'bmat("m1; m2")'
hcat <- function(...) {
  m <- do.call(cbind, list(...))
  class(m) <- c("matrix", "mat")
  m
}

#' @rdname hcat
#' @export
vcat <- function(...) {
  m <- do.call(rbind, list(...))
  class(m) <- c("matrix", "mat")
  m
}


#' Linearly-spaced Elements
#' 
#' Construct a vector of \code{n} linearly-spaced elements from \code{a} 
#' to \code{b}. 
#' 
#' @param a The starting value of the sequence.
#' @param b The final value of the sequence.
#' @param n The number of samples to generate. Default is 50.
#' @return A vector of linearly-spaced elements.
#' @seealso \code{\link{logspace}}, \code{\link{seq}}.
#' @export
#' @examples
#' linspace(0, 1)
#' linspace(1, 5, 5)
#' linspace(1+2i, 10+10i, 8)
#' logspace(0, pi, 10)
linspace <- function(a, b, n = 50) {
  seq(from = a, to = b, length.out = n)
}


#' Logarithmically-spaced Elements
#' 
#' Construct a vector of \code{n} logarithmically-spaced elements from 
#' 10^\code{a} to 10^\code{b}. 
#' 
#' @param a \code{base^a} is the starting value of the sequence.
#' @param b \code{base^b} is the final value of the sequence.
#' @param n The number of samples to generate. Default is 50.
#' @param base The base of the log space.
#' @return A vector of logarithmically-spaced elements.
#' @note
#' If \code{b = pi} and \code{base = 10}, the points are between 
#' \code{10^a} and \code{pi}, not \code{10^a} and \code{10^pi}, for 
#' compatibility with the corresponding MATLAB/Octave, and NumPy functions.
#' @seealso \code{\link{linspace}}, \code{\link{seq}}.
#' @export
logspace <- function(a, b, n = 50, base = 10) {
  if (b == pi && base == 10)  {
    b <- log(b, base = base)
  }
  base ^ seq(from = a, to = b, length.out = n)
}


#' Matrix/Array of Uniform Random Numbers
#' 
#' Construct a matrix or multi-way array of uniform random deviates.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @param ... Further dimensions of the array.
#' @param min Lower limit for the uniform distribution. Must be finite. 
#'   (\code{rand} only).
#' @param max Upper limit for the uniform distribution. Must be finite. 
#'   (\code{rand} only).
#' @return A  matrix or array of pseudorandom numbers. If the result is a
#'         matrix, it will have class \code{c("matrix", "mat")}.
#' @seealso \code{\link{randi}}, \code{\link{randn}}, \code{\link{runif}}.
#' @export
#' @examples
#' rand(100, 100)  # 100 by 100 matrix of uniform random numbers
#' rand(2, 3, min = 100, max = 200)  
rand <- function(nrow = 1, ncol = 1, ..., min = 0, max = 1) {
  if (length(list(...)) == 0) {
    m <- matrix(runif(nrow * ncol, min = min, max = max), nrow = nrow, 
                ncol = ncol)
    class(m) <- c("matrix", "mat")
    m
  } else {
    array(runif(nrow * ncol * prod(unlist(list(...))), min = min, max = max), 
          dim = c(nrow, ncol, unlist(list(...))))
  }
  
}


#' Matrix/Array of Uniform Random Integers
#' 
#' Construct a matrix or multi-way array of uniform random integers.
#' 
#' @param imax A positive integer.
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @param ... Further dimensions of the array.
#' @return A matrix or array of pseudorandom numbers. If the result is a matrix, 
#'         it will have class \code{c("matrix", "mat")}.
#' @seealso \code{\link{rand}}, \code{\link{randn}}, \code{\link{sample}}.
#' @export
#' @examples
#' randi(2, 5, 5)
randi <- function(imax, nrow, ncol = 1, ...) {
  if (!is.integer(imax)) imax <- as.integer(imax)
  if (imax < 1)  {  # make sure imax is a positive integer
    stop("imax must be a positive integer.") 
  }
  if (length(list(...)) == 0) {
    m <- matrix(sample(imax, size = nrow * ncol, replace = TRUE), nrow = nrow, 
                ncol = ncol)
    class(m) <- c("matrix", "mat")
    m
  } else {
    array(sample(imax, size = nrow * ncol * prod(unlist(list(...))), 
                 replace = TRUE), dim = c(nrow, ncol, unlist(list(...))))
  }
}


#' Matrix/Array of Normal Random Numbers
#' 
#' Construct a matrix or multi-way array of normal random deviates.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @param ... Further dimensions of the array.
#' @param mean Mean for the normal distribution. (\code{randn} only).
#' @param sd Standard deviation for the normal distribution. 
#'   (\code{randn} only).
#' @return A  matrix or array of pseudorandom numbers. If the result is a 
#'         matrix, it will have class \code{c("matrix", "mat")}.
#' @seealso \code{\link{rand}}, \code{\link{randi}}, \code{\link{rnorm}}.
#' @export
#' @examples
#' randn(100, 100)  # 100 by 100 matrix of standard normal random variates
#' randn(2, 3, mean = 10, sd = 0.1)
randn <- function(nrow = 1, ncol = 1, ..., mean = 0, sd = 1) {
  if (length(list(...)) == 0) {
    m <- matrix(rnorm(nrow * ncol, mean = mean, sd = sd), nrow = nrow, 
                ncol = ncol)
    class(m) <- c("matrix", "mat")
    m
  } else {
    array(rnorm(nrow * ncol * prod(unlist(list(...))), mean = mean, sd = sd), 
          dim = c(nrow, ncol, unlist(list(...))))
  }
}


#' Resize Matrices and Arrays
#' 
#' Change shape and size of a matrix or array.
#' 
#' @param x A matrix or multi-way array.
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @param ... Further dimensions of the array.
#' @param across Character string specifying whether to flatten the matrix 
#'               across \code{"rows"} (default) or \code{"columns"}. This option
#'               is ignored for multi-way arrays.
#' @param byrow Logical. If FALSE (default) the new matrix is filled by columns, 
#'                       otherwise it is filled by rows. This option is ignored
#'                       for multi-way arrays.
#'              
#' @return A matrix of class \code{c("matrix", "mat")} with dimension 
#'         \code{nrow}-by-\code{ncol}.
#' @seealso \code{\link{flatten}}, \code{\link{mat}}, \code{\link{matrix}}.
#' @export
#' @examples
#' m <- 1:9
#' resize(m)
#' resize(m, 3, 3)
#' resize(m, 2, 2)
resize <- function(x, nrow, ncol, ..., across = c("rows", "columns"), 
                   byrow = FALSE) {
  
  ## Make sure x is a matrix. If it's not, try converting it.
  if (!is.matrix(x)) x <- as.matrix(x)
  if (missing(nrow)) nrow <- dim(x)[1L]  # keep first dimension
  if (missing(ncol)) ncol <- dim(x)[2L]  # keep second dimension
  
  # Flatten and reshape/resize matrix.
  across <- match.arg(across)
  if (length(list(...)) == 0) {
    m <- matrix(flatten(x, across = across), nrow = nrow, ncol = ncol, 
                byrow = byrow)
    class(m) <- c("matrix", "mat")
    m
  } else {
    dim(x) <- c(nrow, ncol, unlist(list(...)))
  }
  x

}


#' Dimensions of a Matrix/Array
#' 
#' Retrieve the dimensions of a matrix or array.
#' 
#' @param x A matrix, array, or data frame.
#' @return The dimensions of the object.
#' @export
#' @seealso \code{\link{dim}}.
#' @examples
#' m <- mat("1, 3, 5; 7, 9, 11")
#' size(m)
size <- function(x) {
  dim(x)
}
