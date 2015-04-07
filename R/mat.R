#' Matrices
#'
#' Like \code{matrix}, \code{mat} creates a matrix from the given set of 
#' values. However, these values can also be represented by a character string, 
#' or a list of vectors.
#' 
#' @param x A data vector, character string, or a list.
#' @param rows Logical. If TRUE (the default) the matrix is filled by rows, 
#'             otherwise the matrix is filled by columns.
#' @param sep Separator string. Values within each row/column of x are 
#'            separated by this string. Default is \code{","}.
#' @param ... Aditional optional arguments.
#' 
#' @seealso \code{\link{bmat}}, \code{\link{dmat}}, \code{\link{matrix}}.
#' 
#' @examples
#' ## Using character vectors
#' mat('1, 2, 3, 4; 5, 6, 7, 8')  # ";" separates rows
#' mat('1, 2, 3, 4; 5, 6, 7, 8', rows = FALSE)  # ";" separates columns
#' mat("1 2 3 4; 5 6 7 8", sep = " ")  # use spaces instead of commas
#' mat(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2, byrow = TRUE)  # works like matrix too
#'
#' ## Using a list
#' z1 <- list(1:5, 6:10)
#' z2 <- list(a = 1:5, b = 6:10)
#' mat(z1)
#' mat(z2)  # preserves names as row names
#' mat(z2, rows = FALSE)  # preserves names as column names
mat <- function(x, ...) {
  UseMethod("mat")
}

#' @rdname mat
#' @export
#' @method mat default
mat.default <- function(x, ...) {
  m <- matrix(x, ...)  # default to base matrix function
  class(m) <- c("matrix", "mat")
}

#' @rdname mat
#' @export
#' @method mat character
mat.character <- function(x, rows = TRUE, sep = ",", ...) {
  
  ## Gather rows and individual values
  vecs <- unlist(strsplit(x, split = ";"))  # column/row vectors
  char_vals <- unname(unlist(lapply(vecs, strsplit, split = sep)))
  num_vals <- unlist(lapply(char_vals, function(x) eval(parse(text = x))))
  
  ## Form matrix from parsed values by calling R's built-in matrix function
  m <- if (rows) {
    matrix(num_vals, nrow = length(vecs), byrow = TRUE, ...)
  } else {
    matrix(num_vals, ncol = length(vecs), byrow = FALSE, ...)
  }
  class(m) <- c("matrix", "mat")
  m
  
}

#' @rdname mat
#' @export
#' @method mat list
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
  m <- if (rows) do.call(rbind, x) else do.call(cbind, x)
  class(m) <- c("matrix", "mat")
  m
  
}


#' Replace with Dots
#' 
#' Replace elements of a vector with \code{...}
#' 
#' @keywords internal
add_dots <- function(x, pos = 3) {
  if (length(x) >= pos + 2) {
    c(x[1:(pos-1)], "...", x[length(x)])
  } else {
    x
  }
}


#' Print Matrices, New-Style
#' 
#' A new method for printing matrices.
#' 
#' @param x A matrix.
#' @param dot.row Integer indicating which row to replace with \code{...}.
#' @param dot.col Integer indicating which column to replace with \code{...}.
#' @param digits Minimal number of significant digits.
#' 
#' @export
#' 
#' @examples
#' m <- randn(100, 100)
#' m
#' print(m, dot.row = 5, dot.col = 5, digits = 2)
print.mat <- function(x, dot.row = 3, dot.col = 3, digits) {
  
  cat("\n")
  cat(paste(dim(x), collapse = " by "), " matrix of ", paste0(typeof(x), "s"), 
      "\n", sep = "")
  
  # Convert to character matrix (after rounding, if appropriate)
  charx <- if (typeof(x) %in% c("integer", "logical")) {
    as.character(x)
  } else {
    if (missing(digits)) digits <- 4
    sprintf(paste0("%.", digits, "f"), x)
  }
  dim(charx) <- dim(x)
  
  # Case 1: rows and columns do not have dots
  if (nrow(x) <= dot.row + 1 && ncol(x) <= dot.col + 1) {
    newx <- x  
    row_labels <- paste0("[", seq_len(nrow(charx)), ",]")
    col_labels <- paste0("[,", seq_len(ncol(charx)), "]")
  }
  
  # Case 2: rows have dots, columns do not
  if (nrow(x) > dot.row + 1 && ncol(x) <= dot.col + 1) {
    res <- rbind(as.matrix(charx[seq_len(dot.row - 1), ]), 
                 rep("...", ncol(charx)),
                 charx[nrow(charx), ])
    row_labels <- c(paste0("[", seq_len(dot.row - 1), ",]"),
                    "",
                    paste0("[", nrow(charx), ",]")) 
    col_labels <- paste0("[,", seq_len(ncol(charx)), "]")
  }
  
  # Case 3: rows do not have dots, columns have dots
  if (nrow(x) <= dot.row + 1 && ncol(x) > dot.col + 1) {
    res <- t(apply(charx, 1, add_dots, pos = dot.col))
    row_labels <- paste0("[", seq_len(nrow(charx)), ",]")
    col_labels <- c(paste0("[,", seq_len(dot.col - 1), "]"),
                    "",
                    paste0("[,", ncol(charx), "]")) 
  }
  
  # Case 4: rows and columns do not have dots
  if (nrow(x) > dot.row + 1 && ncol(x) > dot.col + 1) {
    # Add first dot.row-1 rows
    res <- rbind(t(apply(charx[seq_len(dot.row - 1), ], 1, add_dots, 
                          pos = dot.col)), 
                 rep("...", ncol(smallx)),
                 add_dots(charx[nrow(charx), ], pos = dot.col))

    row_labels <- c(paste0("[", seq_len(dot.row - 1), ",]"),
                    "",
                    paste0("[", nrow(charx), ",]")) 
    col_labels <- c(paste0("[,", seq_len(dot.col - 1), "]"),
                    "",
                    paste0("[,", ncol(charx), "]")) 
  } 
  
  # Print "pretty" matrix
  prmatrix(res, rowlab = row_labels, collab = col_labels, quote = FALSE, 
           right = TRUE)
  
}