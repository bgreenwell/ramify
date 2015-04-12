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


#' Print Matrices, New-Style
#' 
#' A new method for printing matrices.
#' 
#' @param x A matrix.
#' @param pretty.print Logical indicating whether to print a "prettier" version
#'        of the matrix.
#' @param dot.row Integer indicating which row to replace with \code{...}.
#' @param dot.col Integer indicating which column to replace with \code{...}.
#' @param digits Minimal number of significant digits.
#' @param ... Additional optional arguments passed onto \code{prmatrix}.
#' 
#' @export
#' 
#' @method print mat
#' @examples
#' m <- randn(100, 100)
#' m
#' print(m, dot.row = 5, dot.col = 5, digits = 2)
print.mat <- function(x, 
                      pretty.print = getOption("mat.pretty.print"),
                      dot.row = getOption("mat.dot.row"), 
                      dot.col = getOption("mat.dot.col"), digits, ...) {
  
  if (pretty.print) {  # nicer default printing
    
    # Row labels
    row_labels <- if (is.null(rownames(x))) {
      paste0("[", seq_len(nrow(x)), ",]")
    } else {
      rownames(x)
    }
    
    # Columns labels
    col_labels <- if (is.null(colnames(x))) {
      paste0("[,", seq_len(ncol(x)), "]")
    } else {
      colnames(x)
    }
    
    # Convert to character matrix (after rounding, if appropriate)
    charx <- if (typeof(x) == "character") {
      x
    } else if (typeof(x) %in% c("integer", "logical")) {
      as.character(x)
    } else {
      if (missing(digits)) digits <- 4
      sprintf(paste0("%.", digits, "f"), x)
    }
    dim(charx) <- dim(x)
    
    # Case 1: rows and columns do not have dots
    if (nrow(x) <= dot.row + 1 && ncol(x) <= dot.col + 1) {
      res <- x  
    }
    
    # Case 2: rows have dots, columns do not
    if (nrow(x) > dot.row + 1 && ncol(x) <= dot.col + 1) {
      res <- rbind(as.matrix(charx[seq_len(dot.row - 1), ]), 
                   rep("...", ncol(charx)),
                   charx[nrow(charx), ])
      row_labels <- add_dots(row_labels, pos = dot.row) 
    }
    
    # Case 3: rows do not have dots, columns have dots
    if (nrow(x) <= dot.row + 1 && ncol(x) > dot.col + 1) {
      res <- t(apply(charx, 1, add_dots, pos = dot.col))
      col_labels <- add_dots(col_labels, pos = dot.col)
    }
    
    # Case 4: rows and columns have dots
    if (nrow(x) > dot.row + 1 && ncol(x) > dot.col + 1) {
      # Add first dot.row-1 rows
      smallx <- t(apply(charx[seq_len(dot.row - 1), ], 1, add_dots, 
                        pos = dot.col))
      res <- rbind(smallx, 
                   rep("...", ncol(smallx)),
                   add_dots(charx[nrow(charx), ], pos = dot.col))
      row_labels <- add_dots(row_labels, pos = dot.row)
      col_labels <- add_dots(col_labels, pos = dot.col)
    } 
    
    # Print "pretty" matrix
    cat(desc_mat(x), "\n")
    cat("\n")
#     dimnames(x) <- list(row_labels, col_labels)
#     print(res, ...)
    prmatrix(res, rowlab = row_labels, collab = col_labels, quote = FALSE, 
             right = TRUE)
    
  } else {  # use default printing
    
    prmatrix(x, ...)
    
  }
  
  # Return a (temporarily) invisible copy of x
  invisible(x)
  
}


#' Coerce to a \code{"mat"} Object
#' 
#' Functions to check if an object is of class \code{"mat"}, or coerce it if 
#' possible.
#'
#' @param x A matrix or data frame.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @export
as.mat <- function(x, ...) {
  UseMethod("as.mat")
}


#' @rdname as.mat
#' @method as.mat default
#' @export
as.mat.default <- function (x, ...) {
  as.mat(as.matrix(x, ...))
}


#' @rdname as.mat
#' @method as.mat matrix
#' @export
as.mat.matrix <- function(x, ...) {
  class(x) <- c("matrix", "mat")
  x
}


#' @rdname as.mat
#' @method as.mat data.frame
#' @export
as.mat.data.frame <- function(x, ...) {
  m <- data.matrix(x, ...)
  class(m) <- c("matrix", "mat")
  m
}


#' @rdname as.mat
#' @export
is.mat <- function(x) {
  inherits(x, "mat")
}