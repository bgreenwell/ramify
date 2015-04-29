#' Pretty Printing
#' 
#' Prettier printing for matrices and data frames.
#' 
#' @param x An object of class \code{"matrix"} or \code{"data.frame"}.
#' @param dot.row Abc.
#' @param dot.col Def.
#' @param digits The minimum number of significant digits to be printed in 
#'   values.
#' @export
#' @examples
#' pprint(randn(100, 100))
pprint <- function(x, ...) {
  UseMethod("pprint")
}


#' @rdname pprint
#' @method pprint matrix
#' @export
pprint.matrix <- function(x, dot.row = 4, dot.col = 4, digits = NULL, ...) {
  
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
    if (is.null(digits)) digits <- getOption("digits")
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
  prmatrix(res, rowlab = row_labels, collab = col_labels, quote = FALSE, 
           right = TRUE)
  
  # Return a (temporarily) invisible copy of x
  invisible(x)
  
}
