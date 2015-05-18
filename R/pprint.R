#' Pretty Printing
#' 
#' Prettier printing for matrices and data frames.
#' 
#' @param x An object of class \code{"matrix"} or \code{"data.frame"}.
#' @param rowdots Abc.
#' @param coldots Def.
#' @param digits The minimum number of significant digits to be printed in 
#'   values.
#' @param ... Additional optional arguments. None are used at present.
#' @export
#' @examples
#' pprint(randn(100, 100))
pprint <- function(x, ...) {
  UseMethod("pprint")
}


#' @rdname pprint
#' @method pprint matrix
#' @export
pprint.matrix <- function(x, rowdots = NULL, coldots = NULL, digits = NULL, 
                          ...) {
  
  # Default values
  if (is.null(rowdots)) rowdots <- getOption("pprint.rowdots")
  if (is.null(coldots)) coldots <- getOption("pprint.coldots")
  if (is.null(digits)) digits <- getOption("digits")
  
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
    sprintf(paste0("%.", digits, "f"), x)
  }
  dim(charx) <- dim(x)
  
  # Case 1: rows and columns do not have dots
  if (nrow(x) <= rowdots + 1 && ncol(x) <= coldots + 1) {
    res <- x  
  }
  
  # Case 2: rows have dots, columns do not
  if (nrow(x) > rowdots + 1 && ncol(x) <= coldots + 1) {
    res <- rbind(as.matrix(charx[seq_len(rowdots - 1), ]), 
                 rep("...", ncol(charx)),
                 charx[nrow(charx), ])
    row_labels <- add_dots(row_labels, pos = rowdots) 
  }
  
  # Case 3: rows do not have dots, columns have dots
  if (nrow(x) <= rowdots + 1 && ncol(x) > coldots + 1) {
    res <- t(apply(charx, 1, add_dots, pos = coldots))
    col_labels <- add_dots(col_labels, pos = coldots)
  }
  
  # Case 4: rows and columns have dots
  if (nrow(x) > rowdots + 1 && ncol(x) > coldots + 1) {
    # Add first rowdots-1 rows
    smallx <- t(apply(charx[seq_len(rowdots - 1), ], 1, add_dots, 
                      pos = coldots))
    res <- rbind(smallx, 
                 rep("...", ncol(smallx)),
                 add_dots(charx[nrow(charx), ], pos = coldots))
    row_labels <- add_dots(row_labels, pos = rowdots)
    col_labels <- add_dots(col_labels, pos = coldots)
  } 
  
  # Print "pretty" matrix
  cat(desc_mat(x), "\n")
  cat("\n")
  prmatrix(res, rowlab = row_labels, collab = col_labels, quote = FALSE, 
           right = TRUE)
  
  # Return a (temporarily) invisible copy of x
  invisible(x)
  
}