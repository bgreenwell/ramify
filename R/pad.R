prepend_const <- function(x, pad_amt, val, rows = TRUE) {
  
  # Return original matrix
  if (pad_amt == 0) {
    x
  }
  
  # Prepend matrix with constant
  if (is.null(dim(x))) {
    c(rep(val, times = pad_amt), x)
  } else {
    if (rows) {
      m <- fill(val, nrow = pad_amt, ncol = dim(x)[2L])
      rbind(m, x)
    } else {
      m <- fill(val, nrow = dim(x)[1L], ncol = pad_amt)
      cbind(m, x)
    }
  }
  
}


append_const <- function(x, pad_amt, val, rows = TRUE) {
  
  # Return original matrix
  if (pad_amt == 0) {
    x
  }
  
  # Prepend matrix with constant
  if (is.null(dim(x))) {
    c(x, rep(val, times = pad_amt))
  } else {
    if (rows) {
      m <- fill(val, nrow = pad_amt, ncol = dim(x)[2L])
      rbind(x, m)
    } else {
      m <- fill(val, nrow = dim(x)[1L], ncol = pad_amt)
      cbind(x, m)
    }
  }
  
}


prepend_edge <- function(x, pad_amt, rows = TRUE) {
  
  # Return original matrix
  if (pad_amt == 0) {
    x
  }
  
  # Prepend matrix with edge values
  if (is.null(dim(x))) {
    c(rep(x[1], times = pad_amt), x)
  } else {
    if (rows) {
      edge_slice <- x[1, , drop = FALSE]
      rbind(reprow(edge_slice, pad_amt), x)
    } else {
      edge_slice <- x[, 1, drop = FALSE]
      cbind(repcol(edge_slice, pad_amt), x)
    }
  }
  
}


append_edge <- function(x, pad_amt, rows = TRUE) {
  
  # Return original matrix
  if (pad_amt == 0) {
    x
  }
  
  # Prepend matrix with edge values
  if (is.null(dim(x))) {
    c(x, rep(x[length(x)], times = pad_amt))
  } else {
    if (rows) {
      edge_slice <- x[dim(x)[1L], , drop = FALSE]
      rbind(x, reprow(edge_slice, pad_amt))
    } else {
      edge_slice <- x[, dim(x)[2L], drop = FALSE]
      cbind(x, repcol(edge_slice, pad_amt))
    }
  }
  
}


prepend_mean <- function(x, pad_amt, stat_depth, rows = TRUE, na.rm = FALSE) {
  
  # Return original matrix
  if (pad_amt == 0) {
    x
  }
  
  # Equivalent to edge padding for single value, so do that instead
  if (stat_depth == 1) {
    prepend_edge(x, pad_amt, rows = rows)
  }
  
  # Use entire array if stat_depth is too large
  if (rows) {
    if (stat_depth > dim(x)[1L]) {
      stat_depth <- dim(x)[1L]
    }
    mean_slice <- apply(x[1:stat_depth, , drop = FALSE], MARGIN = 2, FUN = mean,
                        na.rm = na.rm)
    rbind(x, reprow(mean_slice, pad_amt))
    
  } else {
    if (stat_depth < dim(x)[2L]) {
      stat_depth <- dim(x)[2L]
    }
    mean_slice <- apply(x[, 1:stat_depth, drop = FALSE], MARGIN = 1, FUN = mean,
                        na.rm = na.rm)
    cbind(x, repcol(mean_slice, pad_amt))
  }
  
}


append_mean <- function(x, pad_amt, stat_depth, rows = TRUE, na.rm = FALSE) {
  
  # Return original matrix
  if (pad_amt == 0) {
    x
  }
  
  # Equivalent to edge padding for single value, so do that instead
  if (stat_depth == 1) {
    prepend_edge(x, pad_amt, rows = rows)
  }
  
  # Use entire array if stat_depth is too large
  if (rows) {
    if (stat_depth > dim(x)[1L]) {
      stat_depth <- dim(x)[1L]
    }
    mean_slice <- apply(x[(dim(x)[1L] - stat_depth + 1):dim(x)[1L], , drop = FALSE], 
                        MARGIN = 2, FUN = mean, na.rm = na.rm)
    rbind(reprow(mean_slice, pad_amt), x)
    
  } else {
    if (stat_depth < dim(x)[2L]) {
      stat_depth <- dim(x)[2L]
    }
    mean_slice <- apply(x[, (dim(x)[2L] - stat_depth + 1):dim(x)[2L], drop = FALSE],
                        MARGIN = 1, FUN = mean, na.rm = na.rm)
    cbind(repcol(mean_slice, pad_amt), x)
  }
  
}


reprow <- function(x, n) {
  matrix(rep(x, each = n), nrow = n)
}


repcol <- function(x, n) {
  matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}


#' Pad a Matrix
#' 
#' Pads a matrix, data frame, or other matrix-like object.
#' 
#' @param x An object that typically inherits from class \code{"matrix"}.
#' @param padding Character string specifying the type of padding. Default is
#'   \code{"constant"}.
#' @param pad_width Integer specifying the amount of padding. Default is 
#'   \code{1}.
#' @param constant_values Numeric constant to be used when 
#'   \code{padding = "constant"}.
#' @param end_values Currently ignored.
#' @param reflect_type Currently ignored.
#' @param stat_depth Number of values at edge of each axis used to calculate the 
#'   statistic value. Note that \code{stat_depth = 1} is equivalent to 
#'   \code{padding = "edge"}.
#' @param na.rm Logical indicating whether or not to remove \code{NA} values
#'   before computing any statistics. Default is \code{FALSE}.
#' @export
#' @examples 
#' m <- mat('1, 2; 3, 4')
#' pad(m, padding = "constant", constant_values = 0, pad_width = 2)
#' pad(m, padding = "edge", pad_width = 2)
#' image(pad(randn(10, 10), constant_values = NA, pad_width = 5))
pad <- function(x, padding = c("constant", "edge", "mean"), pad_width = 1, 
                constant_values = 0, end_values, reflect_type, stat_depth, 
                na.rm = FALSE) {
  
  padding <- match.arg(padding)
  
  # Pad array with constant values
  if (padding == "constant") {
    
    # Pad left and right sides of vector
    if (is.null(dim(x))) {
      
      # Pad left
      padx <- prepend_const(x, pad_amt = pad_width, val = constant_values)
      
      # Pad right
      padx <- append_const(padx, pad_amt = pad_width, val = constant_values)
      
    # Pad all four sides of matrix
    } else {
      
      # Pad matrix in a clockwise fashion starting from noon!
      
      # Pad top
      padx <- prepend_const(x, pad_amt = pad_width, val = constant_values, 
                            rows = TRUE)
      
      # Pad right
      padx <- append_const(padx, pad_amt = pad_width, val = constant_values, 
                           rows = FALSE)
      
      # Pad bottom
      padx <- append_const(padx, pad_amt = pad_width, val = constant_values, 
                           rows = TRUE)
      
      # Pad left
      padx <- prepend_const(padx, pad_amt = pad_width, val = constant_values, 
                            rows = FALSE)
      
    }
    
  }
  
  # Pad array with edge values
  if (padding == "edge") {
    
    # Pad left and right sides of vector
    if (is.null(dim(x))) {
      
      # Pad left
      padx <- prepend_edge(x, pad_amt = pad_width)
      
      # Pad right
      padx <- append_edge(padx, pad_amt = pad_width)
      
      # Pad all four sides of matrix
    } else {
      
      # Pad matrix in a clockwise fashion starting from noon!
      
      # Pad top
      padx <- prepend_edge(x, pad_amt = pad_width, rows = TRUE)
      
      # Pad right
      padx <- append_edge(padx, pad_amt = pad_width, rows = FALSE)
      
      # Pad bottom
      padx <- append_edge(padx, pad_amt = pad_width, rows = TRUE)
      
      # Pad left
      padx <- prepend_edge(padx, pad_amt = pad_width, rows = FALSE)
      
    }
    
  }
  
  # Pad array with mean values
  if (padding == "mean") {
    
    # Pad left and right sides of vector
    if (is.null(dim(x))) {
      
      # Pad left
      padx <- prepend_mean(x, pad_amt = pad_width, stat_depth = stat_depth, na.rm = na.rm)
      
      # Pad right
      padx <- append_mean(padx, pad_amt = pad_width, stat_depth = stat_depth - padwidth, na.rm = na.rm)
      
      # Pad all four sides of matrix
    } else {
      
      # Pad matrix in a clockwise fashion starting from noon!
      
      # Pad top
      padx <- prepend_mean(x, pad_amt = pad_width, stat_depth = stat_depth, rows = TRUE, 
                           na.rm = na.rm)
      
      # Pad right
      padx <- append_mean(padx, pad_amt = pad_width, stat_depth = stat_depth, rows = FALSE, 
                          na.rm = na.rm)
      
      # Pad bottom
      padx <- append_mean(padx, pad_amt = pad_width, stat_depth = stat_depth - pad_width, rows = TRUE, 
                          na.rm = na.rm)
      
      # Pad left
      padx <- prepend_mean(padx, pad_amt = pad_width, stat_depth = stat_depth - pad_width, rows = FALSE, 
                           na.rm = na.rm)
      
    }
    
  }
  
  # Return padded array
  padx
  
}
