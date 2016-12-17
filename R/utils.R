#' @keywords internal
add_dots <- function(x, pos = 3) {
  if (length(x) > pos) {
    c(x[seq_len(pos-1)], "...", x[length(x)])
  } else {
    x
  }
}


#' @keywords internal
desc_mat <- function(x) {
  # paste(paste(dim(x), collapse = " by "), "matrix of", paste0(typeof(x), "s"))
  paste(paste(dim(x), collapse = " x "), "matrix of", paste0(typeof(x), "s:"))
}
