.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mat <- list(
    mat.pretty.print = TRUE,
    mat.dot.row = 4L,
    mat.dot.col = 4L
  )
  toset <- !(names(op.mat) %in% names(op))
  if(any(toset)) options(op.mat[toset])
  
  invisible()
}