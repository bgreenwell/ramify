.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mat <- list(
    mat.sep = ",",
    mat.rowdots = 4L,
    mat.coldots = 4L
  )
  toset <- !(names(op.mat) %in% names(op))
  if(any(toset)) options(op.mat[toset])
  
  invisible()
}