.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mat <- list(
    mat.sep = ",",
    pprint.rowdots = 5L,
    pprint.coldots = 5L,
    atleast_2d = FALSE
  )
  toset <- !(names(op.mat) %in% names(op))
  if(any(toset)) options(op.mat[toset])
  
  invisible()
}