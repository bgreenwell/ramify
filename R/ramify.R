#' ramify: Additional MatrIx FunctionalitY
#' 
#' Additional MatrIx FunctionalitY for R including: (1) wrappers
#' for the base matrix function that allows matrices to be created from 
#' character strings and lists (the former is especially useful for creating 
#' block matrices), (ii) better default printing of large matrices, and (iii) a 
#' number of convenience functions for users more familiar with other scientific 
#' languages like Julia, MATLAB/Octave, or Python.
#' 
#' The main functions in this package are:
#'   \itemize{
#'     \item \code{mat} - matrix wrapper function
#'     \item \code{bmat} - like the character method of \code{mat}, but for 
#'       block matrices
#'     \item \code{dmat} - like \code{mat}, but returns a data frame instead
#'     \item \code{argmax}/\code{argmin} - find the position of the max/min in 
#'           each row or column of a matrix
#'     \item \code{eye} - construct an identity matrix
#'     \item \code{hcat}/\code{vcat} - concatenate matrices
#'     \item \code{fill} - fill a matrix or array with a particular value
#'     \item \code{flatten} - flatten (i.e., collapse) a matrix or array to one 
#'           dimension
#'     \item \code{inv} - calculate the inverse of a square matrix
#'     \item \code{linspace}/\code{logspace} - construct a vector of linearly-
#'           spaced/logarithmically-spaced elements
#'     \item \code{meshgrid} - construct rectangular 2-D grids
#'     \item \code{ones}/\code{zeros} - construct a matrix or array of all 
#'           ones/zeros
#'     \item \code{rand}/\code{randi}/\code{randn} - construct a matrix or array 
#'           of uniformly/normally distributed random numbers
#'     \item \code{resize} - change the size and shape of a given matrix or 
#'           array
#'     \item \code{size} - extract the dimensions of a matrix or array.
#'     \item \code{tri}, \code{tril}, \code{triu} - constructing trinagular matrices
#'     \item \code{trues}/\code{falses} - construct a matrix or array of all 
#'           \code{TRUE}s/\code{FALSE}s
#'   }
#' 
#'  @docType package
#'  @name ramify
NULL