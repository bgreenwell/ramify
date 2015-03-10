ramify
================================================================================

Package `ramify` provides additional matrix functionality for R. It includes a wrapper for the built-in matrix function, extending its functionality by allowing 
matrices to be initialized from character strings and lists. A number of 
convenience functions have also been added for users more familiar with MATLAB 
or Julia.

The main function in this package is `mat`, an extention to the built-in matrix function. Many convenience functions are also available:

  * `bmat` - creates a block matrix from individual matrices in a way similar to
             the character method of `mat`.
  * `dmat` - like `mat`, but instead returns a data frame.
  * `ones` - creates a matrix of all ones
  * `zeros` - creates a matrix of all zeros
  * `flatten` - flatten (i.e., collapse) a matrix to one dimension
  * `resize` - resize a given matrix
  * `trues` - creates a matrix of all `TRUE`s
  * `falses` - creates a matrix of all `FALSE`s
  * `eye` - creates an identity matrix
  * `linspace` - constructs a vector of linearly-spaced elements
  * `logspace` - constructs a vector of logarithmically-spaced elements
  * `fill` - fills a matrix with a particular value
  
## Installation
`ramify` is not currently available on CRAN. In the mean time, download the development version from GitHub:
```r
## Assuming devtools is already installed
devtools::install_github("w108bmg/ramify")
```
