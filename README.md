ramify
================================================================================

Package `ramify` provides additional matrix functionality for R. It includes a wrapper for the base matrix function, extending its functionality by allowing 
matrices to be initialized from character strings and lists. A number of 
convenience functions have also been added for users more familiar with MATLAB,
Python, or Julia.

The main function in this package is `mat`, an extention to the built-in matrix function. Many convenience functions are also available:

  * `bmat` - creates a block matrix in a way similar to the character method of `mat`.
  * `dmat` - like `mat`, but returns a data frame instead of a matrix.
  * `eye` - creates an identity matrix
  * `hcat`/`vcat` - concatenate matrices
  * `fill` - fills a matrix with a particular value
  * `flatten` - flatten (i.e., collapse) a matrix to one dimension
  * `inv` - calculates the inverse of a square matrix
  * `linspace`/`logspace` - constructs a vector of linearly-spaced/logarithmically-spaced elements
  * `ones`/`zeros` - creates a matrix of all ones/zeros
  * `rand`/`randn` - creates a matrix of uniform/normally distributed random numbers
  * `resize` - resize a given matrix
  * `size` - returns the dimensions of a matrix
  * `trues`/`falses` - creates a matrix of all `TRUE`s/`FALSE`s
  
## Installation
`ramify` is not currently available on CRAN. In the mean time, download the development version from GitHub:
```r
## Assuming devtools is already installed
devtools::install_github("w108bmg/ramify")
```
