ramify
================================================================================

Package `ramify` provides additional matrix functionality for R. It includes a wrapper for the base matrix function, extending its functionality by allowing 
matrices to be initialized from character strings and lists. A number of 
convenience functions have also been added for users more familiar with MATLAB,
Python, or Julia.

The main function in this package is `mat`, an extention to the built-in matrix function. Many convenience functions are also available:

  * `bmat` - like `mat`, but constructs a block matrix.
  * `dmat` - like `mat`, but returns a data frame instead of a matrix.
  * `argmax`/`argmin` - find the position of the max/min in each row or column of a matrix
  * `eye` - construct an identity matrix
  * `hcat`/`vcat` - concatenate matrices
  * `fill` - fill a matrix or array with a particular value
  * `flatten` - flatten (i.e., collapse) a matrix or array to one dimension
  * `inv` - calculate the inverse of a square matrix
  * `linspace`/`logspace` - construct a vector of linearly-spaced/logarithmically-spaced elements
  * `ones`/`zeros` - construct a matrix or array of all ones/zeros
  * `rand`/`randi`/`randn` - construct a matrix or array of uniformly/normally distributed random numbers
  * `resize` - change the size and shape of a given matrix or array
  * `size` - extract the dimensions of a matrix or array.
  * `trues`/`falses` - construct a matrix or array of all `TRUE`s/`FALSE`s
  
## Installation
You can install the latest release from CRAN:
```r
install.packages("ramify")
```

The development version is hosted on GitHub at https://github.com/bgreenwell/ramify. You can download the development version using [`devtools`](https://github.com/hadley/devtools):
```r
# Assuming devtools is already installed
devtools::install_github("bgreenwell/ramify")
```
Bug reports should be submitted to https://github.com/bgreenwell/ramify/issues.