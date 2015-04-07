ramify
================================================================================

Package `ramify` provides additional matrix functionality for R. It includes a wrapper for the base matrix function, extending its functionality by allowing 
matrices to be initialized from character strings and lists. A number of 
convenience functions have also been added for users more familiar with MATLAB,
Python, or Julia.

The main function in this package is `mat`, an extention to the built-in matrix function. Many convenience functions are also available, for example:

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

Matrices created using the `ramify` package carry an additional class: `"mat"`. This is useful for printing purposes. Ordinary matrices created by `matrix` (for example) are printed completely on the screen which is not very helpful for large matrices. Matrices inheriting from class `"mat"`, however, are printed in a much more efficient way. For example,
```r
> library(ramify)
> randn(10, 10)
# 10 by 10 matrix of doubles 
#
#          [,1]    [,2]    [,3] ...   [,10]
# [1,]  -0.0336  0.5680  0.6069 ...  0.5275
# [2,]  -1.0799  1.2825  0.4396 ...  0.1739
# [3,]  -1.0831 -1.1032  0.0738 ... -1.2023
# ...       ...     ...     ... ...     ...
# [10,] -0.1007 -0.4649 -0.9833 ... -0.6580

> as.mat(mtcars)
# 32 by 11 matrix of doubles 
# 
#                   mpg    cyl     disp ...   carb
# Mazda RX4     21.0000 6.0000 160.0000 ... 4.0000
# Mazda RX4 Wag 21.0000 6.0000 160.0000 ... 4.0000
# Datsun 710    22.8000 4.0000 108.0000 ... 1.0000
# ...               ...    ...      ... ...    ...
# Volvo 142E    21.4000 4.0000 121.0000 ... 2.0000
```
This printing behavior was partially inspired by the `"tbl_df"` class for data frames provided by the `dplyr` package.

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