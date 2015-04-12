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
> randn(1000, 1000)  # matrix filled with a million normal random deviates
# 1000 by 1000 matrix of doubles 
# 
#            [,1]    [,2]    [,3] ... [,1000]
# [1,]     0.0385 -0.0620 -0.0930 ...  1.9295
# [2,]    -2.5014  0.3277  1.1657 ... -0.6824
# [3,]    -0.1308 -0.9673  0.6388 ... -0.2852
# ...         ...     ...     ... ...     ...
# [1000,]  0.4980  0.6902  1.4920 ... -2.7023

> data(Boston, package = "MASS")  # Boston housing data
> as.mat(Boston)
# 506 by 14 matrix of doubles 
# 
#       crim      zn   indus ...    medv
# 1   0.0063 18.0000  2.3100 ... 24.0000
# 2   0.0273  0.0000  7.0700 ... 21.6000
# 3   0.0273  0.0000  7.0700 ... 34.7000
# ...    ...     ...     ... ...     ...
# 506 0.0474  0.0000 11.9300 ... 11.9000
```
This printing behavior was partially inspired by the `"tbl_df"` class for data frames provided by the [`dplyr`](http://cran.r-project.org/web/packages/dplyr/index.html) package.

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