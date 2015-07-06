ramify
================================================================================

Package `ramify` provides `a`dditional `m`atr`i`x `f`unctionalit`y` for R including: (1) wrappers for the base matrix function that allows matrices to be created from character strings and lists (the former is especially useful for creating block  matrices), (ii) better default printing of large matrices, and (iii) a number of convenience functions for users more familiar with other scientific languages like Julia, MATLAB/Octave, or Python.

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
  * `meshgrid` - construct matrices for evaluating functions over rectangular 2-D grids (useful for contour plots)
  * `ones`/`zeros` - construct a matrix or array of all ones/zeros
  * `rand`/`randi`/`randn` - construct a matrix or array of uniformly/normally distributed random numbers
  * `resize` - change the size and shape of a given matrix or array
  * `size` - extract the dimensions of a matrix or array.
  * `trues`/`falses` - construct a matrix or array of all `TRUE`s/`FALSE`s

`ramify` introduces a pretty print function called `pprint`. This is a generic function. Ordinarily, matrices are printed completely on the screen which is not very helpful when the dimensions are large. Using the `pprint` function, however, allows you to see a more efficient printed version of the matrix. For example,
```r
> library(ramify)
> m <- randn(1000, 1000)  # matrix filled with a million normal random deviates
> pprint(m)  # compare this output to `print(m)` and `head(m)`
# 1000 by 1000 matrix of doubles 
# 
#            [,1]    [,2]    [,3] ... [,1000]
# [1,]     0.0385 -0.0620 -0.0930 ...  1.9295
# [2,]    -2.5014  0.3277  1.1657 ... -0.6824
# [3,]    -0.1308 -0.9673  0.6388 ... -0.2852
# ...         ...     ...     ... ...     ...
# [1000,]  0.4980  0.6902  1.4920 ... -2.7023
```
This can be useful for viewing large (numeric) data frames too:
```r
> data(Boston, package = "MASS")  # Boston housing data
> pprint(Boston)
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
You can install the latest stable release from CRAN:
```r
install.packages("ramify")
```
The development version is hosted on GitHub at https://github.com/bgreenwell/ramify and can be downloaded using [`devtools`](https://github.com/hadley/devtools):
```r
# Assuming devtools is already installed
devtools::install_github("bgreenwell/ramify")
```
Bug reports should be submitted to https://github.com/bgreenwell/ramify/issues.