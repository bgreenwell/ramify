ramify
================================================================================

Package `ramify` provides additional matrix functionality for R. It includes a wrapper for the built-in matrix function, extending its functionality by allowing 
matrices to be initialized from character strings and lists. A number of 
convenience functions have also been added for users more familiar with MATLAB 
or Julia.

The main functions in this package are:

  * `mat` --- an extention to the built-in matrix function
  * `ones` --- creates a matrix of all ones
  * `zeros` --- creates a matrix of all zeros
  * `flatten` --- flatten (i.e., collapse) a matrix to one dimension
  * `resize` --- resize a given matrix
  
## Installation
`ramify` is not currently available on CRAN. In the mean time, download the development version from GitHub:
```r
## Assuming devtools is already installed
devtools::install_github("w108bmg/ramify")
```
