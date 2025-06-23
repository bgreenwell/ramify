# ramify

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ramify)](http://cran.r-project.org/package=ramify)
[![R-CMD-check](https://github.com/w108bmg/ramify/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/w108bmg/ramify/actions/workflows/R-CMD-check.yaml)
![](https://img.shields.io/badge/lifecycle-retired-orange.svg)

`ramify` is an R package that provides additional matrix functionality. Its goal is to make matrix creation and manipulation more convenient, especially for users familiar with scientific languages like MATLAB, Octave, or Python with NumPy.

## Core features

* **Intuitive matrix creation**: Create matrices from character strings or lists using `mat()`, which is useful for seeing the structure of the matrix in your code.
* **Block matrices**: Easily construct block matrices from existing matrix objects using `bmat()`.
* **Pretty-printing**: Display large matrices in the console in a compact and readable format with `pprint()`.
* **Convenience functions**: A suite of functions familiar to MATLAB and NumPy users, including `eye()`, `ones()`, `zeros()`, `linspace()`, `meshgrid()`, `repmat()`, and more.

## Installation

You can install the stable release from CRAN:
```r
install.packages("ramify")
```
Or, you can install the development version from GitHub:
```r
#install.packages("remotes")
remotes::install_github("bgreenwell/ramify")
```

## Usage

```r
library(ramify)

# Create a 2x3 matrix from a string.
mat("1, 2, 3; 4, 5, 6")
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    4    5    6

# Use spaces as a separator instead of commas.
mat("1 2 3; 4 5 6", sep = "")
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    4    5    6

# Create a 3x3 identity matrix.
eye(3)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1

# Create a 2x4 matrix of ones.
ones(2, 4)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    1    1    1
#> [2,]    1    1    1    1

# "Pretty" printing large matrices
pprint(randn(100, 100))
#> 100 x 100 matrix of doubles: 
#> 
#>              [,1]       [,2]      [,3] ...     [,100]
#> [1,]   -0.0918915 -1.6535266 1.9916211 ...  0.2545715
#> [2,]   -1.3707209  0.8595378 1.6227581 ...  0.3485438
#> [3,]    0.3271210 -0.4506809 2.3066801 ... -0.2320033
#> ...           ...        ...       ... ...        ...
#> [100,]  0.3407804  1.3619685 0.4093369 ... -0.3140292
```

## Bug reports

If you find a bug or have a feature request, please submit an issue on [GitHub](https://github.com/bgreenwell/ramify/issues).

## License

This package is licensed under the GPL-3.
