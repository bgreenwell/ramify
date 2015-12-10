ramify
================================================================================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ramify)](http://cran.r-project.org/package=ramify)
[![Build Status](https://travis-ci.org/bgreenwell/ramify.svg?branch=master)](https://travis-ci.org/bgreenwell/ramify)
[![Coverage Status](https://img.shields.io/codecov/c/github/bgreenwell/ramify.svg)](https://codecov.io/github/bgreenwell/ramify?branch=master)
[![Downloads](http://cranlogs.r-pkg.org/badges/ramify)](http://cranlogs.r-pkg.org/badges/ramify)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ramify)](http://cranlogs.r-pkg.org/badges/grand-total/ramify)

Package `ramify` provides additional matrix functionality and tools including: (1) wrappers for the base matrix function that allow matrices to be created from character strings and lists (the former is especially useful for creating block matrices), (ii) a generic "pretty print" `pprint` for printing large matrices to the screen or console, and (iii) a number of convenience functions for users more familiar with other scientific languages like Julia, MATLAB/Octave, or Python+NumPy.

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
