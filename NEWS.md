# NEWS/Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0 - 2025-06-23

### Added
- A `CHANGELOG.md` file to track changes to the package.
- A `.Rbuildignore` file to ensure a clean package build process.
- Enabled and corrected unit tests for `bmat()` to ensure its functionality is verified.

### Changed
- Migrated all package documentation from standard `roxygen2` tags to markdown syntax for improved readability and maintenance.
- Modernized the `DESCRIPTION` file, adding a `VignetteBuilder` field to ensure vignettes build correctly and resolving `R CMD check` warnings.
- "Beefed up" the `README.md` file with a more comprehensive overview, usage examples, and modern formatting.
- Updated `.gitignore` with more robust, standard rules for R package development.
- Updated package-level documentation to use the `_PACKAGE` sentinel, resolving a `roxygen2` deprecation warning.

### Fixed
- The `bmat()` function now correctly evaluates matrix objects in the calling environment, resolving a scoping issue that caused `testthat` failures.
- The `mat()` function now correctly trims whitespace from parsed values, ensuring proper conversion to numeric type.
- The `mat()` function now correctly handles whitespace as a separator when `sep = ""`.
- Resolved a `CRAN` check `NOTE` by correcting a cross-package link in the `matrix_rank()` documentation.

## [0.3.3] - 2016-11-20

### Fixed
- Fixed a bug in `mat.character()` where `eval = TRUE` was not working properly.

## [0.3.2] - 2016-08-27

### Changed
- Improved printing of large matrices.
- The `mat` function can now handle character strings with R expressions (e.g., `"rnorm(10)"`).
- Added a new vignette discussing the package's matrix functionality.

### Removed
- Removed dependency on the `Matrix` package.

## [0.2.1] - 2015-10-26

### Added
- Added `dmat()` for creating data frames and `bmat()` for creating block matrices.
- Added a number of convenience functions (`argmax`, `argmin`, `eye`, `fill`, etc.) for users familiar with MATLAB/NumPy.

## [0.1.0] - 2015-06-08

### Added
- Initial release of `ramify`.
- Added `mat()` function for creating matrices from character strings or lists.
- Added `pprint()` for pretty-printing large matrices.