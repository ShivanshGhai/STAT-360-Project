# mars: Multivariate Adaptive Regression Splines in R

`mars` is a lightweight R package implementing Multivariate Adaptive Regression Splines from first principles. It provides forward basis construction, backward pruning, prediction, diagnostics, and standard S3 interfaces in a compact codebase intended for learning and experimentation.

## Highlights

- Formula-based `mars(formula, data, control)` fitting interface
- Forward stepwise hinge-basis construction
- Backward pruning with a penalized lack-of-fit criterion
- S3 methods for `print`, `summary`, `predict`, `plot`, and `anova`
- C++ hinge-function integration through Rcpp
- Unit tests and reproducible worked examples

## Repository Structure

- `mars/`: installable R package source
- `bonus_fwd_stepwise_explainer.qmd`: forward-selection walkthrough
- `bonus_bwd_stepwise_explainer.qmd`: backward-pruning walkthrough
- `PDFs/`: rendered algorithm explainers

## Run Locally

From the repository root:

```r
pkgload::load_all("mars/")
testthat::test_dir("mars/tests/testthat/")
source("mars/test.R")
```

To install the package:

```r
remotes::install_local("mars/")
```

## Scope

This is an educational implementation designed to make the main MARS algorithm understandable and testable. It does not attempt to reproduce every optimization or feature available in production libraries.

## Author

Shivansh Ghai
