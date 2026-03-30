# STAT 360 Project — mars Package

**Author:** Shivansh Ghai  
**Student ID:** 301549429  

## Overview

This repository contains the `mars` R package — an educational implementation of the Multivariate Adaptive Regression Splines (MARS) algorithm for STAT 360.

## Repository Structure

- `mars/` — the R package
- `documentation_submission.qmd` — package documentation
- `bonus_fwd_stepwise_explainer.qmd` — forward stepwise explainer
- `bonus_bwd_stepwise_explainer.qmd` — backward stepwise explainer
- `SUBMISSION_CHECKLIST.qmd` — submission checklist

## Running the Package
```r
pkgload::load_all("mars/")
testthat::test_dir("mars/tests/testthat/")
```
