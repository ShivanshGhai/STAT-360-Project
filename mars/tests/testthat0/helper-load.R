if (!requireNamespace("mars", quietly = TRUE)) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Package 'mars' is not installed, and 'pkgload' is not available to load it from source.")
  }
  pkgload::load_all("../..", export_all = FALSE, helpers = FALSE, quiet = TRUE)
}
