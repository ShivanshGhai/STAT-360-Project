if (!requireNamespace("mars", quietly = TRUE)) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Package 'mars' is not installed, and 'pkgload' is not available to load it from source.")
  }
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
}

cat("\n================ Example 1: Package sample data ================\n")
data("marstestdata")
mc <- mars.control(Mmax = 10)
fit <- mars(y ~ ., data = marstestdata, control = mc)
print(fit)
cat("\nFirst 10 fitted values:\n")
print(head(predict(fit), 10))
cat("\nModel summary:\n")
print(summary(fit))
cat("\nANOVA table:\n")
print(anova(fit))
plot(fit)

cat("\n================ Example 2: mtcars dataset ================\n")
mtcars_data <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]
fit_mtcars <- mars(mpg ~ disp + hp + wt + qsec, data = mtcars_data,
                   control = mars.control(Mmax = 8))
print(fit_mtcars)
cat("\nFirst 10 predictions for mtcars:\n")
print(head(predict(fit_mtcars, newdata = mtcars_data), 10))
cat("\nModel summary:\n")
print(summary(fit_mtcars))

cat("\n================ Example 3: longley dataset ================\n")
longley_data <- longley[, c("Employed", "GNP.deflator", "GNP", "Unemployed", "Population")]
fit_longley <- mars(Employed ~ GNP.deflator + GNP + Unemployed + Population,
                    data = longley_data,
                    control = mars.control(Mmax = 6, d = 3, trace = FALSE))
print(fit_longley)
cat("\nPredictions for longley:\n")
print(head(predict(fit_longley, newdata = longley_data), 10))
cat("\nModel summary:\n")
print(summary(fit_longley))
