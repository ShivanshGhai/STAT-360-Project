data(marstestdata)

test_that('mars() returns a mars object with lm behaviour', {
  fit <- mars(y ~ ., data = marstestdata, control = mars.control(Mmax = 10))

  expect_s3_class(fit, 'mars')
  expect_true('lm' %in% class(fit))
  expect_true(all(c('B', 'Bfuncs', 'x_names', 'coefficients') %in% names(fit)))
  expect_equal(length(fit$fitted.values), nrow(marstestdata))
})
