data(marstestdata)

test_that('predict.mars() returns predictions of the right length', {
  fit <- mars(y ~ ., data = marstestdata, control = mars.control(Mmax = 10))
  pred1 <- predict(fit)
  pred2 <- predict(fit, newdata = marstestdata)

  expect_equal(length(pred1), nrow(marstestdata))
  expect_equal(length(pred2), nrow(marstestdata))
  expect_equal(pred1, pred2)
})
