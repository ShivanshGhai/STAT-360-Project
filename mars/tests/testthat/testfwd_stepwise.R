data(marstestdata)

test_that('fwd_stepwise() returns the expected structure', {
  x <- as.matrix(marstestdata[, setdiff(names(marstestdata), 'y')])
  y <- marstestdata$y
  out <- fwd_stepwise(y, x, mars.control(Mmax = 10))

  expect_type(out, 'list')
  expect_true(all(c('y', 'B', 'Bfuncs') %in% names(out)))
  expect_equal(length(out$Bfuncs), 11)
  expect_equal(ncol(out$B), 11)
  expect_equal(nrow(out$B), nrow(marstestdata))
  expect_equal(names(out$B), paste0('B', 0:10))
})
