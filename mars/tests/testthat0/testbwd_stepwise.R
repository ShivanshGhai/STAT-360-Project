data(marstestdata)

test_that('bwd_stepwise() prunes the forward model', {
  x <- as.matrix(marstestdata[, setdiff(names(marstestdata), 'y')])
  y <- marstestdata$y
  fwd <- fwd_stepwise(y, x, mars.control(Mmax = 10))
  bwd <- bwd_stepwise(fwd, mars.control(Mmax = 10))

  expect_type(bwd, 'list')
  expect_true(all(c('y', 'B', 'Bfuncs') %in% names(bwd)))
  expect_lt(ncol(bwd$B), ncol(fwd$B))
  expect_equal(nrow(bwd$B), nrow(marstestdata))
})
