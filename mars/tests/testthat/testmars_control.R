test_that('mars.control() returns a valid control object', {
  mc <- mars.control(Mmax = 10)
  expect_s3_class(mc, 'mars.control')
  expect_equal(mc$Mmax, 10L)
  expect_equal(mc$d, 3)
  expect_equal(mc$trace, FALSE)
})
