context("Testing policing_dtd.R function")

test_that('data types, dimension of test data', {
  expect_is(policing_dtd,'data.frame')
  expect_equal(nrow(policing_dtd), 110)
  expect_equal(ncol(policing_dtd), 1)
})
