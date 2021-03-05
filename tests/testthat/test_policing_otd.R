context("Testing policing_otd.R function")

test_that('data types, dimension of test data', {
  expect_is(policing_otd,'data.frame')
  expect_equal(nrow(policing_otd), 110)
  expect_equal(ncol(policing_otd), 1)
})
