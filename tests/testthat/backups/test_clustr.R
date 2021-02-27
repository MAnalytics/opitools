context("Testing clustr.R function")

test_that('data types, dimension of test data', {
  expect_is(clustr,'data.frame')
  expect_equal(nrow(clustr), 9)
})
