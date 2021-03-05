context("Testing tweets.R function")

test_that('data types, dimension of test data', {
  expect_is(tweets,'data.frame')
  expect_equal(nrow(tweets), 1820)
  expect_equal(ncol(tweets), 2)
})
