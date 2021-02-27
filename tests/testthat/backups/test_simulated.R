context("Testing simulated.R function")

test_that('data types, dimension of test data', {
  expect_is(simulated,'data.frame')
  expect_is(colnames(simulated), 'character')
  expect_equal(nrow(simulated), 150)
  expect_equal(ncol(simulated), 21)
})

