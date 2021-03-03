context("Testing covid_keys.R function")

test_that('data types, dimension of test data', {
  expect_is(covid_keys,'data.frame')
  expect_equal(nrow(covid_keys), 16)
  expect_equal(ncol(covid_keys), 1)
})


