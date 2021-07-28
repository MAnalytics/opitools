context("Testing covid_keys.R function")

test_that('data types, dimension of test data', {
  expect_is(covid_theme,'data.frame')
  expect_equal(nrow(covid_theme), 16)
  expect_equal(ncol(covid_theme), 1)
})


