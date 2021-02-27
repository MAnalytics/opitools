context("Testing TO1Risk.R function")

test_that('data types, dimension of test data', {
  expect_is(TO1Risk,'data.frame')
  expect_is(colnames(TO1Risk), 'character')
  expect_equal(nrow(TO1Risk), 378)
  expect_equal(ncol(TO1Risk), 31)
})

