context("Testing osd_data.R function")

test_that('data types, dimension of test data', {
  expect_is(osd_data,'data.frame')
  expect_equal(nrow(osd_data), 1465)
  expect_equal(ncol(osd_data), 3)
})

