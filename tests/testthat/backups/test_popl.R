context("Testing popl.R function")

test_that('data types, dimension of test data', {
  expect_is(popl,'data.frame')
  expect_is(colnames(popl), 'character')
  expect_equal(nrow(popl), 11)
  expect_equal(ncol(popl), 3)
})

test_that('id_field complete and unique', {
  expect_equal(length(unique(popl$location_id)), 11)
  expect_identical(popl$location_id,
                   na.omit(popl$location_id))
})


