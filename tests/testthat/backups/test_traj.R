context("Testing traj.R function")

test_that('data types, dimension of test data', {
  expect_is(traj,'data.frame')
  expect_is(colnames(traj), 'character')
  expect_equal(nrow(traj), 10)
  expect_equal(ncol(traj), 10)
})

test_that('id_field complete and unique', {
  expect_equal(length(unique(traj$location_id)), 10)
})

