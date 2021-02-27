context("Testing remove_rows_n.R function")

test.data2 <- traj

ch_1 <- remove_rows_n(test.data2, remove=1)
ch_2 <- remove_rows_n(test.data2, remove=2)
ch_3 <- remove_rows_n(test.data2, remove=3)

test_that('check removal operation is correct', {
  expect_equal(ch_1$totalRowsRemoved, 4)
  expect_equal(ch_2$totalRowsRemoved, 3)
  expect_equal(ch_3$totalRowsRemoved, 6)
  expect_equal(nrow(ch_1$CleanData), 6)
  expect_equal(nrow(ch_2$CleanData), 7)
  expect_equal(nrow(ch_3$CleanData), 4)
})

test_that('data types are correct', {
  expect_is(ch_1$CleanData,'data.frame')
  expect_is(ch_2$CleanData,'data.frame')
  expect_is(ch_3$CleanData,'data.frame')
  expect_is(ch_1$CleanData$location_ids, 'character')
  expect_is(ch_2$CleanData$location_ids, 'character')
  expect_is(ch_3$CleanData$location_ids, 'character')
  expect_is(ch_1$CleanData$X2001,'integer')
  expect_is(ch_2$CleanData$X2001,'integer')
  expect_is(ch_3$CleanData$X2001,'integer')
})


