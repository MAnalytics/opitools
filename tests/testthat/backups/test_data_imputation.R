context("Testing data_imputation.R function")

sn <- c('sn1','sn2','sn3', 'sn4', 'sn5')
set.seed(3)
col1 <- sample(1:10, 5)
col2 <- sample(1:10, 5)
col3 <- sample(1:10, 5)
col4 <- sample(1:10, 5)
col5 <- sample(1:10, 5)
col6 <- sample(1:10, 5)

test.data6 <- data.frame(sn, col1, col2, col3, col4, col5, col6)

#insert outliers.
test.data6$col2[2] <- NA
test.data6$col3[3] <- NA
test.data6$col3[4] <- Inf
test.data6$col6[3] <- NA

test.data6

#data with a single entry
test.data7 <- test.data6
set.seed(1)
test.data7[3,2:5] <- NA


test_that('data types are correct', {
  expect_is(test.data6,'data.frame')
  expect_is(test.data6$sn, 'character')
  expect_is(test.data6$col2, 'integer')
})

test1 <- data_imputation(test.data6, id_field = TRUE, method = 1,
               replace_with = 1)

test_that('checking output is complete and dimensions correct', {
  expect_equal(length(test1), 4)
  expect_equal(length(test1$RowIndexes_withNA), 2)
  expect_equal(length(test1$RowIndexes_withInf), 1)
  expect_equal(nrow(test1$CompleteData), 5)
  expect_equal(ncol(test1$CompleteData), 7)
})

test_that('detecting singular entry', {
  expect_error(data_imputation(test.data7, id_field = TRUE,
                              method = 2, replace_with = 1,
  prints_text(paste("Trajectory has only one data point.",
                    "Unable to inter/extrapolate between points.",
                    "Program terminated!!", sep=" "))))
})

test2 <- data_imputation(test.data6, id_field = TRUE, method = 1,
                        replace_with = 1)

test_that('detecting no missing data entry', {
  expect_error(data_imputation(test2$CompleteData, id_field = TRUE,
                          method = 1, replace_with = 1,
                          prints_text(paste("No missing entries in data"))))
})
