context("Testing outlier_detect.R function")

sn <- c('sn1','sn2','sn3', 'sn4', 'sn5')
set.seed(1)
col1 <- sample(1:10, 5)
col2 <- sample(1:10, 5)
col3 <- sample(1:10, 5)
col4 <- sample(1:10, 5)
col5 <- sample(1:10, 5)
col6 <- sample(1:10, 5)

test.data4 <- data.frame(sn, col1, col2, col3, col4, col5, col6)

#insert outliers.
test.data4$col2[2] <- 30
test.data4$col3[3] <- 45
test.data4$col3[4] <- 30
test.data4$col6[3] <- 40

#insert all outlier row

test.data5 <- test.data4
set.seed(1)
test.data5[3,2:7] <- sample(40:50,6)#all outliers

test_that('output correct error messages', {
  expect_error(outlier_detect(test.data4, id_field = TRUE, threshold = 1.8,
   prints_text(paste("*--Terminated!!!--*, The 'threshold'",
                     "value should be between 0 and 1", sep=" "))))
  expect_error(outlier_detect(test.data4, id_field = TRUE,
                             threshold = 0.8, replace_with=1,
   prints_text("*--Outlier observation(s) was found in trajectory 3 --*")))
  expect_error(outlier_detect(test.data4, id_field = TRUE,
                             threshold = 0.8, replace_with=2,
   prints_text("*--Outlier observation(s) was found in trajectory 3 --*")))

})

#basic detect and replace
outlier_detection <- outlier_detect(test.data4, id_field = TRUE)

test_that('output dimensions are correct', {
  expect_equal(length(outlier_detection$Outlier_Observations), 1)
  expect_equal(length(outlier_detection$Non_Outlier_Observations), 4)
  expect_equal(length(outlier_detection$Threshold), 1)
  expect_equal(nrow(outlier_detection$Outliers_Replaced), 5)
  expect_equal(ncol(outlier_detection$Outliers_Replaced), 7)
})

outlier_detection2 <- outlier_detect(test.data5, id_field = TRUE)

test_that('output values correct and complete', {
  expect_equal(outlier_detection2$Threshold, 45.1)
  expect_equal(outlier_detection2$Outliers_Replaced[3,4], 13)
})

outlier_detection3 <- outlier_detect(test.data5, id_field = TRUE,
                                    threshold=0.75, replace_with=3)

test_that('remove outliers', {
  expect_equal(length(outlier_detection3$Outlier_Observations), 3)
  expect_equal(length(outlier_detection3$Non_Outlier_Observations), 1)
  expect_equal(length(outlier_detection3$Threshold), 1)
  expect_equal(nrow(outlier_detection3$Outliers_Replaced), 2)
  expect_equal(ncol(outlier_detection3$Outliers_Replaced), 7)
})

