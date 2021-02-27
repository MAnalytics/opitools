context("Testing akclustr.R function")

#simulate dataset
sn <- c('sn1','sn2','sn3', 'sn4', 'sn5')

set.seed(5)
col1 <- sample(1:10, 5)
col2 <- sample(1:10, 5)
col3 <- sample(1:10, 5)
col4 <- sample(1:10, 5)
col5 <- sample(1:10, 5)
col6 <- sample(1:10, 5)

test.data8 <- data.frame(sn, col1, col2,
                         col3, col4, col5, col6)

#sample with with non-unique id_field
test.data9 <- test.data8
test.data9$sn[3] <- "sn2"

test_that('print the right message post clustering', {
  expect_that(akclustr(test.data8, id_field = TRUE, k = c(3),
          crit = 'Calinski_Harabasz', verbose=TRUE),
          prints_text("solution of k = 3 determined!"))
})

test_that('check error msgs output correctly', {
  #cluster number cannot be less than 3
  expect_error(akclustr(test.data8, id_field = TRUE, k = c(2),
         prints_text("(: Program terminated!!! :)")))
  #number of clusters cannot descend
  expect_error(akclustr(test.data8, id_field = TRUE, k = c(4,3),
         prints_text("(: Program terminated!!! :)")))
  #id_field non_unique
  expect_error(akclustr(test.data9, id_field = TRUE, k = c(3),
         prints_text(paste("(: The 'id_field' does not contain unique",
         "elements. Function terminated!!! :)", sep=" "))))
  expect_error(akclustr(test.data8, id_field = TRUE, k = c(3,5),
         crit="someRandomCrit",
         prints_text(paste("*----*(: Quality criterion specified is NOT",
         "RECOGNISED!! Execution terminated!!! :)*----*", sep= " "))))
  expect_error(akclustr(test.data8, id_field = TRUE, k = c(3,5),
      crit="Silhouette",
      prints_text(paste("*----*(: 'Silhouette' criterion is not applicable!.",
      "Try 'Calinski_Harabasz':)*----*", sep=" "))))
})


output <- akclustr(test.data8, id_field = TRUE, k = c(3, 5),
                         crit = "Calinski_Harabasz")

test_that('check that output is complete', {
  ##expect_equal(length(output), 4)
  #check completeness of result..
  expect_equal(length(output), 7)
  expect_equal(length(output$qualitycriterion), 1)
  expect_equal(length(output$optimal_k), 1)
  #expect_equal(length(output$solutions), 5)
  expect_equal(nrow(output$qualityCrit.List), 3)
  expect_equal(ncol(output$qualityCrit.List), 2)
})


#checking that cluster labels are complete.
test_that('no missing label', {
    expect_identical(output$solutions[[1]],
              na.omit(output$solutions[[1]]))
 })


test_that('cluster label matches traj size', {
  expect_equal(length(output$solutions[[1]]), 5)
})

test_that('data type are correct', {
  expect_is(output$qualitycriterion, 'character')
  ##expect_is(output$solutions, 'character')
  expect_is(output$qualityCrit.List, 'data.frame')
})

