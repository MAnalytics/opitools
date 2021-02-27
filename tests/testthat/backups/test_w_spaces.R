context("Testing w_spaces.R function")

#simulating different whitespaces
sn <- c('sn1','sn2','sn3')
col1 <- c(21000, 23400, 26800)
col2 <- as.character(c("23 ","45 ","23 ")) #trailing whitespaces
col3 <- as.character(c(" 23"," 45", " 23")) #leading whitespaces
col4 <- as.character(c(" 23 ", " 45 ", " 23 ")) #trailing and leading

test.data1 <- data.frame(sn, col1, col2, col3, col4)

check_1 <- w_spaces(test.data1, remove="Right")
check_2 <- w_spaces(test.data1, remove="Left")
check_3 <- w_spaces(test.data1, remove="Both")

test_that('number of whitespaces removed correct', {
  expect_equal(check_1$NumberOfWhiteSpacesRemoved, 6)
  expect_equal(check_2$NumberOfWhiteSpacesRemoved, 6)
  expect_equal(check_3$NumberOfWhiteSpacesRemoved, 12)
  expect_false(isTRUE(all.equal(check_1$CleanData, check_2$CleanData)))
  expect_false(isTRUE(all.equal(check_1$CleanData, check_3$CleanData)))
  expect_false(isTRUE(all.equal(check_2$CleanData, check_3$CleanData)))
})

