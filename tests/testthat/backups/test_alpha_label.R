context("Testing alpha_label.R function")

set.seed(7)
ids1 <- sample(1:100, 10, replace=FALSE)
ids2 <- sample(1:500, 400, replace=TRUE)
ids3 <- c("AZ", "F", "EA", "R")
#ids_alphab <- alpha_label(ids2)

test_that('terminate upon errors', {
  expect_error(alpha_label(ids2,
   prints_text(paste("Labels exhausted!",
                     "specify a vector with fewer elements", sep=" "))))
  expect_error(alpha_label(ids3,
    prints_text("*== A vector of numbers is required! ==*")))
})


test_that('correct dimension', {
  expect_equal(length(ids2),
                   length(alpha_label(ids2)))
})
