context("Testing props.R function")

sn <- c('sn1','sn2','sn3', 'sn4')
set.seed(1)
col1 <- sample(1:10, 4)
col2 <- sample(1:10, 4)
col3 <- sample(1:10, 4)
col4 <- sample(1:10, 4)

test.data3 <- data.frame(sn, col1, col2, col3, col4)

props_d1 <- props(test.data3, id_field = TRUE, scale=1, digits=4)

test_that("datatype correct", {
  expect_is(props_d1, "data.frame")
  expect_is(props_d1$sn, "character")
  expect_is(props_d1$col1, "numeric")
  expect_is(props_d1$col2, "numeric")
  expect_is(props_d1$col3, "numeric")
  expect_is(props_d1$col4, "numeric")
})

test_that("column sum equals 1", {
  expect_equal(sum(props_d1$col1), 1, tolerance=1e-2)
  expect_equal(sum(props_d1$col2), 1, tolerance=1e-2)
  expect_equal(sum(props_d1$col3), 1, tolerance=1e-2)
  expect_equal(sum(props_d1$col4), 1, tolerance=1e-2)
})

test_that("throw warnings or error for character field", {
  expect_warning(props(test.data3, id_field = FALSE))
  expect_error(colSums(props(test.data3, id_field = TRUE)))
})

