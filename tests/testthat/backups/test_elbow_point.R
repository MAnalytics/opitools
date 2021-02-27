context("Testing elbow_point.R function")

x <- runif(100, min=-2, max=3)
y <- -exp(-x) * (1+rnorm(100)/3)
plot(x, y)

# get the elbow points
elbP <- elbow_point(x,y)

#sign of most extreme derivative
pick.sign <- function(x) {
  most.extreme <- which(abs(x) == max(abs(x), na.rm=TRUE))[1]
  sign(x[most.extreme])
}

test_that('no missing element and variable complete', {

  expect_equal(length(elbP), 7)

  expect_identical(elbP$fittedSpline$x, na.omit(elbP$fittedSpline$x))
  expect_identical(elbP$fittedSpline$y, na.omit(elbP$fittedSpline$y))
  expect_identical(elbP$fittedSpline$y, na.omit(elbP$fittedSpline$y))
  expect_identical(elbP$first.deriv, na.omit(elbP$first.deriv))
  expect_identical(elbP$second.deriv, na.omit(elbP$second.deriv))

  expect_equal(length(elbP$input.x), length(elbP$fittedSpline$x))
  expect_equal(length(elbP$input.y), length(elbP$fittedSpline$y))
  expect_equal(length(elbP$first.deriv), length(elbP$second.deriv))
})

test_that('sign picking is correct', {

  expect_equal(pick.sign(elbP$first.deriv), 1)
  expect_equal(pick.sign(elbP$second.deriv), -1)
})



