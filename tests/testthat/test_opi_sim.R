context("Testing opi_sim.R function")

#create new fake text document
set.seed(1000)
fake_osd <- data.frame(ID=1:50,
       sentiment=sample(c("negative", "positive"),
       size = 50, replace =TRUE, prob = c(0.6, 0.4)),
       keywords = sample(c("present", "absent"),
       size = 50, replace =TRUE, prob = c(0.45, 0.55)))


#a fictitious user-defined function
user_fun <- function(P, N, O){
  score <- (N + O)/(P + N + O)
  return()
}


#checking that error msg outputs are correct
test_that("print out correct error msgs", {

  expect_error(opi_sim(fake_osd, nsim=98, metric = 1,
                       fun = NULL, quiet=TRUE,
      prints_text(paste("Number of simulation (nsim)",
      "is too small!!", sep=" "))))

  expect_error(opi_sim(fake_osd, nsim=999999, metric = 1,
      fun = NULL, quiet=TRUE,
      prints_text(paste("Consider specifying a smaller",
      "number of simulations (nsim)!!", sep=" "))))

  expect_error(opi_sim(fake_osd, nsim=99, metric = 6,
      fun = NULL, quiet=TRUE,
      prints_text(paste(" 'Metric' argument can only",
        "assume values from 1, 2,..., 5", sep=" "))))


  expect_error(opi_sim(fake_osd, nsim=99, metric = 5,
      fun = NULL, quiet=TRUE,
      prints_text(paste("A function (equation) is",
      "required in the parameter 'fun'", sep=" "))))

  expect_error(opi_sim(fake_osd, nsim=99, metric = 1,
     fun = user_fun, quiet=TRUE,
     prints_text(paste("Warning: `fun` parameter will not be used!!",
     "Otherwise, set`metric = 5`", sep=" "))))

})


test_that('print appropriate warning message', {
  #defining a function without proper 'metric' argument
  expect_that(opi_sim(fake_osd, nsim=99, metric = 1,
            fun = user_fun, quiet=TRUE),
            prints_text(paste("Warning: `fun` parameter will not be used!!",
            "Otherwise, set`metric = 5`", sep=" ")))
})


outp <- opi_sim(fake_osd, nsim=99, metric = 1,
        fun = NULL, quiet=TRUE)

test_that('check that output is complete', {
  #check completeness of result..
  expect_equal(length(outp), 99)
})

test_that('output type iscorrect', {
  expect_is(outp, 'numeric')
})

