context("Testing opi_score.R function")

#create new fake text document
set.seed(1000)
doc <- data.frame(text=sample(c("I love research because it is good!",
                           "I hate research, it is time-consuming",
                           "I have an opinion on it"), size=50,
                           replace = TRUE, prob = c(0.5, 0.3, 0.2)))

#shorter document
doc_short <- data.frame(text=doc[1:14,1])

#a fictitious user-defined function
user_fun <- function(P, N, O){
  score <- (N + O)/(P + N + O)
  return()
}


#aa<-opi_score(textdoc=doc , metric = 1, fun = user_fun)

test_that('print appropriate warning message', {
  #defining a function without proper 'metric' argument
  expect_that(opi_score(textdoc=doc , metric = 1, fun = user_fun),
   prints_text(paste("Warning: `fun` parameter will not be used!!",
   "Otherwise, set`metric = 5`", sep=" ")))
})



#checking that error msg outputs are correct
test_that("print out correct error msgs", {

  expect_error(opi_score(textdoc=doc , metric = 5, fun = NULL,
      prints_text(paste("A user-defined opinion function is",
       "need in the parameter 'fun'", sep=" "))))

  expect_error(opi_score(textdoc=doc , metric = 1, fun = NULL,
      prints_text(paste("Length of document is too small!!",
       "The minimum document length of 15",
       "is recommended! Process terminated!!",
       sep=" "))))

  expect_error(opi_score(textdoc=doc , metric = 6, fun = NULL,
      prints_text(paste(" 'Metric' argument can only assume",
        "values from 1, 2, 3,..., 5",
        sep=" "))))

})

#check that the output is complete
output <- opi_score(textdoc=doc , metric = 1, fun = NULL)

test_that('check that output is complete', {
  #check completeness of result..
  expect_equal(length(output), 5)
  expect_equal(length(output$sentiments), 5)
  expect_equal(length(output$opiscore), 1)
  expect_equal(length(output$metric), 1)
  expect_equal(length(output$equation), 1)
  expect_equal(ncol(output$OSD), 2)
  expect_equal(nrow(output$OSD), 45)
})

test_that('output type are correct', {
  expect_is(output, 'list')
  expect_is(output$opiscore, 'character')
  expect_is(output$metric, 'character')
  expect_is(output$equation, 'character')
  expect_is(output$OSD, 'data.frame')

})


neg_sent <- substr(output$sentiments[3], 2, 9)
pos_sent <- substr(output$sentiments[4], 2, 9)
neu_sent <- substr(output$sentiments[5], 2, 8)

#combine sentiments
combine_sent <- c(neg_sent, pos_sent, neu_sent)

test_that('no sentiment is missing', {
  expect_identical(combine_sent,
                   c("negative", "positive", "neutral"))
})


