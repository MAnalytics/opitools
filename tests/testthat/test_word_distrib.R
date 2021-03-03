context("Testing word_distrib.R function")

#create new fake text document
set.seed(1000)
doc <- data.frame(text=sample(c("I love research because it is good!",
           "I hate research, it is time-consuming",
           "I have an opinion on it"), size=50,
            replace = TRUE, prob = c(0.5, 0.3, 0.2)))

#append one more column
doc2 <- data.frame(doc, ID=seq.int(nrow(doc)))


#checking that error msg outputs are correct
test_that("print out correct error msgs", {

  expect_error(word_distrib(doc2,
         prints_text(paste("Dataframe must include only one column",
            "containing the text records!!", sep=" "))))
})

#check that the output is complete
output <- word_distrib(doc)

test_that('check that output is complete', {
  #check completeness of result..
  expect_equal(length(output), 2)
  expect_equal(ncol(output$freqRank), 6)
})

test_that('output type are correct', {
  expect_is(output, 'list')
  expect_is(output$freqRank, 'data.frame')

})
