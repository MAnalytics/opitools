context("Testing word_imp.R function")

#create new fake text document

#small doc
set.seed(1000)
doc1 <- data.frame(text=sample(c("I love research because it is good!",
                                 "I do not like research, it is time-consuming",
                                 "I have no opinion on it"), size=7,
                               replace = TRUE, prob = c(0.5, 0.3, 0.2)))

#append one more column
doc1_ <- data.frame(doc1, ID=seq.int(nrow(doc1)))

#large doc
set.seed(1000)
doc2 <- data.frame(text=sample(c("I love research because it is good!",
                                "I do not like research, it is time-consuming",
                                "I have no opinion on it"), size=50,
                              replace = TRUE, prob = c(0.5, 0.3, 0.2)))

#append one more column
doc2_ <- data.frame(doc2, ID=seq.int(nrow(doc2)))


#checking that error msg outputs are correct
test_that("print out correct error msgs", {

  expect_error(word_imp(doc1_,
             prints_text(paste("Length of document is too small!!",
             "The minimum allowable length is 20!",
             "Process terminated!!", sep=" "))))
})

#checking that error msg outputs are correct
#test_that("print out correct error msgs", {

  #expect_error(word_imp(doc1_,
                        #prints_text(paste("Dataframe must include only one column",
                                          #"containing the text records!!", sep=" "))))
#})






