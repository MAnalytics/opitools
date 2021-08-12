context("Testing opi_impact.R function")

#create new fake text document
set.seed(1000)
doc <- data.frame(text=sample(c("I love research because it is good!",
                                "I hate research, it is time-consuming",
                                "I have an opinion on it"), size=50,
                              replace = TRUE, prob = c(0.5, 0.3, 0.2)))

doc_xtra <- data.frame(text=sample(c("I love research because it is good!",
                                "I hate research, it is time-consuming",
                                "I have an opinion on it"), size=50,
                              replace = TRUE, prob = c(0.5, 0.3, 0.2)))

#assumed keywords

keywords <- data.frame(keys=c("love","research")) #exist in
keywords2 <- data.frame(keys=c("angry","nice")) #not exist in doc
keywords3 <- data.frame(keys=c("love")) #exist in doc

#checking that error msg outputs are correct
test_that("print out correct error msgs", {

  expect_error(opi_impact(doc, theme_keys=NULL, metric = 1,
     fun = NULL, nsim = 98,
     prints_text(paste("Number of simulation (nsim)",
     "is too small!!", sep=" "))))

  expect_error(opi_impact(doc, theme_keys=NULL, metric = 1,
     fun = NULL, nsim = 999999,
     prints_text(paste("Consider specifying a smaller",
     "number of simulations (nsim)!!", sep=" "))))

  expect_error(opi_impact(doc, theme_keys=NULL, metric = 1,
     fun = NULL, nsim = 99,
     prints_text(paste(" 'theme_keys' parameter",
     "cannot be 'NULL'!! ", sep =" "))))

  expect_error(opi_impact(doc, theme_keys=NULL, metric = 1,
     fun = NULL, nsim = 99, alternative == "less",
     prints_text(paste('When parameter `metric =` ', metric,
    ", argument `two.sided` must be set as 'less'!! ", sep=""))))

  expect_error(opi_impact(doc, theme_keys=NULL, metric = 2,
     fun = NULL, nsim = 99, alternative == "two.sided",
     prints_text(paste('When parameter `metric =` ', metric,
     ", argument `two.sided` must be set as 'less'!! ", sep=""))))

  expect_error(opi_impact(doc, theme_keys=keywords2, metric = 1,
     fun = NULL, nsim = 99, alternative == "two.sided",
     prints_text(paste("The text record contains NONE of",
     "the secondary keywords!! Operation terminated!!", sep=" "))))

  expect_error(opi_impact(doc, theme_keys=keywords, metric = 1,
              fun = NULL, nsim = 99, alternative = "less",
              prints_text(paste("When parameter `metric = 1`, argument",
              " `alternative` must be set as 'two.sided'!! "))))

  expect_error(opi_impact(doc, theme_keys=keywords, metric = 2,
              fun = NULL, nsim = 99, alternative = "two.sided",
              prints_text(paste('When parameter `metric =` ', metric,
              ", argument `two.sided` must be set as 'less'!! ", sep=""))))

  expect_error(opi_impact(textdoc=doc, theme_keys=keywords, metric = 1,
        fun = NULL, nsim = 99, alternative = "two.sided",
        prints_text(paste("The 'theme_keys' are either completely present",
        "or absent in a sentiment class! The process terminated!!",
        sep=" "))))

})


#check that the output is complete
#output3 <- opi_impact(textdoc=doc, theme_keys=keywords3, metric = 1,
#           fun = NULL, nsim = 99, alternative = "two.sided")

test_that('check that output is complete', {
  #check completeness of result..
  expect_equal(length(output3), 7)
  expect_equal(length(output3$test), 1)
  expect_equal(length(output3$criterion), 1)
  expect_equal(length(output3$exp_summary), 6)
  expect_equal(length(output3$p_table), 3)
  expect_equal(length(output3$p_key), 4)
  expect_equal(length(output3$p_formula), 1)
})
#

test_that('check output types are accurate', {
  expect_is(output3, 'list')
  expect_is(output3$test, 'character')
  expect_is(output3$criterion, 'character')
  #expect_is(output3$exp_summary, 'numeric')
  #expect_is(output3$p_table, 'character')
  expect_is(output3$p_key, 'character')
  expect_is(output3$p_formula, 'character')
})


