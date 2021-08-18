library(opitools)

data(policing_dtd)

p1a <- word_imp(textdoc = policing_dtd, metric= "tf-idf",
                words_to_filter=c("police","policing"))

p1a ## doesn't produce facet of histograms with tf-idf (hashed out in the script)

results <- opi_impact(textdoc = policing_dtd, theme_keys=c("covid", "pandemic"), metric = 1,
                      fun = NULL, nsim = 99, alternative="two.sided", quiet=FALSE)

results$plot
