## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----functions, include=FALSE-------------------------------------------------
# A function for captioning and referencing images
fig <- local({
    i <- 0
    ref <- list()
    list(
        cap=function(refName, text) {
            i <<- i + 1
            ref[[refName]] <<- i
            paste("Figure ", i, ": ", text, sep="")
        },
        ref=function(refName) {
            ref[[refName]]
        })
})

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  WORKING_DIR <- 'C:/R/Github/JGIS_Policing_COVID-19'
#  
#  #setting working directory
#  setwd(WORKING_DIR)

## ---- include=TRUE, message=FALSE, eval=TRUE----------------------------------

library(opitools) #for impact analysis
require(knitr)
library(kableExtra)
library(dplyr)
library(cowplot)
#library(rtweet) #for data download
#library(twitteR) #for setting up Twitter authorization
#library(wordcloud2)
#library(tibble)
#library(tm)
#library(dplyr)


## ---- echo=FALSE, include=FALSE-----------------------------------------------
col1 <- c("1", "2", "3")
col2 <- c("`policing_dtd`","`reviews_dtd`","`debate_dtd`")
col3 <- c("`Law Enforcement`","`Marketing`", "`Politics`")
col4 <- c("A digital text document (DTD) containing twitter posts on police/policing during the 2020 COVID-19 pandemic", "A DTD containing customer reviews of the Piccadilly train station (Manchester, uk). Data is downloaded from the www.tripadvisor.co.uk'. The records cover from July 2016 to March 2021.", "A DTD containing individual comments on the video showing the debate between two US presidential nominees (Donald Trump and Hillary Clinton) in Sept. 2016. (Credit: NBC News).")
col5 <- c("www.twitter.com", "www.tripadvisor.co.uk","www.youtube.com")
tble1 <- data.frame(col1, col2, col3, col4, col5)
tble1 <- tble1

## ----table1, results='asis', echo=FALSE, tidy.opts=list(width.cutoff=50)------
knitr::kable(tble1, caption = "Table 1. `Example datasets`", col.names = c("SN","Data","Application","Details", "Data Source")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "8em", background = "white") %>%
  column_spec(3, width = "12em", background = "white") %>%
  column_spec(4, width = "16em", background = "white")#%>%
  #row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## ---- echo=FALSE, include=FALSE-----------------------------------------------
col1 <- c("1", "2")
col2 <- c("`word_distrib`","`word_imp`")
col3 <- c("`Words Distribution`","`Importance of words (terms) embedded in a text document`")
col4 <- c("Examines the extent to which words in a DTD follow the Zipf's distribution (Zipf 1934). The Zipf's distribution  models the ideal natural language text", "Produces a table or graphic that highlights the importance of individual words(terms) in a DTD.")
tble2 <- data.frame(col1, col2, col3, col4)
tble2 <- tble2

## ----table2, results='asis', echo=FALSE, tidy.opts=list(width.cutoff=50)------
knitr::kable(tble2, caption = "Table 2. `Exploratory` functions", col.names = c("SN","Function","Title","Description")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "8em", background = "white") %>%
  column_spec(3, width = "12em", background = "white") %>%
  column_spec(4, width = "16em", background = "white")#%>%
  #row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## ---- message=FALSE, include = TRUE, eval=FALSE-------------------------------
#  
#  # Load data
#  data(tweets)
#  
#  # Get the texts
#  tweets_dat <- as.data.frame(tweets[,1])
#  
#  # Run function
#  plt = word_distrib(textdoc = tweets_dat)
#  
#  #Show Zipf's distribution:
#  
#  plt$plot
#  

## ----figs1, echo=FALSE, fig.width=5,fig.height=6,fig.align="center", fig.cap=fig$cap("figs1", "Data freq. plot vs. Zipf's distribution")----
knitr::include_graphics("zipf.png")

## ---- message=FALSE, include = TRUE, eval=FALSE-------------------------------
#  
#  #Load datasets
#  
#  data("policing_dtd")
#  data("reviews_dtd")
#  data("debate_dtd")
#  
#  
#  p1 <- word_imp(textdoc = policing_dtd, metric= "tf", n_top=5,
#                             words_to_filter=c("police","policing"))
#  
#  #Note: 'words_to_filter' parameter is used to eliminate non-necessary words that
#  #may be too dominant in the DTD.
#  
#  p2 <- word_imp(textdoc = reviews_dtd, metric= "tf", n_top=5,
#                             words_to_filter=c("station", "manchester","train"))
#  
#  p3 <- word_imp(textdoc = debate_dtd, metric= "tf", n_top=5,
#                             words_to_filter=NULL)
#  #output3 <- word_imp(textdoc = debate_otd, metric= "tf", n_top=5
#  p1$plot
#  p2$plot
#  p3$plot
#  

## ----figs2, echo=FALSE, fig.width=3,fig.height=4,fig.align="center", fig.cap=fig$cap("figs2", "Highlighting words importance from a DTD")----
knitr::include_graphics("wordcloud.png")

## ---- echo=FALSE, include=FALSE-----------------------------------------------
col1 <- c("3", "4", "5")
col2 <- c("`opi_score`","`opi_sim`", "`opi_impact`")
col3 <- c("`Opinion score of a text document`",
          "`Simulates the opinion expectation distribution of a text document`",
          "`Statistical assessment of impacts of a specified theme (or subject) from a document`")
col4 <- c("Given a text document, this function computes the overall opinion score based on the proportion of text records classified as expressing positive, negative or a neutral sentiment about the subject.",  "This function simulates the expectation distribution of the observed opinion score (computed using the `opi_score` function).",  "This function assesses the impacts of a theme (or subject) on the overall opinion computed for a text document. The text records relating to the theme in question should be identified and provided as input to this function")
tble3 <- data.frame(col1, col2, col3, col4)
tble3<- tble3

## ----table3, results='asis', echo=FALSE, tidy.opts=list(width.cutoff=50)------
knitr::kable(tble3, caption = "Table 3. `Impact Analytical` function", col.names = c("SN","Function","Title","Description")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "8em", background = "white") %>%
  column_spec(3, width = "12em", background = "white") %>%
  column_spec(4, width = "16em", background = "white")#%>%
  #row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## ---- message=FALSE, include = TRUE, eval=FALSE-------------------------------
#  
#  #Application: Law enforcement
#  
#  # Load DTD
#  data(policing_dtd)
#  
#  # Load theme keywords
#  data(covid_keys)
#  
#  # Run the analysis
#  output1 <- opi_impact(policing_dtd, sec_keywords=covid_keys, metric = 1,
#                         fun = NULL, nsim = 99, alternative="two.sided",
#                         quiet=TRUE)
#  print(output1)
#  

## ---- echo=TRUE, message=FALSE, eval=FALSE------------------------------------
#  
#  > output1
#  
#  $test
#  [1] "Test of significance (Randomization testing)"
#  
#  $criterion
#  [1] "two.sided"
#  
#  $exp_summary
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   -8.240  -5.880  -5.880  -4.837  -3.530  -1.180
#  
#  $p_table
#  
#  |observed_score |S_beat |nsim |pvalue |signif |
#  |:--------------|:------|:----|:------|:------|
#  |-5.88          |51     |99   |0.52   |'      |
#  
#  $p_key
#  [1] "0.99'"   "0.05*"   "0.025**" "0.01***"
#  
#  $p_formula
#  [1] "(S_beat + 1)/(nsim + 1)"
#  
#  $plot

## ----figs3, echo=FALSE, fig.width=5,fig.height=6,fig.align="center", fig.cap=fig$cap("figs3", "Percentage proportion of classes")----
knitr::include_graphics("likert.png")

## ---- echo=FALSE, include=FALSE-----------------------------------------------
col1 <- c("1", "2a", "2b","3")
col2 <- c("Does COVID-19 pandemic influence public opinion on neighourhood policing?", "Do the refreshment outlets impact customers' opinion of the Piccadilly train services?", "Do the signages influence customers' opinion of the Piccadilly train services?", "How does the democratic candidate (Hillary Clinton ) impacts the viewers' opinion of the debate?")
col3 <- c("`policing_dtd`","`reviews_dtd`", "`reviews_dtd`", "`debate_dtd`")
col4 <- c("`covid_keys`","`refreshment_keys`", "`signage_keys`", "direct input")
col5 <- c("two.sided", "two.sided", "two.sided", "two.sided")
col6 <- c("(P - N)/(P + N)*100", "(P - N)/(P + N)*100", "(P - N)/(P + N)*100", "(P - N)/(P + N)*100")
col7 <- c("-5.88", "67.92", "67.92", "-0.33")
col8 <- c("0.52", "0.01", "0.1", "0.93")
tble4 <- data.frame(col1, col2, col3, col4, col5, col6, col7, col8)
tble4 <- tble4

## ----table4, results='asis', echo=FALSE, tidy.opts=list(width.cutoff=50)------
knitr::kable(tble4, caption = "Table 4. `Impact analysis`", col.names = c("SN.","RQs","Primary data","sec_keywords","criterion", "Score function", "Observed score (S)", "p-value")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "26em", background = "white") %>%
  column_spec(3, width = "8em", background = "white") %>%
  column_spec(4, width = "8em", background = "white") %>%
  column_spec(5, width = "8em", background = "white") %>%
  column_spec(6, width = "8em", background = "white") %>%
  column_spec(7, width = "8em", background = "white")%>%
  column_spec(8, width = "8em", background = "white")#%>%
  #row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## ---- echo=TRUE, message=FALSE, eval=FALSE------------------------------------
#  
#  #The equation
#  Score = (P + O - N)/(P + O + N)
#  
#  #Corresponding function
#  myfun <- function(P, N, O){
#     score <- (P + O - N)/(P + O + N)
#     return(score)
#  }
#  

## ---- echo=TRUE, message=FALSE, eval=FALSE------------------------------------
#  
#  output <- opi_impact(debate_dtd, sec_keywords=keys, metric = 5,
#                         fun = myfun, nsim = 99, alternative="two.sided",
#                         quiet=TRUE)

