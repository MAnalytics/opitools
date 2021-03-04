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
#library(rtweet) #for data download
#library(twitteR) #for setting up Twitter authorization
#library(wordcloud2)
#library(tibble)
#library(tm)
#library(dplyr)


## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  
#  #Run function
#  waitFun <- function(x){
#    p1 <- proc.time()
#    Sys.sleep(x)
#    proc.time() - p1
#  }
#  
#  #specify tokens and authorize
#  #Note: replace asterisk with real keys
#  
#  consumer_key <- '*******************************'
#  consumer_secret <- '*******************************'
#  access_token <- '*******************************'
#  access_secret <- '*******************************'
#  
#  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#  
#  token <- create_token(
#    app = "AppName", #App name
#    consumer_key = consumer_key,
#    consumer_secret = consumer_secret)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  
#  #Define the keywords for subject A
#  keywords <- c("police", "policing", "law enforcement")
#  
#  #tweets holder
#  all_Tweets <- NULL
#  
#  #Loop through each keyword and wait for 15 minutes
#  #and row-bind the results
#  for(i in seq_len(length(keywords))){
#  
#    tweets_g1 <- NULL
#  
#    #actual download codes
#    tweets_g1 <- search_tweets(q=keywords[i],  n=17500, type="recent", include_rts=TRUE,
#                               token = token, lang="en",geocode='53.805,-4.242,350mi')
#  
#    if(nrow(tweets_g1)!=0){
#      tweets_g1 <- tweets_g1 %>% dplyr::mutate(class=keywords[i])
#      all_Tweets <- rbind(all_Tweets, tweets_g1)
#    }
#  
#    flush.console()
#    print(paste(nrow(tweets_g1), nrow(tweets_g1), sep="||"))
#    print("waiting for 15.5 minutes")
#    waitFun(960)
#  }
#  
#  #save the output
#  write_as_csv(all_Tweets, "tweets.csv", na="NA", fileEncoding = "UTF-8")
#  

## ---- message=FALSE, include = TRUE, eval=FALSE-------------------------------
#  
#  #using a randomised Twitter data from 'opitools'
#  
#  #data(tweets)
#  
#  tweets_dat <- as.data.frame(tweets[,1])
#  
#  plt = word_distrib(textdoc = tweets_dat)
#  
#  #to show the plot, type:
#  
#  #>plt$plot
#  

## ----figs1, echo=FALSE, fig.width=5,fig.height=6,fig.align="center", fig.cap=fig$cap("figs1", "Data freq. plot vs. Zipf's distribution")----
knitr::include_graphics("zipf.png")

## ---- message=FALSE, include = TRUE, eval=FALSE-------------------------------
#  
#  dat <- list(tweets_dat)
#  
#  series <- tibble()
#  
#  #tokenize document
#  series <- tibble(text = as.character(unlist(dat)))%>%
#    unnest_tokens(word, text)%>% #tokenize
#    dplyr::select(everything())
#  
#  #removing stopwords
#  tokenize_series <- series[!series$word %in% stopwords("english"),]
#  
#  #compute term frequencies
#  doc_words <- tokenize_series %>%
#    dplyr::count(word, sort = TRUE) %>%
#    dplyr::ungroup() %>%
#    dplyr::mutate(len=nchar(word)) %>%
#    #remove words with character length <= 2
#    dplyr::filter(len > 2)%>%
#    data.frame() %>%
#    dplyr::rename(freq=n)%>%
#    dplyr::select(-c(len))%>%
#    #removing the words, '' & '' because of
#    #their dominance
#    dplyr::filter(!word %in% c("police", "policing"))
#  
#  
#  row.names(doc_words) <- doc_words$word
#  
#  #use only the top 1000 words
#  wordcloud2(data=doc_words[1:1000,], size = 0.7, shape = 'pentagon')
#  

## ----figs2, echo=FALSE, fig.width=3,fig.height=4,fig.align="center", fig.cap=fig$cap("figs2", "Detecting important words from within the document")----
knitr::include_graphics("wordcloud.png")

## ---- message=FALSE, include = TRUE, eval=FALSE-------------------------------
#  
#  # call data
#  data(tweets)
#  
#  # Get an n x 1 text document
#  tweets_dat <- as.data.frame(tweets[,1])
#  
#  # Run the analysis
#  
#  output <- opi_impact(tweets_dat, sec_keywords=covid_keys, metric = 1,
#                         fun = NULL, nsim = 99, alternative="two.sided",
#                         quiet=TRUE)
#  

## ---- echo=TRUE, message=FALSE, eval=FALSE------------------------------------
#  output
#  
#  #> $test
#  #> [1] "Test of significance (Randomization testing)"
#  #>
#  #> $criterion
#  #> [1] "two.sided"
#  #>
#  #> $exp_summary
#  #>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  #>  -27.80  -26.52  -26.10  -26.13  -25.75  -24.26
#  #>
#  #> $p_table
#  #>
#  #>
#  #> observed_score   S_beat   nsim   pvalue   signif
#  #> ---------------  -------  -----  -------  -------
#  #> -28.23           0        99     0.01     ***
#  #>
#  #> $p_key
#  #> [1] "0.99'"   "0.05*"   "0.025**" "0.01***"
#  #>
#  #> $p_formula
#  #> [1] "(S_beat + 1)/(nsim + 1)"

## ----figs3, echo=FALSE, fig.width=5,fig.height=6,fig.align="center", fig.cap=fig$cap("figs3", "Percentage proportion of classes")----
knitr::include_graphics("likert.png")

## ---- echo=TRUE, message=FALSE, eval=FALSE------------------------------------
#  
#  #define opinion score function
#  myfun <- function(P, N, O){
#     score <- (P + O - N)/(P + O + N)
#     return(score)
#  }
#  

## ---- echo=TRUE, message=FALSE, eval=FALSE------------------------------------
#  
#  results <- opi_impact(tweets_dat, sec_keywords=covid_keys, metric = 5,
#                         fun = myfun, nsim = 99, alternative="two.sided",
#                         quiet=TRUE)

## ---- echo=TRUE, message=FALSE, eval=FALSE------------------------------------
#  
#  print(results)
#  
#  #> $test
#  #> [1] "Test of significance (Randomization testing)"
#  #>
#  #> $criterion
#  #> [1] "two.sided"
#  #>
#  #> $exp_summary
#  #>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  #>  -27.80  -26.52  -26.10  -26.13  -25.75  -24.26
#  #>
#  #> $p_table
#  #>
#  #>
#  #> observed_score       S_beat   nsim   pvalue   signif
#  #> -------------------  -------  -----  -------  -------
#  #> -0.234129692832764   99       99     1        NA
#  #>
#  #> $p_key
#  #> [1] "0.99'"   "0.05*"   "0.025**" "0.01***"
#  #>
#  #> $p_formula
#  #> [1] "(S_beat + 1)/(nsim + 1)"
#  

