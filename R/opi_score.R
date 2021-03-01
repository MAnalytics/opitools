#' @title Opinion score (of the main subject matter) of a text document
#' @description Given a text document (concerning a subject A),
#' this function computes the overall opinion score based on the
#' proportion of text records classified as expressing positive,
#' negative or a neutral sentiment about the subject.
#' The function first transforms
#' the text document into a tidy-format dataframe, described as the
#' `observed sentiment document (OSD)` (Adepeju and Jimoh, 2021),
#' in which each text record is assigned a sentiment class based
#' on the sum of all sentiments expressed by the words in the
#' text record.
#' @param textdoc An \code{n} x \code{1} list (dataframe) of
#' individual text records, where \code{n} is the total
#' number of individual records.
#' @param metric (an integer) Specify the metric to utilize for
#' the calculation of opinion score. Available values in this
#' package are: \code{1, 2, ...,5}.
#' Assuming \code{P}, \code{N} and \code{O} represent positive,
#' negative, and neutral record sentiments, respectively,
#' the followings are the details of the opinion score function
#' represented by the numerical arguments above:
#' \code{1}: Polarity (percentage difference)
#' \code{((P - N)/(P + N))*100}, (Bound: -100%, +100%);
#' \code{2}: Polarity (proportional difference)
#' \code{((abs(P - N) / (P + N + O))*100},
#' (Bound: 0, +100%);
#' \code{3}: Positivity \code{(P/ (P + N + O))*100},
#' (Bound: 0, +100%); \code{4}: Negativity \code{(N / (P + N + O))*100},
#' (Bound: 0, +100%) (Malshe, A. 2019;
#' Lowe et al. 2011). \code{5}: To pass a
#' user-defined opinion score function (also see the \code{fun}
#' parameter below.
#' @param fun A user-defined function given that \code{metric}
#' parameter (above) is set equal to \code{5}.
#' For example, given a defined opinion score function
#'  `myfun` <- `function(P, N, O){`
#' `("some tasks to do")`; `return("a value")}`, the input
#' argument of \code{fun} parameter then becomes `fun = myfun`.
#' Default: \code{NULL}.
#'
#' @usage opi_score(textdoc, metric = 1, fun = NULL)
#'
#' @examples
#' # Use police/pandemic posts on Twitter
#' # Experiment with a standard metric (e.g. metric 1)
#' score <- opi_score(textdoc = policing_otd, metric = 1, fun = NULL)
#' #print result details
#' print(score)
#' #preview results
#' print(score)
#'
#' #Example using a user-defined opinion score -
#' #a demonstration with a component of SIM opinion
#' #Score function (by Razorfish, 2009). The opinion
#' #function can be expressed as:
#'
#' myfun <- function(P, N, O){
#'   score <- (P + O - N)/(P + O + N)
#' return(score)
#' }
#'
#' #Run analysis
#' score <- opi_score(textdoc = policing_otd, metric = 5, fun = myfun)
#' #preview results
#' print(score)
#'
#'
#' @details An opinion score is derived from all the sentiments
#' (i.e. positive, negative (and neutral) expressed within a
#' text document. We deploy a lexicon-based approach
#' (Taboada et al. 2011) using the `AFINN` lexicon
#' (Nielsen, 2011).
#' @return Returns an `opi_object` containing details of the
#' opinion measures from the text document.
#' @references (1) Adepeju, M. and Jimoh, F. (2021). An
#' Analytical Framework for Measuring Inequality in the
#' Public Opinions on Policing – Assessing the impacts
#' of COVID-19 Pandemic using Twitter Data.
#' https://doi.org/10.31235/osf.io/c32qh
#' (2) Malshe, A. (2019) Data Analytics Applications.
#' Online book available at:
#' https://ashgreat.github.io/analyticsAppBook/index.html.
#' Date accessed: 15th December 2020.
#' (3) Taboada, M.et al. (2011).
#' Lexicon-based methods for sentiment analysis. Computational
#' linguistics, 37(2), pp.267-307.
#' (4) Lowe, W. et al. (2011).
#' Scaling policy preferences from coded political texts.
#' Legislative studies quarterly, 36(1), pp.123-155.
#' (5) Razorfish (2009) Fluent: The Razorfish Social Influence
#' Marketing Report. Accessed: 24th February, 2021.
#' (6) Nielsen, F. A. (2011), “A new ANEW: Evaluation of a word
#' list for sentiment analysis in microblogs”, Proceedings of the
#' ESWC2011 Workshop on 'Making Sense of Microposts': Big things
#' come in small packages (2011) 93-98.
#' @importFrom dplyr mutate select rename filter
#' left_join inner_join group_by summarise ungroup
#' @importFrom tidyr separate
#' @importFrom tidytext unnest_tokens get_sentiments
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr %>%
#' @import tibble tibble
#' @export

opi_score <- function(textdoc, metric = 1, fun = NULL){

  #call afinn
  #afinn_111 <- afinn_111

  #global variables
  ID <- as_tibble <- bigram <- get_sentiments <- neg <- sentiment <-
    sentiment_score <- separate <- text <- value <- value2 <-
    word <- word1 <- word2 <- NULL

  #ensuring appropriate length of document is supplied
  if(dim(textdoc)[1] < 15){
    stop(paste("Length of document is too small!!",
               "The minimum document length of 15 is recommended!",
               "Process terminated!!", sep = " "))
  }

  #if the length of document is too large
  if(dim(textdoc)[1] > 10000000){
    stop(paste("Length of document is too large!!",
               "The maximum document length is 10 million records!!",
               "Process terminated!!", sep = " "))
  }

  #check metric
  if(!metric %in% c(1:5)){
    stop(paste(" 'Metric' argument can only assume values from",
               "1, 2,..., 5", sep=" "))
  }

  #check if a user-defined function is inputted
  if(metric == 5 & is.null(fun)){
    stop(paste("A user-defined opinion function is",
      "need in the parameter 'fun'", sep=" "))
  }

  if(metric %in% c(1:4) & !is.null(fun)){
    print(paste("Warning: `fun` parameter will not be used!!",
                "Otherwise, set`metric = 5`", sep=" "))
  }

  opi_object <- list()

  textdoc <- as.data.frame(textdoc[,1])
  colnames(textdoc) <- "text"
  #head(textdoc)#nrow(textdoc)

  #create the id
  textdoc$ID <- seq.int(nrow(textdoc))

  #handle sentiment words preceded by negation words
  token_neg_pre <- suppressMessages(as_tibble(textdoc) %>%
    #unnest_tokens
    unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
    separate(bigram, c("word1", "word2"), sep = " ")%>%
    as.data.frame() %>%
    #get only words preceded by the 'negation' word
    filter(word1 %in% c("not", "never", "no", "without")) %>%
    #get sentiment score
    mutate(neg = paste(word1, word2, sep=" ")) %>%
    rename(word  = word2)%>%
    #left_join(get_sentiments("afinn")) %>% #
    left_join(afinn_111) %>% #
    select(ID, neg, value)%>%
    #reverse the score (and multiply by 2)
    mutate(value2 = value * 2)%>%
    mutate(value = value2)%>%
    select(-c(value2))%>%
    rename(word=neg)%>%
    filter(!is.na(value)))

  #handle all others
  token_regular <- suppressMessages(textdoc %>%
    #handle regular words
    unnest_tokens(word, text, drop = FALSE) %>%
    #join to the lexicon
    #inner_join(get_sentiments("afinn")) %>%
    inner_join(afinn_111) %>%
    select(-c(text)))#drop text

  #join both tables
  both_tokens <- data.frame(rbind(token_regular, token_neg_pre))

  afinn_OSD_each <- both_tokens %>%
    group_by(ID)%>%
    summarise(sentiment_score = sum(value))%>%
    #retain neutral
    #filter(sentiment_score != 0) %>%
    mutate(sentiment = if_else(sentiment_score > 0,
                                      "positive",
                                      if_else(sentiment_score < 0, "negative",
                                              "neutral")))%>%
    select(-c(sentiment_score))%>%
    ungroup()

    afinn_OSD <- afinn_OSD_each %>%
      group_by(sentiment)%>%
      #count the proportion of
      summarise(n=n())


    #to ensure that each value exist
    sent_gr <- data.frame(sentiment=c("negative", "positive", "neutral"),
                        n=0)
    wh <- sent_gr$sentiment %in% afinn_OSD$sentiment

    afinn_OSD <- afinn_OSD %>%
    bind_rows(sent_gr[which(wh==FALSE),])

    #calculate opinion score
    if (metric %in% c(1:4)){

      if(metric == 1){

        total_n <- sum(afinn_OSD$n)

        afinn_OSD <- afinn_OSD %>%
          rename(No_of_text_records=n)

        P <- afinn_OSD[which(afinn_OSD$sentiment == "positive"),2]
        N <- afinn_OSD[which(afinn_OSD$sentiment == "negative"),2]
        PD <- round(((P - N)/(P + N))*100,digits = 2)

        opi_object$sentiments <- knitr::kable(data.frame(afinn_OSD))
        opi_object$opiscore <- paste(PD, "%", sep="")
        opi_object$metric <- "Polarity (Percentage Difference)"
        opi_object$equation <- paste('((#Positive - #Negative)',
                                      '(#Positive + #Negative))*100%',
                                      sep="/")
        opi_object$OSD <- as_tibble(afinn_OSD_each)

      }
      if(metric == 2){
        P <- afinn_OSD[which(afinn_OSD$sentiment == "positive"),2]
        N <- afinn_OSD[which(afinn_OSD$sentiment == "negative"),2]
        O <- afinn_OSD[which(afinn_OSD$sentiment == "neutral"),2]
        PD <- round((abs(P - N) / (P + N + O))*100,digits = 2)

        total_n <- sum(afinn_OSD$n)

        afinn_OSD <- afinn_OSD %>%
          rename(No_of_text_records=n)

        opi_object$sentiments <- knitr::kable(data.frame(afinn_OSD))
        opi_object$opiscore <- paste(PD, "%", sep="")
        opi_object$metric <- "Polarity (Proportional Difference)"
        opi_object$equation <- paste('(abs(#Positive - #Negative)',
                 '(#Positive + #Negative + #neutral))*100%',
                                      sep="/")
        opi_object$OSD <- as_tibble(afinn_OSD_each)
      }

      if(metric == 3){
        P <- afinn_OSD[which(afinn_OSD$sentiment == "positive"),2]
        N <- afinn_OSD[which(afinn_OSD$sentiment == "negative"),2]
        O <- afinn_OSD[which(afinn_OSD$sentiment == "neutral"),2]
        PoS <- round((P / (P + N + O))*100, digits = 2)

        total_n <- sum(afinn_OSD$n)

        afinn_OSD <- afinn_OSD %>%
          rename(No_of_text_records=n)

        opi_object$sentiments <- knitr::kable(data.frame(afinn_OSD))
        opi_object$opiscore <- paste(PoS, "%", sep="")
        opi_object$metric <- "Positivity"
        opi_object$equation <-
          "(#Positive / (#Positive + #Negative + #neutral))*100%"
        opi_object$OSD <- as_tibble(afinn_OSD_each)
      }

      if(metric == 4){
        P <- afinn_OSD[which(afinn_OSD$sentiment == "positive"),2]
        N <- afinn_OSD[which(afinn_OSD$sentiment == "negative"),2]
        O <- afinn_OSD[which(afinn_OSD$sentiment == "neutral"),2]
        Neg <- round((N / (P + N + O))*100, digits = 2)

        total_n <- sum(afinn_OSD$n)

        afinn_OSD <- afinn_OSD %>%
          rename(No_of_text_records=n)

        opi_object$sentiments <- knitr::kable(data.frame(afinn_OSD))
        opi_object$opiscore <- paste(Neg, "%", sep="")
        opi_object$metric <- "Negativity"
        opi_object$equation <-
          "(#Negativity / (#Positive + #Negative + #neutral))*100%"
        opi_object$OSD <- as_tibble(afinn_OSD_each)
      }

  }

  if(metric == 5){
    P <- afinn_OSD[which(afinn_OSD$sentiment == "positive"),2]
    N <- afinn_OSD[which(afinn_OSD$sentiment == "negative"),2]
    O <- afinn_OSD[which(afinn_OSD$sentiment == "neutral"),2]
    userfun <- as.numeric(fun(P, N, O))

    total_n <- sum(afinn_OSD$n)

    afinn_OSD <- afinn_OSD %>%
      rename(No_of_text_records=n)

    opi_object$sentiments <- knitr::kable(data.frame(afinn_OSD))
    opi_object$opiscore <- userfun
    opi_object$metric <- "User-defined"
    opi_object$equation <- fun
    opi_object$OSD <- as_tibble(afinn_OSD_each)

    }

    return(opi_object)
  }
