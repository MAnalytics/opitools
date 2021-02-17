#' @title Opinion score of a text document
#' @description Given a text document (concerning a subject A)
#' this function compute the overall opinion score based on the
#' proportion of positive and negative sentiments expressed across
#' individual text record within the document. The function
#' transforms the text document into a tidy-format dataframe,
#' referred to as `observed sentiment document (OSD)`, with each word
#' assigned a sentiment score. The overall sentiment score is
#' then computed from the OSD.
#' word
#' @param textdoc A collection (dataframe) of individual text
#' records, such as tweets or Facebook posts. The first column
#' of the dataframe must contain the text records.
#' @param metric [integer] Metric to utilize for the calculation
#' of the opinion score. Input values: \code{1, 2, ...,5}.
#' Assuming \code{P}, \code{N} and \code{O} represent positive,
#' negative, and neutral text records, respectively,
#' \code{1}: \code{((P - N)/(P + N))*100}, Polarity
#' (percentage difference) (Bound: -100%, +100%);
#' \code{2}: \code{((abs(P - N) / (P + N + O))*100},
#' Polarity (proportional difference) (Bound: 0, +100%);
#' \code{3}: \code{(P/ (P + N + O))*100}, Positivity
#' (Bound: 0, +100%); \code{4}: \code{(N / (P + N + O))*100},
#' Negativity (Bound: 0, +100%) (Malshe, A. 2019;
#' Lowe et al. 2011).\code{5}: To pass a
#' user-defined function as argument into \code{fun} parameter.
#' @param fun A user-defined function if parameter \code{metric}
#' is set as \code{5}. For example, myfun <- function(P, N, O){
#' ("some tasks to do"); return("a value")}.
#' \code{fun} parameter is then set as `fun = myfun`.
#' Default: \code{NULL} i.e. when \code{metric} parameter
#' is \code{1}, \code{2},..,or \code{4}.
#' Default: \code{FALSE}.
#' @usage opi_score(textdoc, metric = 1, fun = NULL)
#' @details An opinion score is derived from all the sentiments
#' (i.e. positive and negative) expressed within a text document, assuming
#' that each text record is considered to express only one sentiment
#' (i.e. positive, negative or neutral sentiment). We deploy
#' lexicon-based approach (Taboada et al. 2011) to determine
#' the sentiment of each text record, employing the `AFINN` lexicon
#' (Nielsen, 2011). Depending on the question at hand, a user can
#' retrieve any or all of the metrics above.
#' @return Returns an `opi_object` containing details of the
#' extracted sentiments and the computed opinion score.
#' @references Malshe, A. (2019) Data Analytics Applications.
#' Online. Available at:
#' https://ashgreat.github.io/analyticsAppBook/index.html.
#' Date accessed: 15th December 2020.
#' Taboada, M., Brooke, J., Tofiloski, M., Voll, K. and Stede, M., 2011.
#' Lexicon-based methods for sentiment analysis. Computational
#' linguistics, 37(2), pp.267-307.
#' Lowe, W., Benoit, K., Mikhaylov, S. and Laver, M., 2011.
#' Scaling policy preferences from coded political texts.
#' Legislative studies quarterly, 36(1), pp.123-155.
#' @import dplyr
# @importFrom plyr count
# @importFrom dplyr

# @importFrom textdata get_sentiments
# @importFrom tidytext unnest_token
# @importFrom reshape2 dcast unnest_token
#' @importFrom tidyr separate
#' @importFrom tidytext unnest_tokens
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr %>%
#' @export

opi_score <- function(textdoc=textonly, metric = 1, fun = NULL){

  #global variables
  ID <- as_tibble <- bigram <- get_sentiments <- neg <- sentiment <-
    sentiment_score <- separate <- text <- value <- value2 <-
    word <- word1 <- word2 <- NULL

  #check metric
  if(!metric %in% c(1:5)){
    stop(paste("Metric parameter can only assume values from",
               "1, 2, 3, 4, 5", sep=" "))
  }

  #check if a user-defined function is inputted
  if(metric == 5 & is.null(fun)){
    stop("A function in required in the parameter 'fun'")
  }

  if(metric %in% c(1:4) & !is.null(fun)){
    print(paste("Warning: `fun` parameter will not be used!!",
                "Otherwise, set`metric = 5`", sep=" "))
  }

  opi_object <- list()

  textdoc <- data.frame(textdoc[,1])
  colnames(textdoc) <- "text"
  #head(textdoc)#nrow(textdoc)

  #create the id
  textdoc$ID <- seq.int(nrow(textdoc))

  #handle sentiment words preceded by negation words
  token_neg_pre <- as_tibble(textdoc) %>%
    #unnest_tokens
    unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
    separate(bigram, c("word1", "word2"), sep = " ")%>%
    as.data.frame() %>%
    #get only words preceded by the 'negation' word
    dplyr::filter(word1 %in% c("not", "never", "no", "without")) %>%
    #get sentiment score
    dplyr::mutate(neg = paste(word1, word2, sep=" ")) %>%
    dplyr::rename(word  = word2)%>%
    left_join(get_sentiments("afinn")) %>% #
    dplyr::select(ID, neg, value)%>%
    #reverse the score (and multiply by 2)
    dplyr::mutate(value2 = value * 2)%>%
    dplyr::mutate(value = value2)%>%
    dplyr::select(-c(value2))%>%
    dplyr::rename(word=neg)%>%
    dplyr::filter(!is.na(value))

  #handle all others
  token_regular <- textdoc %>%
    #handle regular words
    unnest_tokens(word, text, drop = FALSE) %>%
    #join to the lexicon
    inner_join(get_sentiments("afinn")) %>%
    dplyr::select(-c(text))#drop text

  #join both tables
  both_tokens <- data.frame(rbind(token_regular, token_neg_pre))

  afinn_OSD_each <- both_tokens %>%
    dplyr::group_by(ID)%>%
    dplyr::summarise(sentiment_score = sum(value))%>%
    #retain neutral
    #dplyr::filter(sentiment_score != 0) %>%
    dplyr::mutate(sentiment = if_else(sentiment_score > 0,
                                      "positive",
                                      if_else(sentiment_score < 0, "negative",
                                              "neutral")))%>%
    dplyr::select(-c(sentiment_score))%>%
    ungroup()

    afinn_OSD <- afinn_OSD_each %>%
      group_by(sentiment)%>%
      #count the proportion of
      dplyr::summarise(n=n())


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
          dplyr::rename(No_of_text_records=n)

        P <- afinn_OSD[which(afinn_OSD$sentiment == "positive"),2]
        N <- afinn_OSD[which(afinn_OSD$sentiment == "negative"),2]
        PD <- round(((P - N)/(P + N))*100,digits = 2)

        opi_object$sentiments <- afinn_OSD
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

        opi_object$sentiments <- afinn_OSD
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

        opi_object$sentiments <- afinn_OSD
        opi_object$opiscore <- paste(PoS, "%", sep="")
        opi_object$metric <- "Positivity"
        opi_object$equation <- "(#Positive / (#Positive + #Negative + #neutral))*100%"
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

        opi_object$sentiments <- afinn_OSD
        opi_object$opiscore <- paste(Neg, "%", sep="")
        opi_object$metric <- "Negativity"
        opi_object$equation <- "(#Negativity / (#Positive + #Negative + #neutral))*100%"
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

    opi_object$sentiments <- afinn_OSD
    opi_object$opiscore <- userfun
    opi_object$metric <- "User-defined"
    opi_object$equation <- fun
    opi_object$OSD <- as_tibble(afinn_OSD_each)

    }

    return(opi_object)
  }
