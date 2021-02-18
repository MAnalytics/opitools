#' @title To assess the impact of subject B on subject A
#' @description To determine the impacts of the opinion
#' (inherent within a text document) concerning subject B
#' on the original subject A of the text document. For
#' Twitter data, subject A refers to the subject whose
#' related keywords (#hashtags) are used to download the
#' data. Subject B refers to any other secondary subject
#' that was being discussed in relation to subject A in the
#' text document. Keywords relating to secondary subject
#' need to be identified and input into \code{opi_mpact}
#' function. These keywords can be identified by running
#' a preliminary topic analysis on the downloaded text
#' document. We provided \code{tf_idf} function to help a
#' user identify important keywords that may signify a
#' secondary subject in a text document. A user is free to
#' deploy any other means in order to extract these important
#' (secondary) keywords. Alternatively, a user can specify
#' these keywords manually.
#' @param textdoc A collection (dataframe) of individual text
#' records, such as tweets or Facebook posts. The first column
#' of the dataframe must contain the text records.
#' @param sec_keywords [list] A one column  dataframe (of any
#' number of rows) containing a list of keywords relating to
#' the secondary subject (\code{B}).
#' @param metric [integer] Metric to utilize for the calculation
#' of the opinion score. Default: \code{1}. See the documentation
#' of \code{metric} parameter of \code{opi_score} function for details.
#' @param fun A user-defined function if parameter \code{metric}
#' is set as \code{5}. Also, see the documentation
#' of \code{fun} parameter of \code{opi_score} function for details.
#' @param nsim [integer] Number of replicas of the OSD to generate.
#' Recommended values: 99, 999, 9999, and so on. Since the run time
#' is proportional to the number of replicas, a lower number of
#' simulation is recommended. Default: \code{99}.
#' @param alternative [character] By default, this function will
#' assume a two-tailed test, as indicated by \code{"two.sided"} argument
#' in the `alternative` parameter. You can override
#' this by specifying \code{“less”} or \code{“greater”}, which will
#' run the analysis as a one-tailed test with the
#' criterion (i.e. the observed score) being located at the
#' lower or upper regions of the distribution, respectively.
#' Note: when \code{metric} parameter is set as \code{1},
#' `alternative` parameter is \code{"two.sided"}, while it is
#' \code{"less"} when \code{metric} parameter is set as \code{2},
#' \code{3}, or \code{4}. For a user-defined metric, i.e.
#' when \code{metric=5} with \code{fun} parameter specified,
#' the user needs to determine the bounds of the metric score
#' (see full documentation of \code{fun} parameter in
#' \code{opi_score} function). A simple rule that can be
#' applied is that if the opinion score can assume either
#' a negative or a positive value, then the `alternative`
#' argument should be set as "two.sided", else if it can
#' only assume a negative value, it should be set as "less",
#' and lastly, if it can only assume a positive value, then
#' it should be set as "greater". Note that the function is
#' able to detect a contradicting sign to the \code{less}
#' or \code{greater} argument, and automatically utilize the
#' \code{"two.sided"} criterion instead.
#' @param pplot [logical] To display graphical plot showing
#' the proportion of text records containing (or not
#' containing) secondary keywords (i.e. 'present' and
#' 'absent' groups), as well as the proportion of
#' 'positive' and 'negative' classes.
#' @param quiet (TRUE or FALSE) To suppress processing
#' messages. Default: \code{TRUE}.
#' @usage opi_impact(textdoc, sec_keywords=NULL, metric = 1,
#' fun = NULL, nsim = 99, alternative="two.sided", pplot=FALSE,
#' quiet=TRUE)
#' @details This function compares the observed opinion scores
#' (computed using the \code{opi_score} function) with the
#' expected opinion scores (distribution) from the
#' \code{opi_sim} function, in order to estimate the
#' statistical significance value (p-value). The p-value
#' is computed as p = (S.beat+1)/(S.total+1),
#' where 'S_total' is the total number of replicas created,
#' 'S.beat' is number of replicas with the expected score
#' greater than the observed score. If a user-defined opinion
#' score function is specified, he/she needs to determine
#' whether a one-tailed or two-tailed comparison is required.
#' Typically, a `99` replicas is sufficient.
#' Thus, if, for example, three of the 99
#' replicas have higher scores than observed score,
#' then the p-value is equal to (3+1)/(99+1) = 0.04.
#' @return Details of statistical significance of impacts
#' of subject B on subject A.
#' @references (1) Adepeju, M. and Jimoh, F. (2021). An Analytical
#' Framework for Measuring Inequality in the Public Opinions on
#' Policing – Assessing the impacts of COVID-19 Pandemic using
#' Twitter Data. https://doi.org/10.31235/osf.io/c32qh
#' @importFrom tidytext unnest_tokens
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom purrr map_chr
#' @export

opi_impact <- function(textdoc, sec_keywords=NULL, metric = 1,
                       fun = NULL, nsim = 99, alternative="two.sided",
                       pplot = FALSE, quiet=TRUE){ #tweets
  #output holder
  output <- list()

  #check if randomization is too small
  if(nsim < 9){
    stop("Number of simulation (nsim) is too small!!")
  }

  if(nsim > 9999){
    stop(paste("Consider specifying a smaller",
          "number of simulations (nsim)!!", sep=" "))
  }

  if(is.null(sec_keywords)){
    stop(" 'sec_keywords' parameter cannot be 'NULL'!! ")
  }

  #check any contradiction in tail comparison
  if(metric == 1 & alternative %in% c("less", "greater")){
    stop(paste("When parameter `metric = 1`, argument",
        " `alternative` must be set as 'two.sided'!! "))

  }

  if(metric %in% c(2:4) & alternative == "two.sided"){
    stop(paste('When parameter `metric =` ', metric,
          ", argument `two.sided` must be set as 'less'!! ", sep=""))
  }

  #format keywords
  sec_keywords <- data.frame(as.character(sec_keywords[,1]))
  colnames(sec_keywords) <- "keys"
  sec_keywords <-
    as.character(sec_keywords %>% map_chr(~ str_c(., collapse = "|")))

  #format text records
  textdoc <- data.frame(textdoc[,1])
  colnames(textdoc) <- "text"

  #separate `textdoc` into two:
  #'textdoc_keypresent': contains any of the keywords
  #'textdoc_keyabsent': contains no keywords

  textdoc_keypresent <- data.frame(textdoc) %>%
    dplyr::filter(stringr::str_detect(text, sec_keywords, negate=FALSE)) %>%
    mutate(keywords = "present")

  if(nrow(textdoc_keypresent)==0){
    stop(paste("The text record contains NONE any of",
               "the secondary keywords!! Operation terminated!!", sep=" "))
  }


  textdoc_keyabsent <- data.frame(textdoc) %>%
    dplyr::filter(stringr::str_detect(text, sec_keywords, negate=TRUE))%>%
    mutate(keywords = "absent")

  #combine and retain the IDs of text records
  #for later joining
  textdoc_comb <- rbind(textdoc_keypresent, textdoc_keyabsent)

  #create seq_ID
  textdoc_comb$ID <- seq.int(nrow(textdoc_comb))

  textonly <- data.frame(textdoc_comb$text) #head(textonly)
  colnames(textonly) <- "text"

  #drop the large text field
  textdoc_comb <- textdoc_comb %>%
    select(c(keywords, ID))

  #compute the OSD and opinion scores
  obj_both <- opi_score(textonly, metric = metric, fun = fun)

  #extract the score
  observed_score <- as.numeric(gsub("\\%", "", obj_both$opiscore))

  #extract the OSD
  OSD <- obj_both$OSD

  #join OSD and the with the pre-processed
  OSD_joined <- textdoc_comb %>%
    left_join(OSD) %>%
    #filter records that do not contain sentiment words
    filter(!is.na(sentiment)) %>%
    arrange(desc(keywords), sentiment)%>%
    select(ID, sentiment, keywords)

  #prepare data for Likert plot
  #filter neutral
  likert_osd <- OSD_joined %>%
    #filter(sentiment != "neutral") %>%
    dplyr::arrange(keywords, desc(sentiment)) %>%
    dplyr::select(-c(ID)) %>%
    dplyr::mutate(class = if_else(sentiment == "neutral", paste("neutral", "", sep=""),
                                  paste(paste("key",keywords, sep="."), sentiment, sep="_")))%>%
    dplyr::select(class)

  #hich(OSD_joined$sentiment == "neutral")
  #unique(likert_osd$class)

  #plot function here
  if(pplot == TRUE){
    dev.new(width=8,height=3,noRStudioGD = TRUE)
    #lik_p1 <- read.table(file = paste("Likert_Data",PR[m], ".csv", sep="_"), sep=",", head=TRUE)
    lik_p1 <- data.frame(likert_osd) %>% mutate_if(is.character,as.factor)
    # Make list of ordered factor variables
    out <- lapply(lik_p1, function(x) ordered(x, levels = c("key.absent_positive", "key.absent_negative", "neutral", "key.present_negative", "key.present_positive")))
    #  Combine into data.frame
    res <- do.call( data.frame , out )
    # Build plot
    likert.options(legend="Classes")
    p <- likert(res)
    title = "Percentage proportion of classes"
    #plot(p, center=3, centered=FALSE) + ggtitle(title)
    pp <- likert.bar.plot(p, legend="Classes")
  }

  if(pplot == FALSE){
    #do nothing
  }

  if(quiet == TRUE){
  #generate expected scores using `opi_sim` function
  expected_scores <- opi_sim(osd_data = OSD_joined,
                             nsim=nsim,quiet=TRUE)
  }

  if(quiet == FALSE){
  #generate expected scores using `opi_sim` function
  expected_scores <- opi_sim(osd_data = OSD_joined,
                             nsim=nsim,quiet=FALSE)
  }

  #check if there is contradiction in
  #the alternative argument
  if(observed_score > 0 & alternative == "less"){
    flush.console()
    print(paste("Warning: 'Observed score' is positive!!",
          "'two.sided' criterion is utilized!"))

    alternative <- "two.sided"
  }

  if(observed_score <= 0 & alternative == "greater"){
    flush.console()
    print(paste("Warning: 'Observed score' is negative!!",
                "'two.sided' criterion is utilized!"))

    alternative <- "two.sided"
  }

  #first, check the alternative argument
  #second, check the sign of the observed score.
  #third, compute the p-value
  if(alternative == "two.sided"){
    if(observed_score <= 0){
      S <- expected_scores[which(expected_scores <= observed_score)]
      S <- length(S)
    }
    if(observed_score > 0){
      S <- expected_scores[which(expected_scores > observed_score)]
      S <- length(S)
    }
    p <- round((S + 1)/(nsim + 1), digits = nchar(nsim))
  }


  if(alternative == "less"){
      S <- expected_scores[which(expected_scores <= observed_score)]
      S <- length(S)
      p <- round((S + 1)/(nsim + 1), digits = nchar(nsim))
  }

  if(alternative == "greater"){
    S <- expected_scores[which(expected_scores > observed_score)]
    S <- length(S)
    p <- round((S + 1)/(nsim + 1), digits = nchar(nsim))
  }


  #preparing final result
  S_beat <- S
  #significance level
  #first, create a table of significance
  #nsim=99
  ci <- c(95, 97.5, 99, 99.9, 99.99)
  v <- c(nsim, ((nsim + 1) - ((nsim + 1) * (ci/100))))
  #remove decimal locations
  v <- v[v >= 1]
  p_loc <- round(v/(nsim+1), digits = nchar(nsim+1))
  p_loc <- p_loc[order(p_loc)]
  #create list of asterix
  aster <- NULL
  for(i in 3:1){ #i=3
    n <- rep("*", i)
    aster <- rbind(aster, paste(n, collapse='' ))
  }
  aster <- rbind(aster, "'")
  p_loc <- data.frame(cbind(p_loc, aster))
  colnames(p_loc) <- c("p_loc", "asterisk")

  v <- c(v, 0)
  v <- v[order(v)]
  #where does S falls
  int <- findInterval(S_beat, v)
  signif <- p_loc[int,2]
  signif <- paste(signif, collapse='')

  #finally merge col
  p_loc <- p_loc %>%
    mutate(comb=paste(p_loc, asterisk, sep="")) %>%
    dplyr::select(comb)

  p_loc <- p_loc[, "comb"]


  #collate all results
  output$test <- "Test of significance (Randomization testing)"
  output$criterion <- alternative
  output$exp_summary <- summary(expected_scores)

  output$p_table <- knitr::kable(data.frame(cbind(observed_score, S_beat, nsim, p=(S+1)/(nsim+1),   signif)))
  output$p_key <- rev(p_loc)
  output$p_formula <- "(S_beat + 1)/(nsim + 1)"
  output$plot <- pp


  return(output)

}






