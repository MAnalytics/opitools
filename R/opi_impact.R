#' @title Impact analysis of subject B on the opinion expressed
#' concerning subject A in a text document
#' @description This function assesses the impacts of a subject
#' B (a secondary subject) on the opinion concerning subject A
#' (the primary subject) in a text document. Keywords relating
#' to the secondary subject, can be identified  using any
#' analytical techniques, such as the frequency analysis.
#' The keywords should then be collated provide as input into
#' this function (see below). The subject A (primary subject)
#' is usually the main theme of the text document. For instance,
#' by downloading Twitter data that include a set of related
#' hashtags; e.g. ’#police’, ’#policing’ and/or ’#law enforcement’,
#' then "Police or Policing" forms the primary subject of the
#' downloaded text document.
#' @param textdoc An \code{n} x \code{1} list (dataframe) of
#' individual text records, where \code{n} is the total
#' number of individual records.
#' @param sec_keywords [a list] A one-column dataframe (of any
#' number of length) containing a list of keywords relating
#' to the secondary subject (subject \code{B}).
#' @param metric [an integer] Specify the metric to utilize
#' for the calculation of opinion score. Default: \code{1}.
#' See detailed documentation
#' in the \code{opi_score} function.
#' @param fun A user-defined function given that parameter
#' \code{metric} (above) is set equal to \code{5}.
#' See detailed documentation
#' in the \code{opi_score} function.
#' @param nsim [an integer] Number of replicas (ESD) to generate.
#' See detailed documentation in the \code{opi_sim} function.
#' Default: \code{99}.
#' @param alternative [a character] Default: \code{"two.sided"},
#' indicating a two-tailed test. A user can override
#' this default value by specifying \code{“less”} or \code{“greater”} to run
#' the analysis as one-tailed test when the observed score
#' is located at the lower or upper regions of the expectation
#' distribution, respectively. Note: for \code{metric=1},
#' the `alternative` parameter should be
#' set equal to \code{"two.sided"} because the opinion score is
#' bounded by both positive and negative values. For an opinion
#' score bounded by positive values, such as when
#' \code{metric = 2, 3 or 4}, the `alternative` parameter
#' should be set as "greater", and set as "less" otherwise.
#' If metric parameter is set equal to \code{5}, with a user-defined
#' opinion score function (i.e. `fun` not NULL ), the user is required
#' to determine the boundary of the opinion scores, and set the
#' `alternative` argument appropriately.
#' @param quiet (TRUE or FALSE) To suppress processing
#' messages. Default: \code{TRUE}.
#' @usage opi_impact(textdoc, sec_keywords=NULL, metric = 1,
#' fun = NULL, nsim = 99, alternative="two.sided",
#' quiet=TRUE)
#' @examples
#' #test document: 'policing_otd'
#' #list of keywords: 'covid_keys'
#'
#' output <- opi_impact(textdoc = policing_otd,
#'           sec_keywords=covid_keys, metric = 1,
#'           fun = NULL, nsim = 99, alternative="two.sided",
#'           quiet=TRUE)
#'
#' #check output variables
#' print(output)
#'
#' #to access the pvalue
#' output$pvalue
#'
#' @details This function calculates the statistical
#' significance value (\code{p-value}) of an opinion score
#' by comparing the observed score (from the \code{opi_score}
#' function) with the expected scores (distribution) (from the
#' \code{opi_sim} function). The formula is given as
#' `p = (S.beat+1)/(S.total+1)`, where `S_total` is the total
#' number of replicas (`nsim`) specified, `S.beat` is number of replicas
#' in which their expected scores are than the observed score (See
#' further details in Adepeju and Jimoh, 2021).
#' @return Details of statistical significance of impacts
#' of a secondary subject `B` on the opinion concerning the
#' primary subject `A`.
#' @references (1) Adepeju, M. and Jimoh, F. (2021). An Analytical
#' Framework for Measuring Inequality in the Public Opinions on
#' Policing – Assessing the impacts of COVID-19 Pandemic using
#' Twitter Data. https://doi.org/10.31235/osf.io/c32qh
#' @importFrom tidytext unnest_tokens
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom purrr map_chr
#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom likert likert.options likert
#' likert.bar.plot
#' @importFrom dplyr filter mutate left_join arrange
#' select arrange if_else mutate_if

#' @export

opi_impact <- function(textdoc, sec_keywords=NULL, metric = 1,
                       fun = NULL, nsim = 99, alternative="two.sided",
                       quiet=TRUE){ #tweets

  keywords <- text <- ID <- sentiment<-flush.console <-asterisk <- comb <- NULL

  #output holder
  output <- list()

  #check if randomization is too small
  if(nsim < 99){
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
  if(metric == 1 & (alternative %in% c("less", "greater"))){
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
  colnames(textdoc) <- c("text")

  #separate `textdoc` into two:
  #textdoc_keypresent': contains any of the keywords
  #textdoc_keyabsent': contains no keywords

  textdoc_keypresent <- data.frame(textdoc) %>%
    filter(str_detect(text, sec_keywords, negate=FALSE)) %>%
    mutate(keywords = "present")

  if(nrow(textdoc_keypresent)==0){
    stop(paste("The text record contains NONE of",
               "the secondary keywords!! Operation terminated!!", sep=" "))
  }

  textdoc_keyabsent <- data.frame(textdoc) %>%
    filter(stringr::str_detect(text, sec_keywords, negate=TRUE))%>%
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
    arrange(keywords, desc(sentiment)) %>%
    select(-c(ID)) %>%
    mutate(class = if_else(sentiment == "neutral",
        paste("neutral", "", sep=""),
        paste(paste("key",keywords, sep="."), sentiment, sep="_")))%>%
    select(class)

  #hich(OSD_joined$sentiment == "neutral")
  #unique(likert_osd$class)

  #plot function here
  #if(pplot == TRUE){
    lik_p1 <- data.frame(likert_osd) %>% mutate_if(is.character,as.factor)
    # Make list of ordered factor variables
    out <- lapply(lik_p1, function(x) ordered(x,
           levels = c("key.absent_positive", "key.absent_negative",
           "neutral", "key.present_negative", "key.present_positive")))
    #  Combine into data.frame
    res <- do.call( data.frame , out )
    # Build plot
    likert.options(legend="Classes")
    p <- likert(res)
    title <- "Percentage proportion of classes"
    #plot(p, center=3, centered=FALSE) + ggtitle(title)
    pp <- likert.bar.plot(p, legend="Classes")
  #}

    #terminate process if keyword fields
    #does not include both 'present' and 'absent'
    pres_abs <- unique(OSD_joined$keywords)

    if(length(pres_abs) == 1){
      stop(paste("The 'sec_keywords' are either completely present",
                 "or absent in a sentiment class! The process terminated!!",
                 sep=" "))
    }

    #generate expected scores using `opi_sim` function
    expected_scores <- opi_sim(osd_data = OSD_joined,
                               nsim=nsim,
                               metric = metric,
                               fun = fun,
                               quiet=quiet)
  #}

  # if(quiet == FALSE){
  #   #generate expected scores using `opi_sim` function
  #   expected_scores <- opi_sim(osd_data = OSD_joined,
  #                              nsim=nsim,quiet=FALSE)
  # }

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
    select(comb)

  p_loc <- p_loc[, "comb"]


  #collate all results
  output$test <- "Test of significance (Randomization testing)"
  output$criterion <- alternative
  output$exp_summary <- summary(expected_scores)

  output$p_table <- knitr::kable(data.frame(cbind(observed_score,
        S_beat, nsim, pvalue=(S+1)/(nsim+1),   signif)))
  output$p_key <- rev(p_loc)
  output$p_formula <- "(S_beat + 1)/(nsim + 1)"
  output$plot <- pp

  return(output)

}
