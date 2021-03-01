#' @title To simulate the expected sentiment (opinion) distribution
#' @description Given a text document with two underlying subjects
#' A and B, this function simulates the expected distribution of the
#' observed opinion score from the `opi_score`. The resulting
#' tidy-format dataframe is referred to as the `expected sentiment
#' document (ESD)` (Adepeju and Jimoh, 2021).
#' @param osd_data A list (dataframe). An \code{n} x \code{3}
#' OSD, in which \code{n} represents the number of number of
#' text records that have been successfully classified as
#' positive, negative or neutral. Column \code{1} of the OSD
#' is the record ID, column \code{2} shows the sentiment
#' classes (i.e. positive, negative, or neutral), while
#' Column \code{3} contains two indicator variables: `present`
#' and `absent`, to indicate records that consist and records
#' that do not consist, respectively, of any of
#' the identified secondary keywords.
#' @param nsim [integer] Number of replicas (ESD) to generate.
#' Recommended values: 99, 999, 9999, and so on. Since the run time
#' is proportional to the number of replicas, a lower number of
#' simulation is recommended. Default: \code{99}.
#' @param metric [integer] Metric to utilize for the calculation
#' of the opinion score. Default: \code{1}. See detailed documentation
#' in the \code{opi_score} function. The argument selected here must
#' correspond to that of \code{opi_score} function in order to
#' compute a statistical significance value (p-value).
#' @param fun A user-defined function provided parameter
#' \code{metric} is set as \code{5}. See detailed documentation
#' in the \code{opi_score} function.
#' @param quiet (TRUE or FALSE) To suppress processing and Warning
#' messages. Default: \code{TRUE}.
#' @usage opi_sim(osd_data, nsim=99, metric = 1, fun = NULL, quiet=TRUE)
#' @examples
#'
#' #Prepare an osd data from the output
#' #of `opi_score` function.
#'
#' score <- opi_score(textdoc = policing_otd,
#'                      metric = 1, fun = NULL)
#' #extract OSD
#' OSD <- score$OSD
#' #note that `OSD` is shorter in length
#' #than `policing_otd`, meaning that some
#' #text records were not classified
#'
#' #Bind a fictitious indicator column
#' set.seed(1000)
#' osd_data2 <- data.frame(cbind(OSD,
#'            keywords = sample(c("present","absent"), nrow(OSD),
#'            replace=TRUE, c(0.35, 0.65))))
#'
#' #generate expected distribution
#' exp_score <- opi_sim(osd_data2, nsim=99, metric = 1,
#'                                  fun = NULL, quiet=TRUE)
#' #preview the distribution
#' hist(exp_score)
#'
#' @details Uses randomization testing approach in
#' order to generate expected distribution of the observed
#' opinion scores (see details in Adepeju, M. and Jimoh, F., 2021).
#' @return Returns a list of expected opinion scores with length equal
#' to the number of simulation (\code{nsim}) specified.
#' @references (1) Adepeju, M. and Jimoh, F. (2021). An Analytical
#' Framework for Measuring Inequality in the Public Opinions on
#' Policing – Assessing the impacts of COVID-19 Pandemic using
#' Twitter Data. https://doi.org/10.31235/osf.io/c32qh
#' @importFrom tidytext unnest_tokens
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate
#' ungroup distinct select summarise bind_rows rename
#'
#' @export
#'
opi_sim <- function(osd_data, nsim=99, metric = 1, fun = NULL, quiet=TRUE){

  #options(warn=-1)

  sentiment <- keywords <- nnrow <- pos_neg_count <-
    prob2 <- head <- ID <- sentiment2 <-
    flush.console <-

  #check if randomization is too small
  if(nsim < 99){
    stop("Number of simulation (nsim) is too small!!")
  }


  if(nsim > 9999){
    stop(paste("Consider specifying a smaller",
               "number of simulations (nsim)!!", sep=" "))
  }

  #check metric
  if(!metric %in% c(1:5)){
    stop(paste(" 'Metric' argument can only assume values from",
               "1, 2,..., 5", sep=" "))
  }

  #check if a user-defined function is inputted
  if(metric == 5 & is.null(fun)){
    stop("A function (equation) is required in the parameter 'fun'")
  }

  if(metric %in% c(1:4) & !is.null(fun)){
    print(paste("Warning: `fun` parameter will not be used!!",
                "Otherwise, set`metric = 5`", sep=" "))
  }



    nsim_exp_scores <- NULL

    for(m in seq_len(nsim)){ #m<-1


      #create backup of the all 'neutrals'
      #for simulation, neutral remains untouched
      #to be appended back later
      neutral_osd <- osd_data %>%
        filter(sentiment == 'neutral')

      #filter neutral
      #simul is based on permutation
      #of positv and negat sentimnt labels
      p1 <- osd_data %>%
        filter(sentiment!="neutral")%>%
        group_by(keywords)%>%
        mutate(nnrow=n())%>%
        mutate(prob1=nnrow/nrow(osd_data))%>% #prob of absent
        ungroup()%>%
        group_by(keywords, sentiment) %>%
        mutate(pos_neg_count=n())%>%
        mutate(prob2=pos_neg_count/nnrow)##%>%

      len_excl_neutral <- nrow(p1)

      ab_class_sent <- p1[which(p1$keywords == "absent"),2]

      length_present_group <- length(which(p1$keywords == "present"))

      #now collate the unique probabilities of 'absent' class
      p1_prob <- p1 %>%
        filter(keywords == "absent")%>%
        distinct(sentiment, .keep_all = TRUE)%>%
        select(sentiment, prob2)


      if(nsim == 1){
        set.seed(len_excl_neutral)
        new_ex_class_sent <- sample(p1_prob$sentiment,
          length_present_group, replace=TRUE, prob = p1_prob$prob2)
      }

      if(nsim > 1){
      #generate samples of present class using the prob of absent class
        if(m == 1){
          set.seed(len_excl_neutral)
          new_ex_class_sent <- sample(p1_prob$sentiment,
            length_present_group, replace=TRUE, prob = p1_prob$prob2)
        }

        if(m > 1){
          #set.seed(nrow(data))
          new_ex_class_sent <- sample(p1_prob$sentiment,
            length_present_group, replace=TRUE, prob = p1_prob$prob2)
        }

      }

      new_ex_class_sent

      new_sentiment_list <- c(new_ex_class_sent,
                              as.vector(unlist(ab_class_sent)))

      #p1[which(p1$keywords == "present"), 3] <- new_ex_class_sent

      final_p1 <- data.frame(cbind(p1, sentiment2=new_sentiment_list))

      head(final_p1)

      #expected
      final_p1_ESD <- final_p1 %>%
        select(ID, sentiment, keywords, sentiment2)%>%
        mutate(sentiment = sentiment2)%>%
        select(-c(sentiment2))

      #now, prepare compute different opinion scores

      #append neutral list
      final_p1_ESD <- rbind(final_p1_ESD, neutral_osd)


      afinn_ESD <- final_p1_ESD %>%
        group_by(sentiment)%>%
        #count the proportion of
        summarise(n=n())


      #to ensure that each value exist
      sent_gr <- data.frame(sentiment=c("negative", "positive", "neutral"),
                            n=0)
      wh <- sent_gr$sentiment %in% afinn_ESD$sentiment

      #
      afinn_ESD <- afinn_ESD %>%
        bind_rows(sent_gr[which(wh==FALSE),])


      #calculate opinion score
      if (metric %in% c(1:4)){

        if(metric == 1){

          total_n <- sum(afinn_ESD$n)

          afinn_ESD <- afinn_ESD %>%
            rename(No_of_text_records=n)

          P <- afinn_ESD[which(afinn_ESD$sentiment == "positive"),2]
          N <- afinn_ESD[which(afinn_ESD$sentiment == "negative"),2]
          PD <- round(((P - N)/(P + N))*100,digits = 2)

          }
        if(metric == 2){
          P <- afinn_ESD[which(afinn_ESD$sentiment == "positive"),2]
          N <- afinn_ESD[which(afinn_ESD$sentiment == "negative"),2]
          O <- afinn_ESD[which(afinn_ESD$sentiment == "neutral"),2]
          PD <- round((abs(P - N) / (P + N + O))*100,digits = 2)

        }

        if(metric == 3){
          P <- afinn_ESD[which(afinn_ESD$sentiment == "positive"),2]
          N <- afinn_ESD[which(afinn_ESD$sentiment == "negative"),2]
          O <- afinn_ESD[which(afinn_ESD$sentiment == "neutral"),2]
          PD <- round((P / (P + N + O))*100, digits = 2)

        }

        if(metric == 4){
          P <- afinn_ESD[which(afinn_ESD$sentiment == "positive"),2]
          N <- afinn_ESD[which(afinn_ESD$sentiment == "negative"),2]
          O <- afinn_ESD[which(afinn_ESD$sentiment == "neutral"),2]
          PD <- round((N / (P + N + O))*100, digits = 2)

        }

      }

      if(metric == 5){
        P <- afinn_ESD[which(afinn_ESD$sentiment == "positive"),2]
        N <- afinn_ESD[which(afinn_ESD$sentiment == "negative"),2]
        O <- afinn_ESD[which(afinn_ESD$sentiment == "neutral"),2]
        PD <- as.numeric(fun(P, N, O))

    }
      nsim_exp_scores <- c(nsim_exp_scores,
                           as.numeric(as.character(PD)))

      if(quiet == FALSE){
        flush.console()
        print(paste("No. of simulations completed:", m, sep=" "))
      }
      if(quiet == TRUE){
        #do nothing
      }
    }

    return(nsim_exp_scores)

  }


#}
