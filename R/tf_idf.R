#' @title Highlight the top-most important topics in a text document
#' @description This function identifies the most
#' important words across different text groups in a text document,
#' according to the \code{tf-idf} measure (Silge & Robinson, 2016).
#' @param textdoc An \code{n} x \code{1} list (dataframe) of
#' individual text records, where \code{n} is the total
#' number of individual records. An \code{n} x code{2} dataframe can
#' also be supplied, but the second column must contain
#' pre-defined group labels of the text records, e.g. group labels
#' according to geographical locations.
#' For an \code{n} x code{1} dataframe, the function
#' automatically impose arbitrary group labels on
#' the text document, based on the length \code{n} of the document.
#' Default: \code{FALSE}
#' @param n_top [integer] maximum number of top-most important words
#' per group to display. Default value: \code{10}
#' @param showplot To display graphical plot showing
#' ranks of top-most important words by group.
#' Default: \code{FALSE}
#' @usage tf_idf(textdoc, n_top=10, showplot=FALSE)
#' @details The function utilizes the \code{tf-idf} measure
#' in order to determine the most important words across various
#' text groups in a document. The idea of \code{tf-idf} is
#' to find words that are not used very much, but appear across
#' many groups in the document. using this function, a user
#' may be able to identify certain keywords that indicates some
#' underlying (secondary) subject that are been discussed
#' in relation to the original (primary) subject of the text
#' document.
#' @return A graphical display showing top-most important
#' words in each group, according to the \code{tf-idf}
#' measure.
#' @references Silge, J. and Robinson, D. (2016) tidytext:
#' Text mining and analysis using tidy data principles in R.
#' Journal of Open Source Software, 1, 37.
#' @importFrom tidytext bind_tf_idf

tf_idf <- function(textdoc, n_top=10, showplot=FALSE){


  output <- list()

  #global variables
  aes <- arrange <- collapse <- coord_flip <- desc <- facet_wrap <- filter <- geom_bar <-
  group_by <- labs <- mutate <- ntile <- rowname <- scale_alpha_discrete <-
    scale_fill_brewer <- select <- stopwords <- text <- tf <- top_n <- ungroup <- word <-
    groups <- tibble <- dev.new <- NULL

  #if the length of document is too small
  if(dim(textdoc)[1] < 20){
    stop(paste("Length of document is too small!!",
        "The minimum document length is 20!",
        "Process terminated!!", sep = " "))
  }

  #if the length of document is too large
  if(dim(textdoc)[1] > 10000000){
    stop(paste("Length of document is too large!!",
               "The maximum document length is 10 million records!!",
               "Process terminated!!", sep = " "))
  }


  #if a dataframe of more than two column is supplied
  if(length(dim(textdoc)) != 2 & (dim(textdoc)[2] != 1 | dim(textdoc)[2] != 2)){
    stop("Input data needs to be a dataframe containing 1 or 2 columns")
  }

  nr <- dim(textdoc)[1] #no. of rows
  nc <- dim(textdoc)[2] #no. of columns


  #if there are no groupings in the text document,
  #create an arbitrary group
  if(nc == 1){
    dat <- as.data.frame(textdoc)

    #create interval to determine number of arbitrary group
    #to impose on a document.
    no_of_grps <- c(5, 10, 15, 20)
    #abit_label <- data.frame(rbind(c(20, 200),c(201, 1000),
                                   #c(1001, 10000), c(10001, 10000000)))
    abit_label <- c(20, 200, 1000, 10000, 10000000)

    #join
    n_grp <- no_of_grps[findInterval(nr, abit_label)]

    #determine where data length fall in the
    #intervals

    series <- tibble()
    series <- tibble(text = as.character(dat[,1]))%>%
      tibble::rownames_to_column() #append rownames to the data
    series$groups <- ntile(as.numeric(series$rowname), n_grp)
    series <- series %>%
      select(-c(rowname))

  }

  #if there are groupings in the document,
  #check that there are at least 20 text records
  #per group
  #groups
  if(nc == 2){
    dat <- as.data.frame(textdoc)
    series <- tibble()
    series <- tibble(text = as.character(dat[,1]), groups = dat[,2])
    groups <- unique(as.character(dat[,2]))
    #if groups are less than 4, stop
    #if greater or equal to 4, okay
      if(length(groups) <= 3){
        stop(paste("The number of groups in the text document",
              "should be greater than 3!!",
              "And ensure that there are at least 20 text",
              "records per group", sep=" "))
      }
  }


  #tokenize
  tokenize_series <- series %>%
    group_by(groups)%>%
    collapse(text, sep= " ")%>%
    unnest_tokens(word, text) #%>%

  #removing stopwords
  tokenize_series <- tokenize_series[!tokenize_series$word %in% stopwords("english"),]

  #calculate tf_idf value per groups
  tf_idf <- tokenize_series %>%
    group_by(word, groups)%>%
    mutate(c=n())%>%
    bind_tf_idf(word, groups, c) %>%
    arrange(desc(tf_idf))%>%
    group_by(groups)%>%
    #remove duplicate
    filter(!duplicated(word))

  #tf_idf[which(tf_idf$group==7),]

  mode(tf_idf$groups)
  tf_idf$groups <- as.character(tf_idf$groups)

  #print results
  output[1] <- list(tf_idf)

  #show plot
  if(showplot == TRUE){
  dev.new()
  plt <- tf_idf %>%
    arrange(desc(tf_idf)) %>%
    #arrange(desc(tf)) %>%
    mutate(
           groups = factor(groups, levels = unique(groups))) %>%
    group_by(groups) %>%
    #top_n(10, wt = tf_idf) %>%
    top_n(n_top, wt = tf_idf) %>%
    ungroup() %>%
    #ggplot(aes(word, tf_idf, fill = Regions)) +
    ggplot(aes(word, tf, fill = groups)) +
    geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
    labs(title = "",
         x = NULL, y = "tf-idf") +
    facet_wrap(~groups, ncol = 2, scales = "free") +
    scale_alpha_discrete(range = c(0.4,1)) +
    scale_fill_brewer(palette = "Set1"
                      , name = "groups")+
    coord_flip()
  flush.console()
  print(plt)
  }
  #if false, do nothing
  if(showplot == FALSE){
    #do nothings
  }

  output[2] <- plt

  return(output)
}

#note I use 'tweets' here
#tf_idf(textdoc = tweets, n_top=10, showplot=FALSE)
