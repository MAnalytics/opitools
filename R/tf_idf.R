#' @title Detect important topics in a text document
#' @description Calculates the tf-idf metric
#' (Silge & Robinson, 2016) in order to determine
#' the important topics in a document. The \code{tf-idf}
#' value represents level of importance of a given
#' topic (or word) across the group of text that exist in a document.
#' @param textdoc A collection (dataframe) of individual text
#' records, such as tweets or Facebook posts.
#' By default: The \code{textdoc} is a one-column dataframe.
#' If contains two-columns, the second column must contain
#' text group labels.
#' @param grouping Indicate whether the dataframe
#' includes a second column indicating a defined groupings.
#' If \code{FALSE}, the function impose arbitrary groups.
#' Default: \code{FALSE}
#' @param n [integer] the number of top important words across
#' the text document to display. Default value: \code{10}
#' @param showplot To show graphical plot showing
#' rank of important words by group. Default: \code{FALSE}
#' @usage tf_idf(textdoc = tweets, grouping=FALSE, n=10, showplot=FALSE)
#' @details The idea of tf-idf is to find important
#' words from the content of the text document across various
#' text groups by decreasing the weight for commonly used words
#' and increasing the weight for words that are not used very
#' much in the documents. Based on the result, a user may be
#' able to identify words that indicates certain underlying
#' subjects that relate to the original subject of the text
#' document. A user can collate these words and use them
#' as input into the \code{sec_keywords} parameter of the
#' \code{opi_impact} function. For this function, we built on
#' the `bind_tf_idf` function from the tidytext R-package
#' (Silge & Robinson, 2016).
#' @return List of topics with their respective tf-idf values
#' (importance).
#' @references Silge, J. and Robinson, D. (2016) tidytext:
#' Text mining and analysis using tidy data principles in R.
#' Journal of Open Source Software, 1, 37.
#' @importFrom tidytext bind_tf_idf

tf_idf <- function(textdoc = tweets, grouping=FALSE, n=10, showplot=FALSE){

  output <- list()

  #global variables
  aes <- arrange <- collapse <- coord_flip <- desc <- facet_wrap <- filter <- geom_bar <- group
  group_by <- labs <- mutate <- ntile <- rowname <- scale_alpha_discrete <-
    scale_fill_brewer <- select <- stopwords <- text <- tf <- top_n <- tweets <- ungroup <- word <-
    group <- dev.new <- NULL

  #checks
  if(length(dim(textdoc)) != 2 & (dim(textdoc)[2] != 1 | dim(textdoc)[2] != 2)){
    stop("Input data needs to be a dataframe containing 1 or 2 columns")
  }

  nr <- dim(textdoc)[1] #no. of rows
  nc <- dim(textdoc)[2] #no. of columns

  #if there are no groupings in the text document
  if(grouping == FALSE & nc != 1){
    stop(paste("Input data has more than one field!",
               "Check the `documentation` of `grouping` argument", sep=" "))
  }
  #if no grouping field is provided
  if(grouping == TRUE & nc == 1){
    stop(paste("Input data has one field!",
               "Check the `documentation` of `grouping` argument", sep=" "))
  }


  #check that 'grouping' parameter is correct
  if(nc == 1 ){
    #create 10 group and append to the data
    #prepare the document
    dat <- as.data.frame(textdoc)
    series <- tibble()
    series <- tibble(text = as.character(dat[,1]))%>%
      tibble::rownames_to_column() #append rownames to the data
    series$group <- ntile(as.numeric(series$rowname), 10)
    series <- series %>%
      select(-c(rowname))

  }

  #if there are groups
  if(nc == 2){
    dat <- as.data.frame(textdoc)
    series <- tibble()
    series <- tibble(text = as.character(dat[,1]), group = dat[,2])
    groups <- unique(as.character(dat[,2]))
    #if groups are less than 4, stop
    #if greater or equal to 4, okay
      if(length(groups) <= 3){
        stop("The number of groups in the text document should be greater than 3 for better results")
      }
  }

  #tokenize
  tokenize_series <- series %>%
    group_by(group)%>%
    collapse(text, sep= " ")%>%
    unnest_tokens(word, text) #%>%

  #removing stopwords
  tokenize_series <- tokenize_series[!tokenize_series$word %in% stopwords("english"),]

  #calculate tf_idf value per groups
  tf_idf <- tokenize_series %>%
    group_by(word, group)%>%
    mutate(c=n())%>%
    bind_tf_idf(word, group, c) %>%
    arrange(desc(tf_idf))%>%
    group_by(group)%>%
    #remove duplicate
    filter(!duplicated(word))

  #tf_idf[which(tf_idf$group==7),]

  mode(tf_idf$group)
  tf_idf$group <- as.character(tf_idf$group)

  #print results
  output[1] <- list(tf_idf)

  #show plot
  if(showplot == TRUE){
  dev.new()
  plt <- tf_idf %>%
    arrange(desc(tf_idf)) %>%
    #arrange(desc(tf)) %>%
    mutate(
           groups = factor(group, levels = unique(group))) %>%
    group_by(group) %>%
    #top_n(10, wt = tf_idf) %>%
    top_n(n, wt = tf_idf) %>%
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
  }
  #if false, do nothing
  if(showplot == FALSE){
    plt <- "Plot disabled"
    #do nothings
  }

  output[2] <- plt

  return(output)
}
