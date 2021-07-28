#' @title Importance of words (terms) embedded
#' in a text document
#' @description Produces a wordcloud which represents the
#' level of importance of each word (across different text groups)
#' within a text document, according to a specified measure.
#' @param textdoc An \code{n} x \code{1} list (dataframe) of
#' individual text records, where \code{n} is the total
#' number of individual records. An \code{n} x code{2} dataframe can
#' also be supplied, in which the second column represents the
#' labels of the pre-defined groupings of the text records,
#' e.g. labels of geographical areas where each text record
#' originates.
#' For an \code{n} x \code{1} dataframe, an arbitrary grouping is
#' automatically imposed.
#' @param metric (character) The measure for determining the level of
#' importance of each word within the text document. Options include \code{'tf'}
#' representing `term frequency` and \code{'tf-idf'}
#' representing `term frequency inverse document frequency`
#' (Silge & Robinson, 2016).
#' @param words_to_filter A pre-defined vector of words (terms) to
#' filter out from the DTD prior to highlighting words importance.
#' default: \code{NULL}. This parameter helps to eliminate
#' non-necessary words that may be too dominant in the results.
#' @usage word_imp(textdoc, metric= "tf",
#' words_to_filter=NULL)
#' @examples
#' #words to filter out
#' wf <- c("police","policing")
#' output <- word_imp(textdoc = policing_dtd, metric= "tf",
#' words_to_filter= wf)
#' @details The function determines the most important words
#' across various grouping of a text document. The measure
#' options include the `tf` and `tf-idf`. The idea of `tf`
#' is to rank words in the order of their number of occurrences
#' across the text document, whereas `tf-idf` finds words that
#' are not used very much, but appear across
#' many groups in the document.
#' @return Graphical representation of words importance
#' according to a specified metric. A wordcloud is used
#' to represent words importance if `tf` is specified, while
#' facet wrapped histogram is used if `tf-idf` is specified.
#' A wordcloud is represents each word with a size corresponding
#' to its level of importance. In the facet wrapped histograms
#' words are ranked in each group (histogram) in their order
#' of importance.
#' @references Silge, J. and Robinson, D. (2016) tidytext:
#' Text mining and analysis using tidy data principles in R.
#' Journal of Open Source Software, 1, 37.
#' @importFrom tidytext bind_tf_idf
#' @importFrom tm stopwords
#' @importFrom tidytext unnest_tokens
#' @importFrom wordcloud2 wordcloud2
#' @importFrom dplyr summarise ntile select arrange
#' @importFrom cowplot plot_grid
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_col
#'
#' @export

word_imp <- function(textdoc, metric = "tf",
                            words_to_filter=NULL){

  output <- list()

  #global variables
  aes <- freq<- arrange <- collapse <- coord_flip <- desc <- facet_wrap <- filter <- geom_bar <-
    group_by <- labs <- mutate <- ntile <- rowname <- scale_alpha_discrete <-
    scale_fill_brewer <- select <- stopwords <- text <- text2 <-
    tf <- top_n <- ungroup <- word <- wordcloud2 <- img1 <- img2 <-img3 <-
    img4 <- img5 <- img6 <- img7 <- img8 <- img9 <- img10 <- img11 <-
    img12 <- img13 <- img14 <- img15 <- img16 <- img17 <- img18 <- img19 <-
    img20 <- groups <- tibble <- dev.new <- gg_color_hue <- hcl <-
    slice <- par <- scale_y_discrete <- scale_x_continuous <- dec_place <-
    number <- incr <- bigN2 <- differentiator <-
    theme_bw <- NULL

  #if the length of document is too small
  if(dim(textdoc)[1] < 20){
    stop(paste("Length of document is too small!!",
               "The minimum allowable length is 20!",
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
    #create interval to determine the number of arbitrary group
    #to impose on the document.
    no_of_grps <- c(5, 10, 15, 20)
    #abitr_label <- data.frame(rbind(c(20, 200),c(201, 1000),
    #c(1001, 10000), c(10001, 10000000)))
    abit_label <- c(20, 200, 1000, 10000, 10000000)

    #number of arbitrary groups
    n_grp <- no_of_grps[findInterval(nr, abit_label)]

    #determine where data length fall in the
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
      stop(paste("Number of groups",
                 "should be greater than 3!",
                 "And, ensure no group has less than",
                 "20 text records", sep=" "))
    }
  }


  #tokenize
  tokenize_series <- series %>%
    group_by(groups)%>%
    #paste(text, collapse = " ")
    summarise(text2 = paste(text, collapse=" "))%>%
    rename(text=text2)%>%
    unnest_tokens(word, text) %>%
    dplyr::filter(!word %in% words_to_filter)

  #removing stopwords
  tokenize_series <- tokenize_series[!tokenize_series$word %in% stopwords("english"),]

  #calculate 'tf' across all groups & 'tf_idf' value per groups
  tf <- tokenize_series %>%
    select(word)%>%
    group_by(word)%>%
    summarise(c=n())%>%
    #bind_tf_idf(word, groups, c) %>%
    arrange(desc(c))

  tf_idf <- tokenize_series %>%
    group_by(word, groups)%>%
    summarise(c=n())%>%
    bind_tf_idf(word, groups, c) %>%
    arrange(desc(tf_idf))%>%
    group_by(groups)%>%
    #remove duplicate
    filter(!duplicated(word))

  #tf_idf[which(tf_idf$group==7),]

  mode(tf_idf$groups)
  tf_idf$groups <- as.character(tf_idf$groups)

  #print results
  #output[1] <- list(tf_idf)
  #output[2]

  #function to find the number decimal places
  num.decimals <- function(x) {
    stopifnot(class(x)=="numeric")
    x <- sub("0+$","",x)
    x <- sub("^.+[.]","",x)
    nchar(x)
  }

  #if(metric == "tf"){
  tf <- data.frame(tf) %>%
      rename(freq=c)
      row.names(tf) <- tf$word
    #use only the top 1000 words
  tf <- tf[1:500,]
  tf_plot <- wordcloud2(data=tf, size = 0.7, shape = 'circle')
  #}

  #borrow freq list of 'tf'
  magn_tf <- tf$freq

  #if(metric == "tf-idf"){
    tf_idf <- data.frame(tf_idf) %>%
      rename(freq=tf_idf) %>%
      select(word, freq) %>%
      arrange(-freq, word) %>%
      mutate(freq=round(freq, digits = 3))%>%
      mutate(dec_place = num.decimals(freq))%>%
      filter(!duplicated(word))%>%
      group_by(freq) %>%
      mutate(count=n())%>%
      mutate(bigN = as.numeric(paste("1e-", dec_place, sep=""))) %>%#modify by adding values
      mutate(number=1)%>%
      mutate(incr = cumsum(number))%>%
      mutate(bigN2 = as.numeric(paste(paste("1e-", dec_place, sep=""),incr, sep="")))%>%
      mutate(differentiator = freq + bigN2) %>%
      mutate(freq=differentiator)%>%
      select(word, freq)

    tf_idf <- data.frame(tf_idf[1:length(magn_tf),])
    #tf_idf <- tf_idf[1:500,]
    tf_idf$freq <- magn_tf
    #tf_idf <- tf_idf %>%
      #select(word, freq)
    #use only the top 500 words
    tfidf_plot <- wordcloud2(data=tf_idf, size = 0.7, shape = 'circle')
  #}

  if(metric == "tf"){
    mtr <- "term frequency"
  }
  if(metric == "tf-idf"){
    mtr <- "term frequency-inverse document frequency"
  }
  if(metric == "tf"){
    table <- as_tibble(tf)
  }
  if(metric == "tf-idf"){
    table <- tf_idf
  }
  if(metric == "tf"){
    plt <- tf_plot
  }
  if(metric == "tf-idf"){
    plt <- tfidf_plot
  }
  output <- list(metric = mtr,
                 table = table,
                 plot = plt)

  return(output)
}
