#' @title Highlights of importance of words
#' (terms) embedded in a text document
#' @description Produces a word cloud which represents the
#' level of importance of each word (across different text groups)
#' within a text document,
#' according to a specified measure.
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
#' importance of a word. Options include \code{'tf'}
#' representing `term frequency` and \code{'tf-idf'}
#' representing `term frequency inverse document frequency`
#' (Silge & Robinson, 2016).
#' @param n_top [integer] number of most-important words
#' to display (per group) in the textual representation
#' of the output. Default value: \code{5}.
#' @usage word_importance(textdoc, metric= "tf", n_top=5)
#' @examples
#' output <- word_importance(textdoc = policing_otd, metric= "tf",
#' n_top=5)
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
#' @importFrom dplyr summarise ntile
#' @importFrom cowplot plot_grid
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_col
#'
#' @export

word_importance <- function(textdoc, metric = "tf", n_top=5){

  output <- list()

  #global variables
  aes <- arrange <- collapse <- coord_flip <- desc <- facet_wrap <- filter <- geom_bar <-
    group_by <- labs <- mutate <- ntile <- rowname <- scale_alpha_discrete <-
    scale_fill_brewer <- select <- stopwords <- text <- text2 <-
    tf <- top_n <- ungroup <- word <- wordcloud2 <- img1 <- img2 <-img3 <-
    img4 <- img5 <- img6 <- img7 <- img8 <- img9 <- img10 <- img11 <-
    img12 <- img13 <- img14 <- img15 <- img16 <- img17 <- img18 <- img19 <-
    img20 <- groups <- tibble <- dev.new <- gg_color_hue <- hcl <-
    slice <- par <- scale_y_discrete <- scale_x_continuous <-
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
    unnest_tokens(word, text) #%>%

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

  if(metric == "tf"){
  tf <- data.frame(tf) %>%
      rename(freq=c)
      row.names(tf) <- tf$word
    #use only the top 1000 words
  tf_plot <- wordcloud2(data=tf[1:500,], size = 0.7, shape = 'rectangle')
  }

  if(metric == "tf-idf"){

  #colour function
  gg_color_hue <- function(p){
    hues <- seq(15, 375, length=p+1)
    hcl(h=hues, l =65, c=100)[1:p]
  }
  cols2 <- gg_color_hue(n_grp)

  #a rough way to do facet wrap..
  for(m in seq_len(n_grp)){ #m<-1

  topn <- data.frame(tf_idf) %>%
    filter(groups == m) %>%
    slice(tf_idf, 1:n_top)

    #topn
    par(mar=rep(0.8,4))
    img <- data.frame(tf_idf) %>%
      filter(groups == m) %>%
      mutate(groups = paste("Group",groups,sep=" "))%>%
      slice(tf_idf, 1:n_top) %>%
      ggplot(aes(tf_idf, fct_reorder(word, tf_idf)))+ #cols[3]
      geom_col(show.legend = FALSE) +
      geom_col(fill=cols2[m],show.legend = FALSE) +
      scale_y_discrete(labels = rev(topn$word))+
      facet_wrap(~groups, ncol = 2, scales="fixed") +
      scale_x_continuous(limits=c(min(tf_idf$tf_idf),max(tf_idf$tf_idf))) +
      labs(x = "tf_idf", y=NULL)+
      theme_bw()
      # theme(axis.title.x=element_blank(),
      #       axis.text.x=element_blank(),
      #       axis.ticks.x=element_blank())
    img

    if(m == 1){
      img1 <- img
    }
    if(m == 2){
      img2 <- img
    }
    if(m == 3){
      img3 <- img
    }
    if(m == 4){
      img4 <- img
    }
    if(m == 5){
      img5 <- img
    }
    if(m == 6){
      img6 <- img
    }
    if(m == 7){
      img7 <- img
    }
    if(m == 8){
      img8 <- img
    }
    if(m == 9){
      img9 <- img
    }
    if(m == 10){
      img10 <- img
    }
    if(m == 11){
      img11 <- img
    }
    if(m == 12){
      img12 <- img
    }
    if(m == 13){
      img13 <- img
    }
    if(m == 14){
      img14 <- img
    }
    if(m == 15){
      img15 <- img
    }
    if(m == 16){
      img16 <- img
    }
    if(m == 17){
      img17 <- img
    }
    if(m == 18){
      img18 <- img
    }
    if(m == 19){
      img19 <- img
    }
    if(m == 20){
      img20 <- img
    }

  images <- list(img1=img1, img2=img2, img3=img3,
                 img4=img4, img5=img5, img6=img6,
                 img7=img7, img8=img8,img9=img9,img10=img10,
                 img11=img11,img12=img12,img13=img13,
                 img14=img14,img15=img15,img16=img16,
                 img17=img17,img18=img18,img19=img19,
                 img20=img20)

  images <- images[which(images != "NULL")]

  tfidf_plot <- plot_grid(plotlist=images, ncol = 2)

    }
  }

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
