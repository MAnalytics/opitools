#' @title Word Distribution
#' @description This function examines whether the distribution
#' of word frequency in a text document follows the Zipf distribution
#' (Zipf 1934). The Zipf's distribution is considered the ideal
#' distribution of a perfect natural language text.
#' @param textdoc \code{n} x \code{1} list (dataframe)
#' of individual text records, where \code{n} is the number
#' of individual records.
#' @usage word_distrib(textdoc)
#' @examples
#'
#' #Get an \code{n} x 1 text document
#' tweets_dat <- data.frame(text=tweets[,1])
#' plt = word_distrib(textdoc = tweets_dat)
#'
#' plt
#'
#' @details The Zipf's distribution is most easily observed by
#' plotting the data on a log-log graph, with the axes being
#' log(word rank order) and log(word frequency). For a perfect
#' natural language text, the relationship between the word rank
#' and the word frequency should have a negative slope with all points
#' falling on a straight line. Any deviation from the straight
#' line can be considered an imperfection attributable to the
#' texts within the document.
#' @return A list of word ranks and their respective
#' frequencies, and a plot showing the relationship between
#' the two variables.
#' @references Zipf G (1936). The Psychobiology of Language.
#' London: Routledge; 1936.
#' @importFrom tidytext unnest_tokens
#' @importFrom ggplot2 ggplot geom_line aes scale_color_manual
#' labs scale_x_log10 scale_y_log10 scale_shape_manual geom_abline
#' xlab ylab theme theme_light element_text element_rect geom_point
#' ggplot_build position_nudge geom_text
#' @importFrom tibble tibble tribble
#' @importFrom magrittr %>%
#' @importFrom dplyr summarise ungroup row_number
#' select count mutate filter rename
#' @importFrom cowplot get_legend plot_grid
#'
#' @export

word_distrib <- function(textdoc){

  outp <- list()

  #check that data is one-column
  if(dim(textdoc)[2]!=1){
    stop(paste("Dataframe must include only one column",
        "containing the text records!!", sep=" "))
  }

  dat <- list(textdoc)

  series <- tibble()

  #tokenize document
  tokenized <- tibble(text = as.character(unlist(dat)))%>%
    unnest_tokens(word, text)%>% #tokenize
    select(everything())
  series <- tokenized

  #compute term frequencies
  doc_words <- series %>%
    count(word, sort = TRUE) %>%
    ungroup()

  total_words <- doc_words %>%
    summarise(total = sum(n))

  doc_words <- cbind(doc_words, total_words)

  #plot log(rank order) vs. log(word frequency)
  freq_by_rank <- doc_words %>%
    mutate(rank = row_number(),
           `term_freq` = n / total)

  #---
  data <- word <- text <- everything <- summarise <- n <- mutate <-
    row_number <- total <- aes <- `term_freq` <- geom_line <-
    scale_x_log10 <- scale_y_log10 <- filter <- lm <- geom_abline <- xlab <-
    ylab <- theme_light <-Legend <- scale_colour_brewer <- x <- y <-
    position_nudge <- ggplot_build <- NULL

  # ggplot(freq_by_rank, aes(rank, `term_freq`)) +
  #   geom_line() +
  #   scale_x_log10() +
  #   scale_y_log10()

  #compare the distribution to a simple regression line
  #examine the head, tail and mid section of the plot

  lower_rank <- freq_by_rank %>%
    filter(rank < 500)%>%
    rename(`term_freq` = 5)
  int_slope <- lm(log10(`term_freq`) ~ log10(rank), data = lower_rank)

  #colors <- c("Sepal Width" = "blue", "Petal Length" = "red")

  #create a fictitious data to borrow its legend
  dat <- tribble(
    ~Legend, ~x, ~y,
    "LogFreq_vs_LogRank", -1, -1,
    "LogFreq_vs_LogRank", 1, 1,
    "Idealized_Zipfs_Law", -1, 1,
    "Idealized_Zipfs_Law", 1, -1
  )

  p <- ggplot(data=dat, aes(x, y, color=Legend)) +
    geom_line(size = 1.6, alpha = 0.8) +
    scale_color_manual(values = c(LogFreq_vs_LogRank = "red",
                                  Idealized_Zipfs_Law = "gray40"))


  #get legend
  leg <- get_legend(p)

  freq_by_rank <- freq_by_rank %>%
    mutate(group=1)

  #get index for labels
  xx <-round(nrow(freq_by_rank)/10, digits=0) - 2
  #create sequence
  idx <- xx
  for(i in 2:10){#i<-2
    idx <- rbind(idx, idx[i-1] + xx)
    #flush.console()
    #print(i)
  }

  idx <- as.vector(idx)

  #plot to first get x labels
  get_x <- ggplot(freq_by_rank, aes(rank, term_freq,
                                    color="Log(freq) vs. Log(r)")) +
    geom_line(size = 1.6, alpha = 0.8, colour="red") +
    geom_point(colour="red", alpha=0, size=1) +
    geom_text(data=freq_by_rank[idx,],
              aes(rank, term_freq,label=word))+
    labs(title="Checking text document against Zipf's law")+
    scale_x_log10() +
    scale_y_log10()

  get_x <- ggplot_build(get_x)$layout$panel_params[[1]]$x$get_labels()

  get_x <- as.numeric(get_x[!is.na(get_x)])


  lpt <- ggplot(freq_by_rank, aes(rank, term_freq,
                                  color="Log(freq) vs. Log(r)")) +
    geom_line(size = 1.6, alpha = 0.8, colour="red") +
    geom_point(colour="red", alpha=0, size=1) +
    geom_text(data=freq_by_rank[get_x,],
              aes(rank, term_freq,label=word, colour="black"),
              colour="black", position = position_nudge(y = 0.35))+
    labs(title="Checking text document against Zipf's law")+
    scale_x_log10() +
    scale_y_log10() +
    xlab("log(Rank)") +
    ylab("log(Term frequency)")+
    #theme(legend.position = "top")
    geom_abline(intercept = as.numeric(int_slope$coefficients[1]),
                slope = as.numeric(int_slope$coefficients[2]),
                color = c("gray40"), linetype = 2, size=1.2) +
    theme_light()+
    scale_shape_manual(values = 1) +
    labs(shape = "", linetype = "") +
    theme(panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12))

  final_lpt <- plot_grid(lpt, leg, ncol = 2, rel_heights = c(1, .1),
                         rel_widths = c(2,1), axis = "l")#,
                         #labels = c("AA","BB"),
                         #label_x = c(1.28, 0.495), label_y= c(0.53, 0.485))

  outp$freqRank <- freq_by_rank
  outp$plot <- final_lpt

  return(outp)

}

#plt = word_distrib(textdoc = policing_otd)
#print(plt)

#Ideal Zipf's law
