#' @title Word (Term) Distribution
#' @description Models words in a text document in terms of the
#' Zipf's distribution (Zipf's 1936). Allows a user to assess
#' whether the content of text document follows the natural
#' language text distribution.
#' Generates Examines whether the word frequency in a text
#' document follows the Zipf distribution (Zipf 1934).
#' @param textdoc A collection (dataframe) of individual text
#' records, such as tweets or Facebook posts.
#' @usage word_distrib(textdoc)
#' @details The Zipf's distribution is most easily observed by
#' plotting the data on a log-log graph, with the axes being
#' log(term rank order) and log(term frequency). For a perfect
#' dataset, the relationship between rank and frequency should
#' have a negative slope with all points falling on a straight
#' line.
#' @return A graphical plot showing the rank-frequency graph.
#' @references Zipf G. The Psychobiology of Language.
#' London: Routledge; 1936.
#' the rank of word in the text document.
#' @importFrom tidytext unnest_tokens
#' @importFrom ggplot2 ggplot
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom plyr count
#'
#'
#' @export


word_distrib <- function(textdoc=data){

  dat <- list(textdoc)

  series <- tibble()

  #tokenize document
  tokenized <- tibble(text = as.character(unlist(dat)))%>%
                  unnest_tokens(word, text)%>% #tokenize
                  dplyr::select(everything())
  series <- tokenized

  #compute term frequencies
  doc_words <- series %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::ungroup()

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
    ylab <- theme_light <- scale_colour_brewer <- NULL

  ggplot(freq_by_rank, aes(rank, `term_freq`)) +
    geom_line() +
    scale_x_log10() +
    scale_y_log10()

  #compare the distribution to a simple regression line
  #examine the head, tail and mid section of the plot

  lower_rank <- freq_by_rank %>%
    filter(rank < 500)%>%
    dplyr::rename(`term_freq` = 5)
  int_slope <- lm(log10(`term_freq`) ~ log10(rank), data = lower_rank)

  lpt <- freq_by_rank %>%
    ggplot(aes(rank, `term_freq`, color = "red")) +
    geom_abline(intercept = as.numeric(int_slope$coefficients[1]), slope = as.numeric(int_slope$coefficients[2]),
                color = "gray50", linetype = 2, size=0.6) +
    geom_line(size = 1.6, alpha = 0.8) +
    xlab("log(Rank)") +
    ylab("log(Term frequency)")+
    scale_x_log10() +
    scale_y_log10() +
    theme_light()

  return(
  lpt + scale_colour_brewer(palette = "Set1")
  )


}
