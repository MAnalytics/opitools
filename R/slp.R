#' @title yes
#' @description this
#' @param x [integer]: value ihj jyg
#' @usage testit(x=2)
#' @details this
#' @examples testit(x=2)
#' @return
#' @references kml is one
#' @importFrom ggplot2 ggplot
#' @export

#function to put PC to sleep for an x number of seconds
testit <- function(x=2){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}
