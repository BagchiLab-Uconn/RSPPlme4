
#' Plot a klmerci object  with parameters and confidence intervals
#' from a bootstrap

#' @param x  A klmerci object from \code{\link{confint.klm}}.
#' @param ... additional arguments (currently unused).
#' 
#' @return a ggplot object

#' @family RSPP plot functions
#'

#' @export

plot.klmci <- function(x, ...){
  print(autoplot.klmci(x, ...))
}
