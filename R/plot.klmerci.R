
#' Plot a klmerci object  with parameters and confidence intervals
#' from a bootstrap

#' @param x  A klmerci object from \code{\link{confint.klmerHyper}}.
#' @param ... additional arguments (currently unused).
#' 
#' @return a ggplot object

#' @family RSPP plot functions
#'

#' @export

plot.klmerci <- function(x, ...){
  print(autoplot.klmerci(x, ...))
}
