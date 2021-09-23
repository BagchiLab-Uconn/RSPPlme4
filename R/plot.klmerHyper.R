
#' Plot a klmerHyper object. Plots parameter
#' estimates distance

#' @param x A \code{\link{klmerHyper}} object.
#' @param ... additional arguments (currently unused).
#' 
#' @return a ggplot object

#' @family RSPP plot functions
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

plot.klmerHyper <- function(x, ...){
 print(autoplot.klmerHyper(x, ...))
}

 