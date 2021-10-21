
#' Plot a klm object. Plots parameter estimates against distance

#' @param x A \code{\link{klm}} object.
#' @param ... additional arguments (currently unused).
#' 
#' @return a ggplot object

#' @family RSPP plot functions
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

plot.klm <- function(x, ...){
 print(autoplot.klm(x, ...))
}

 