
#' Plot a klmer object. Plots parameter
#' estimates distance

#' @param x A \code{\link{klmer}} object.
#' @param ... additional arguments (currently unused).
#' 
#' @return a ggplot object

#' @family RSPP plot functions
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

plot.klmer <- function(x, ...){
 print(autoplot.klmer(x, ...))
}

 