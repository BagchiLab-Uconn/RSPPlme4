#' Builds a ggplot object to plot parameter estimates against distance

#' @param x A \code{\link{klmerHyper}} object.
#' @param ... additional arguments (currently unused).
#' 
#' @return a \code{\link[ggplot2]{ggplot}} object

#' @family RSPP plot functions
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap
#' @importFrom rlang .data
#' @export

autoplot.klmerHyper <- function(x, ...){
  pl <- ggplot2::ggplot(makePlotData_klmerHyper(x)) +
    ggplot2::geom_line(ggplot2::aes(x=.data$distance, y=.data$estimate)) + 
    ggplot2::facet_wrap(~.data$term, scales="free_y")
  return(pl)
}


