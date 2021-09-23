#' Builds a ggplot object to plot parameter estimates with their 
#' confidence intervals against distance

#' @param x A klmerci object from \code{\link{confint.klmerHyper}}.
#' @param ... additional arguments (currently unused).
#' 
#' @return a \code{\link[ggplot2]{ggplot}} object

#' @family RSPP plot functions
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_hline facet_wrap
#' @importFrom rlang .data
#' @export

autoplot.klmerci <- function(x, ...){
  pl <- ggplot2::ggplot(makePlotData_klmerci(x), 
                        aes(x=.data$distance, y=.data$estimate, 
                            ymin=.data$lcl, ymax=.data$ucl)) +
  ggplot2::geom_ribbon(colour=NA, alpha=0.3) + 
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept=0, linetype='dotted') + 
  ggplot2::facet_wrap(~.data$term, scale='free')
return(pl)
}
