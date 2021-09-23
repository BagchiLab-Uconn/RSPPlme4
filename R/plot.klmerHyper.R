
#' Plot a klmerHyper object. Plots parameter
#' estimates distance

#' @param x A klmerHyper object
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
  dat <- data.frame(do.call('rbind', lapply(x, fixef)))
  names(dat)[1] <- "Intercept"
  dat$distance <- as.numeric(names(x))
  dat <- tidyr::pivot_longer(dat, cols = - .data$distance, names_to="term", values_to="estimate", 
                      names_transform=list(term = factor))
  dat$term <- relevel(dat$term, "Intercept")
  pl <- ggplot2::ggplot(dat) +
    ggplot2::geom_line(ggplot2::aes(x=.data$distance, y=.data$estimate)) + 
    ggplot2::facet_wrap(~.data$term, scales="free_y")
  return(pl)
}

