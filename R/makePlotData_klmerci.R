#' Format a klmerci object for plotting of parameters and confidence intervals
#' from a bootstrap

#' @param x A klmerci object from \code{\link{confint.klmer}}

#' @return a \code{\link[tibble]{tibble}} of fixed effects against distance.

#' @family RSPP plot functions
#'
#' @importFrom dplyr rename mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

makePlotData_klmerci <- function(x){
  dat <- x$pars_fixed
  dimnames(dat)[[1]][1] <- "Intercept"
  as.data.frame.table(dat) %>% 
    tidyr::pivot_wider(names_from=.data$Var3, values_from=.data$Freq) %>%
    dplyr::rename(
      "distance" = "Var2", 
      "term"="Var1", 
      "lcl" = "lower", 
      "ucl" = "upper") %>%
  dplyr::mutate(distance = as.numeric(as.character(.data$distance)))
}
