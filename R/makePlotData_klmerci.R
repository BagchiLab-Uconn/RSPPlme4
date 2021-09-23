#' Format a klmerci object for plotting of parameters and confidence intervals
#' from a bootstrap

#' @param x A klmerci object from \code{\link{confint.klmerHyper}}

#' @return a \code{\link[tibble]{tibble}} of fixed effects against distance.

#' @family RSPP plot functions
#'
#' @importFrom dplyr rename mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

makePlotData_klmerci <- function(x){
  dat <- x$pars_fixed
  dimnames(dat)[[3]][1] <- "Intercept"
  dat <- as.data.frame.table(dat) %>% 
    tidyr::pivot_wider(names_from=.data$Var1, values_from=.data$Freq) %>%
    dplyr::rename("distance" = "Var2", "term"="Var3", 
                  "lcl" = "2.5%", "ucl" = "97.5%") %>%
  dplyr::mutate(distance = as.numeric(as.character(.data$distance)))
}
