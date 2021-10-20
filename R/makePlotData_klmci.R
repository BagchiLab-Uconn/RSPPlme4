#' Format a klmci object for plotting of parameters and confidence intervals
#' from a bootstrap

#' @param x A klmci object from \code{\link{confint.klm}}

#' @return a \code{\link[tibble]{tibble}} of fixed effects against distance.

#' @family RSPP plot functions
#'
#' @importFrom dplyr rename mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

makePlotData_klmci <- function(x){
  dat <- x$pars
  dimnames(dat)[[1]][1] <- "Intercept"
  dat <- as.data.frame.table(dat)
  dat <- tidyr::pivot_wider(dat, names_from=.data$Var3, values_from=.data$Freq)
  dat <- dplyr::rename(dat, 
                       "distance" = "Var2", 
                       "term"="Var1", 
                       "lcl" = "lower", 
                       "ucl" = "upper")
    dat <-  dplyr::mutate(dat, distance = 
                            as.numeric(as.character(.data$distance)))
    return(dat)
}
