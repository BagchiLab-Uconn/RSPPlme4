#' Simulate Deviance Differences between klmer Models Under a Null Hypothesis.
#'
#' @param modsH1 The full model of class \code{\link{klmerHyper}}
#' @param modsH0 The null model of class \code{\link{klmerHyper}}
#' @param resids Exchangeable residuals from modsH1
#' @param maxit The maximum number of iterations to try before giving up.
#'
#' @return The vector of simulated deviances for all distances.
#' @export
#'

modCompareBoot <- function(modsH1, modsH0, resids, maxit)
{
  do.again <- maxit
  while (do.again > 0)
  {
    ## Simulate new K function from the H0 (null) model.
    K_r <- simulate(modsH0, resids=resids)
    ## refit the models
    modsH0_r <- refit.klmerHyper(modsH0, newK=K_r)
    modsH1_r <- refit.klmerHyper(modsH1, newK=K_r)


    bootstrap_stat <- modCompare(modsH1=modsH1_r, modsH0=modsH0_r)

    ## If any errors or deviance changes < 0 repeat iteration
    do.again <- (do.again - 1) * (
      any(sapply(modsH0_r, inherits, "try-error")) |
      any(sapply(modsH0_r, inherits, "try-error")) |
        bootstrap_stat < 0
    )

    if(do.again > 0)
      warning("convergence error, repeating iteration")
  }
  bootstrap_stat[sapply(bootstrap_stat, is.null)] <- NA
  return(bootstrap_stat)

}
