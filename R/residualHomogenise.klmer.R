#' Homogenises residuals from a lmer object to make them exchangable.
#' @import stats lme4
#' @param mod An lmer model
#'
#' @return List of residuals and random effects that are exchangable.
#' @export
#'

residualHomogenise.klmer <- function(mod){
  if(is.null(mod))
    return(NULL)
  ## Extract grouping levels for each level
  grps <-  getME(mod, "flist")

  ## get the weights
  if(!is.null(weights(mod)))
    wts <-   weights(mod)
  else wts <- rep(1,mod$dims$N) ## if no weights, set them all as equal -
  # this shouldn't happen!

  ## Pull out the variances and co-variances of blups at all levels
  vc <- VarCorr(mod)

  ## Pull out the BLUPs
  U_raw <- ranef(mod)

  ## rescale the random effects - there may be warnings here when
  ## a variance compoenent is near 0. The code corrects of that issue,
  ## if forcePD is true, but gives a warning.
  Unew <- rescaleblups(U = U_raw, Sigma=vc, forcePD=TRUE)

  ## Now correct residuals for the weights
  resids <- wts*resid(mod)^0.5
  resids <- resids - mean(resids) ## centre
  Ls1 <- sqrt(mean(resids^2)) ## empirical resid variance
  A1 <- sigma(mod)/Ls1   ## ratio of modelled to observed
  residNew <- resids*A1  ## make empirical resid variance = to modelled variance
  result <- c(level1resids=list(residNew), Unew)
  return(result)
}
