
#' Semi-Parametric Bootstrap sample form a klmerHyper object.
#'
#' @param mods list of klmer models, usually from \code{\link{klmerHyper}}
#'
#' @return A list of simulated Kfunctions for all distances in klmerHyper
#' object.
#' @export

simulate.klmerHyper <- function(mods, resids)
{

  ## extract the level 1 residuals
  resids_e <- lapply(resids, function(x) x[['residuals']])

  ## Sample the residuals with replacement and correct for weights etc.
  resids_r <- residRandomise(resids_e, mods)

  ## randomise the random effects and correct for shrinkage.
  ranef_res <- lapply(resids, function(r)
    r[-which(names(r)=='residuals')]) ## extract random effects

  ## Randomise random effects.
  ranef_r <- ranefRandomise(mods=mods, ranefs=ranef_res)

  ## put sampled variance components together to be returned.
  resids <- mapply(function(resids_r, ranef_r) {
    c(resids_r = list(resids_r), ranef_r)
  }, resids_r=resids_r, ranef_r=ranef_r, SIMPLIFY=F)

  K_r <- mapply(generateK, mod=mods, resids=resids, SIMPLIFY=FALSE)

  return(K_r) ## return simulations for all distances
}

