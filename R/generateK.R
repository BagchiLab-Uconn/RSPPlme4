#' Generate Bootstrap Sample of K functions from models and resampled residuals.
#'
#' @param mod A \code{\link{klmer_i}} model.
#' @param resids The sampled residuals and random effects.
#'
#' @return A K function.


generateK <- function(mod, resids){

  n <-  length(resids$resids_r) # extract length of residuals

  mm <- c('resids' = list(rep(1, n)),
          lme4::getME(mod, "mmList"))

  summed_res <- mapply(function(z, b)
  {
    diag(z %*% t(b))
  }, z=mm, b=resids, SIMPLIFY=F)

  ## sum up the random effects and residuals across all levels
  summed_res <- Reduce('+', summed_res)

  ## bootstrap replicate of K function
  K_r <- fitted(mod) + summed_res
  return(K_r)
}
