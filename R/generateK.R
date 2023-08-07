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

  if(!is.null(attr(mod@frame, "na.action")))
  {
    omitted_rows  <- attr(mod@frame, "na.action")
    K_r <- rep(NA, n + length(omitted_rows))
    
    ## bootstrap replicate of K function
    K_r[-omitted_rows] <- fitted(mod) + summed_res
  }
  else
    K_r <- fitted(mod) + summed_res
  
  return(K_r)
}
