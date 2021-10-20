
#' function to homogenise the residuals of a linear model
#' to make them exchangable.

#' Homogenises residuals from a lm object to make them exchangable.

#' @param mod  lm model

#' @return List of residuals and random effects that are exchangable.


residHomogenise.klm <- function(mod)
{
  ## extract resids, multiply by sqrt of weights
  ## and divide by hat values - taken from p279 in
  # Davison & Hinkley, 1997
  
  resids <- resid(mod) * (weights(mod) ^ 0.5) / (1 - hatvalues(mod)) ^ 0.5
  
  mn_resids <- mean(resids) # centre the residuals
  
  resids <- resids - mn_resids
  
  return(resids)
}