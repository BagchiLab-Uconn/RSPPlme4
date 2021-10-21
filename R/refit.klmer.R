#' Refits an klmer model to the Bootstrapped K Function Data
#'
#' @param mods Original \code{\link{klmer}} model fitted to observed data.
#' @param newK Simulated K functions.
#'
#' @return A klmer model fitted to the randomised data.
#'
#' @export
#'
refit.klmer <- function(mods, newK){
  mods <- mapply(function(mod, K_r)
  {
    if(!is.null(mod))
    {
      ## refit model with new response
      mod_r <- try(lme4::refit(mod, K_r,
                               control=lme4::lmerControl(calc.derivs=FALSE)), #,
                                                         ##check.conv.hess="ignore",
                                                         ##check.conv.grad = "ignore")),
                   silent=TRUE)
      return(mod_r)
    }
    else
      return(NULL)
  }, mod=mods, K_r = newK, SIMPLIFY=FALSE)
  class(mods) <- "klmer"
  return(mods)
}
