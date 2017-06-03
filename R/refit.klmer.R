
#' Refits an klmer Model to the Bootstrapped K Function Data
#'
#' @param mod Original klmer model fitted to observed data.
#' @param res_r randomised residuals.
#'
#' @return An klmer model fitted to the randomised data.
#'
#' @note This is probably the time consuming step. Might be worth setting the
#' parameters of mod as starting values and increasing tolerances several fold.
#' We are only interested in the paramter estimates here, not their
#' uncertainties. Speeding up the code will make it feasible to increase
#' bootstraps, which is where better estimates of uncertainty will come from.
#'
#' @export
#'
refit.klmerHyper <- function(mods, newK){
  mapply(function(mod, K_r)
  {
    if(!is.null(mod))
    {
      ## refit model with new response
      mod_r <- try(lme4::refit(mod, K_r, calc.derivs=FALSE,
                               check.conv.hess=FALSE),
                   silent=TRUE)

      return(mod_r)
    }
    else
      return(NULL)
  }, mod=mods, K_r = newK, SIMPLIFY=FALSE)
}
