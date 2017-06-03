
#' Calculate Bootstrap Confidence Intervals on a klmerHyper object.
#'
#' @param mods A \code{\link{klmerHyper}} object.
#' @param lin_comb Linear combintaion of the fixed parameters
#' @param confint Desired confidence intervals.
#' @param bootobj Bootstrap object run outside function. Defaults to NULL so
#' the bootstrap is run internally.
#' @param nboot Number of bootstrap simulations.
#' @param ncore Number of cpus to use.
#' @param cltype Type of cluster to use.
#' @param iseed Random number seed.
#'
#' @return Returns the confidence intervals on an klmer object.
#' @export
#'
#' @examples
#'
confint.klmerHyper <- function(mods, lin_comb=NULL, bootobj=NULL, confint = 0.95,
                               nboot=NULL, ncore=1, cltype="PSOCK", iseed=NULL)
  {

  ## If bootstraps haven't been run yet, run now.
  if(is.null(bootobj))
    {
    if(is.null(nboot)|is.null(lin_comb))
      stop(paste("Must define lin_comb and nboot if no bootobj passed to confint.",
                 "\nConsider defining parameters for parallel processing:
                 ncore and cltype.\n"))

    bootobj <- bootParallel.klmerHyper(mods,lin_comb=lin_comb, nboot=nboot,
                                         ncore=ncore, cltype=cltype, iseed=iseed)

  }

  lin_comb <- attr(bootobj,  "linear.combination")

  alpha <-  1 - confint ## simplify calculations by taking 1 - alpha.

  ## Extract parameters from the models.
  mod_pars <- sapply(mods, getPars, lin_comb =lin_comb,
                     simplify=FALSE)

  t_fixed <- sapply(bootobj, function(sim, est){
    mapply(function(sim, est) {
      t_r <- (sim$beta_r- est$beta_r)/sqrt(diag(sim$vcov_r))
      est$beta_r - t_r * sqrt(diag(est$vcov_r))
    },  sim=sim, est=est, SIMPLIFY=TRUE)
  },est=mod_pars, simplify=FALSE)


  ## pull ou the cis of the parameter estimates
  cis_fixed <- apply(do.call(abind::"abind", args=list(what=t_fixed, along=3)),
                      c(2, 1), quantile, c(alpha/2, 1-alpha/2), na.rm=T)

  ## Extract and organise fixed effect paramter estimates
  est_fixed <- aperm(sapply(mod_pars, function(x) x$beta_r), c(2,1))
  est_fixed <- array(est_fixed, dim=c(1, dim(est_fixed)))

  pars_fixed  <- abind::abind(list('estimate'=est_fixed, cis_fixed), along=1)

  ##construct the 't-distributions' for the predictions
  t_pred <- lapply(bootobj, function(sim, obs)
  {
    as.matrix(mapply(function(sim, obs){
      t_r <- (sim$pred_r - obs$pred_r)/sim$se_pred_r
      return(t_r)
    }, sim=sim, obs=obs))
  }, obs=mod_pars)

  ## turn these into a matrix
  t_pred <- do.call(abind::"abind", args=list(what=t_pred, along=3))
  uci_pred <- apply(t_pred, c(2, 1), quantile, alpha/2, na.rm=T) ## upper CI
  lci_pred <- apply(t_pred, c(2, 1), quantile, 1-alpha/2, na.rm=T) ##  lower CI

  est_pred <- t(do.call(abind::"abind",
                      args=list(what = lapply(mod_pars, function(x) x$pred_r),
                                along=2)))
  se_pred <- t(do.call(abind::"abind",
                        args=list(what = lapply(mod_pars, function(x) x$se_pred_r),
                                  along=2)))
  ucl_pred <- est_pred - uci_pred*se_pred
  lcl_pred <- est_pred - lci_pred*se_pred

  preds <- abind::abind(est=est_pred, ucl_pred=ucl_pred,
                        lcl_pred=lcl_pred, along=3)
  ## organise into return object
  ci_boot <- list(predictions = preds, pars_fixed=pars_fixed)
  attr(ci_boot, "bootobj") <- bootobj
  class(ci_boot) <- "klmerci"
  return(ci_boot)
}
