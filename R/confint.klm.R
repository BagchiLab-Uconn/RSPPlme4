
#' Calculate confidence intervals on a klm object.
#'
#' @param object A \code{\link{klm}} object.
#' @param level Desired confidence intervals.
#' @param lin_comb Linear combintaion of the fixed parameters
#' @param object Bootstrap object run outside function. Defaults to NULL so
#' the bootstrap is run internally.
#' @param nboot Number of bootstrap simulations.
#' @param iseed Random number seed.
#' @param ... Additional arguments, currently ignored.
#'
#' @return Returns the confidence intervals on an klm object.
#' @export

confint.klm <- function(object, lincomb, nsim=1, level, iseed = NULL){
  
  ## repeats the bootstrap from lm.boot nsim times
  alpha <-  1 - level ## simplify calculations by taking 1 - alpha.
  
  if(!is.null(iseed))
  {
    oldseed <- .Random.seed
    on.exit( { .Random.seed <<- oldseed } )
    set.seed(iseed)
  }
  
  bsci <- replicate(nsim, bootstrap.klm(object, lincomb=lincomb), simplify=FALSE)
  
  ## extracts the observed k-function and standard error
  mod_pars <- lapply(object, function(x)
  {
    list(pars = coef(x), se_pars = sqrt(diag(vcov(x))))
  })
  
# extracts the predicted K-function and standard errors
  exp_pred <- lapply(object, function(mod_i)
  {
    pred <- lincomb %*% coef(mod_i)
    se_pred <- sqrt(diag(lincomb %*% vcov(mod_i) %*% t(lincomb)))
    return(list(pred=pred, se_pred=se_pred))
  })
  
  # t-distribution uses the difference between each
  ## bootstrap and the fitted, divides by the bootstrap standard error
  ## to get a "t-distribution" that is then multiplied by the observed
  ## standard error and added to the fitted values.
  
  t_pars <- sapply(bsci, function(sim, est){
    do.call("cbind", mapply(function(sim, est) {
      t_r <- (sim$pars - est$pars)/(sim$se_pars)
      (((est$pars - t_r * est$se_pars)))
    },  sim=sim, est=est, SIMPLIFY=FALSE))
  },est=mod_pars, simplify=FALSE)
  

  t_pars <- do.call(abind::"abind", args=list(t_pars, along=3))

  t_ci_pars <- apply(t_pars, c(1, 2), quantile, 
                     c(alpha/2, 1-alpha/2), na.rm = T)
  lcl_pars <-
    do.call("cbind", 
            lapply(mod_pars, function(x) x$pars)) -
    t_ci_pars[2 , ,] * 
    do.call("cbind", lapply(mod_pars, function(x) x$se_pars))
  
  ucl_pars <-
    do.call("cbind", lapply(mod_pars, function(x) x$pars)) -
    t_ci_pars[1 , ,] * 
      do.call("cbind", lapply(mod_pars, function(x) x$se_pars))
  
  est_pars <- do.call("cbind", lapply(mod_pars, function(x) x$pars))
  
  est_pars <- abind::abind(
    estimate = est_pars, 
    lower = lcl_pars, 
    upper = ucl_pars, 
    along = 3)
  
  t_pred <- lapply(bsci, function(sim, obs){
    do.call('cbind',  mapply(function(obs.t, sim.t){
      t.score.t <- (sim.t$pred - obs.t$pred)/sim.t$se_pred
      t.score.t <- matrix(t.score.t, nc=1)
      return(t.score.t)}, obs.t= obs, sim.t=sim, SIMPLIFY=F))}, obs=exp_pred)
  
  t_pred <- do.call(abind::'abind', args=list(t_pred, along=3))
  
  t_ci_pred <- apply(t_pred, c(1, 2), quantile, 
                     c(alpha/2, 1-alpha/2), na.rm = T)
  
  lcl_pred <- 
    sapply(exp_pred, function(x) x$pred) - 
    t_ci_pred[2 , ,] * sapply(exp_pred, function(x) x$se_pred)
  
  ucl_pred <- 
    sapply(exp_pred, function(x) x$pred) - 
    t_ci_pred[1 , ,] * sapply(exp_pred, function(x) x$se_pred)

  preds <- abind::abind(est_pred = sapply(exp_pred, function(x) x$pred),
                        ucl_pred = ucl_pred,
                        lcl_pred = lcl_pred, along=3)
  
  ci_boot <- list(predictions = preds, pars = est_pars) 


  attr(ci_boot, "level") <- level
  attr(ci_boot, "bootobj") <- bsci
  attr(ci_boot, "model") <- object
  class(ci_boot) <- "klmci"
  return(ci_boot)
    # 
    # return(list(lmK=mod, lmKpred=estimator,
    #           lower=lower.CI, upper=upper.CI))
}



