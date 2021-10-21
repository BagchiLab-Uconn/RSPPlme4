
#' Calculate confidence intervals on a klm object.
#'
#' @param object A \code{\link{klm}} object.
#' @param parm Parameter to get confidence interval for. Currently ignored.
#' @param level Desired confidence intervals.
#' @param nboot Number of bootstrap simulations.
#' @param lin_comb Linear combination of the fixed parameters. Defaulats
#' to NULL to extract it from the fitted model.
#' @param iseed Random number seed.
#' @param ... Additional arguments, currently ignored.

#'
#' @return Returns the confidence intervals on an klm object.
#' @export

confint.klm <- function(object, parm, level = 0.95, lin_comb = NULL, nboot=1, iseed = NULL, ...){
  
  alpha <-  1 - level ## simplify calculations by taking 1 - alpha.
  
  if(!is.null(iseed))
  {
    oldseed <- .Random.seed
    on.exit( { .Random.seed <<- oldseed } )
    set.seed(iseed)
  }
  
  if(is.null(lin_comb))
      lin_comb <- object[[1]]$x
  
  
  ## repeats the bootstrap from lm.boot nboot times
  bootobj <- replicate(nboot, bootstrap.klm(object, lin_comb=lin_comb), simplify=FALSE)
  
  ## extracts the observed k-function and standard error
  mod_pars <- lapply(object, function(x)
    list(
      pars = coef(x), 
      se_pars = sqrt(diag(vcov(x))))
    )
  
# extracts the predicted K-function and standard errors
  exp_pred <- lapply(object, function(mod_i)
  {
    pred <- lin_comb %*% coef(mod_i)
    se_pred <- sqrt(diag(lin_comb %*% vcov(mod_i) %*% t(lin_comb)))
    return(list(pred=pred, se_pred=se_pred))
  })
  
  # t-distribution uses the difference between each
  ## bootstrap and the fitted, divides by the bootstrap standard error
  ## to get a "t-distribution" that is then multiplied by the observed
  ## standard error and added to the fitted values.
  
  
  t_pars <- sapply(bootobj, function(sim, est) {
    do.call("cbind",
            mapply(
              function(sim, est) {
                t_r <- (sim$pars - est$pars) / (sim$se_pars)
                return(t_r)
              },
              sim = sim,
              est = est,
              SIMPLIFY = FALSE
            ))
  }, est = mod_pars, simplify = FALSE)

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

  t_pred <- lapply(bootobj, function(sim, obs){
    do.call('cbind',  mapply(function(obs.t, sim.t){
      t.score.t <- (sim.t$pred - obs.t$pred)/sim.t$se_pred
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
  attr(ci_boot, "bootobj") <- bootobj
  attr(ci_boot, "model") <- object
  class(ci_boot) <- "klmci"
  return(ci_boot)

}



