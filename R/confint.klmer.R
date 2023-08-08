

#' Calculate Bootstrap Confidence Intervals on a klmer object.
#'
#' @param object A \code{\link{klmer}} object.
#' @param parm Parameter to get confidence interval for. Currently ignored.
#' @param level Desired confidence intervals.
#' @param nboot Number of bootstrap simulations.
#' @param newdata New data to make predictions on. Defaults to NULL whereupon
#' fitted values are returned.
#' @param lin_comb Linear combination of the fixed parameters
#' @param bootobj Bootstrap object run outside function. Defaults to NULL so
#' the bootstrap is run internally.
#' @param ncore Number of cpus to use.
#' @param cltype Type of cluster to use.
#' @param iseed Random number seed.
#' @param ... Additional arguments, currently ignored.
#'
#' @return Returns the confidence intervals on an klmer object.
#'
#' @description The function uses a semi-parameteric bootstrapping approach 
#' to compute confidence intervals on parameter estimates and predictions from 
#' based on a klmer model object. 
#' 
#' Currently, random effects are ignored in the computations of predcitions -
#' i.e. predictions are made for an "average" group but assuming that any
#' group effects are known (i.e., no uncertainty is added from not knowing
#' which group the prediction is for). 
#' 
#' @export
#'
confint.klmer <-
  function(object,
           parm = NULL,
           level = 0.95,
           nboot = NULL,
           newdata = NULL,
           lin_comb = NULL,
           bootobj = NULL,
           ncore = 1,
           cltype = "PSOCK",
           iseed = NULL,
           ...)
  {
    
    # get model matrix from model if no custom matrix supplied
    
    if (is.null(lin_comb))
    {
      if (is.null(newdata))
        lin_comb <- lme4::getME(object[[1]], "X")
      else
        lin_comb <-
          model.matrix(update(formula(object[[1]], fixed.only = TRUE),
                              NULL~.), data = newdata)
    }
    
    if (is.null(bootobj)) 
      ## If bootstraps haven't been run yet, run now.
    {
      if (is.null(nboot))
        stop(
          paste(
            "Must define nboot if no bootobj passed to confint.",
            "\nConsider defining parameters for parallel processing:
                 ncore and cltype.\n"
          )
        )
      else
      {
        if (nboot < 1 / (1 - level))
          warning(
            paste(
              nboot,
              "samples is unlikely sufficient for",
              100 * level,
              "% confidence intervals"
            )
          )
        
        bootobj <- bootstrap.klmer(
          object,
          lin_comb = lin_comb,
          nboot = nboot,
          ncore = ncore,
          cltype = cltype,
          iseed = iseed
        )
      }
    }
    else 
    {
      if(class(bootobj) == "klmerci")
      {
        ## extract the required parts of the klmerci object
        bootobj <- lapply(attr(bootobj, "bootobj"), function(sim, lin_comb){
          mods_r <- attr(sim, "bootmod")
          pars_r <- sapply(mods_r, getPars, lin_comb =lin_comb,
                           simplify=FALSE)
          attr(pars_r, "bootmod") <- mods_r
          return(pars_r)
        }, lin_comb = lin_comb)
      }
      else
        stop(paste("Require a klmerci object to calculate confidence
                   intervals. \n
                   Estimate confidence intervals using the confint function"))
    }
    
    alpha <-  1 - level ## simplify calculations by taking 1 - alpha.
    
    ## Extract parameters from the models.
    mod_pars <- sapply(object, getPars, lin_comb = lin_comb,
                       simplify = FALSE)
    
    t_fixed <- sapply(bootobj, function(sim, est) {
      do.call("cbind",
              mapply(
                function(sim, est) {
                  t_r <- (sim$beta_r - est$beta_r) / sqrt(diag(sim$vcov_r))
                  est$beta_r - t_r * sqrt(diag(est$vcov_r))
                },
                sim = sim,
                est = est,
                SIMPLIFY = FALSE
              ))
    }, est = mod_pars, simplify = FALSE)
    
    ## pull out the cis of the parameter estimates
    cis_fixed <-
      apply(
        do.call(abind::"abind", args = list(what = t_fixed, along = 3)),
        c(2, 1),
        quantile,
        c(alpha / 2, 1 - alpha / 2),
        na.rm = T
      )
    
    ## Extract and organise fixed effect parameter estimates
    est_fixed <-
      do.call("cbind", lapply(mod_pars, function(x)
        x$beta_r))
    
    pars_fixed <- abind::abind(
      "est" = est_fixed,
      "lwr"  = t(cis_fixed[1, ,]),
      "upr" = t(cis_fixed[2 , ,]),
      along = 3
    )
    
    ##construct the 't-distributions' for the predictions
    t_pred <- lapply(bootobj, function(sim, obs)
    {
      as.matrix(mapply(function(sim, obs) {
        t_r <- (sim$pred_r - obs$pred_r) / sim$se_pred_r
        return(t_r)
      }, sim = sim, obs = obs))
    }, obs = mod_pars)
    
    ## turn these into a matrix
    t_pred <- do.call(abind::"abind", args = list(what = t_pred, along = 3))
    uci_pred <-
      apply(t_pred, c(2, 1), quantile, alpha / 2, na.rm = T) ## upper CI
    lci_pred <-
      apply(t_pred, c(2, 1), quantile, 1 - alpha / 2, na.rm = T) ##  lower CI
    
    est_pred <- t(do.call(abind::"abind",
                          args = list(
                            what = lapply(mod_pars, function(x)
                              x$pred_r),
                            along = 2
                          )))
    se_pred <- t(do.call(abind::"abind",
                         args = list(
                           what = lapply(mod_pars, function(x)
                             x$se_pred_r),
                           along = 2
                         )))
    ucl_pred <- est_pred - uci_pred * se_pred
    lcl_pred <- est_pred - lci_pred * se_pred
    
    preds <- abind::abind(
      est = est_pred,
      upr = ucl_pred,
      lwr = lcl_pred,
      along = 3
    )
    preds[preds < 0] <- 0
    ## organise into return object
    ci_boot <- list(predictions = preds, pars_fixed = pars_fixed)
    attr(ci_boot, "level") <- level
    attr(ci_boot, "bootobj") <- bootobj
    attr(ci_boot, "model") <- object
    class(ci_boot) <- "klmerci"
    return(ci_boot)
  }
