#' bootstrapping function for klm models to obtain confidence intervals.

#' @param mods A \code{\link{klm}} object.
#' @param lin_comb Linear combination of the fixed parameters
#' @return A bootstrap sample of predictions, their standard errors,
#' fixed effects and variance covariance matrix for all distances.
#' @export


bootstrap.klm <- function(mods, lincomb)
{
  resids <-
    lapply(mods, residHomogenise.klm) ## extract exchangable residuals

  samp <-
    sample(1:max(sapply(resids, length)), replace = T) ## set up sample to be
  
  
  # repeated at all distances
  ## extract the bootstraped K function for 1 iteration
  preds.all <- mapply(
    function(mod, resids, lincomb,  indx) {
      k_dataframe <- as.data.frame(mod$model)
      mod <- eval(getCall(mod))
      if (length(resids[indx]) == length(mod$weights)) {
        newK <- fitted(mod) +   resids[indx] / (weights(mod) ^ 0.5)
        
        k_dataframe$Kr <- newK
        if (!any(is.na(newK))) {
          modnew <- update(mod,
                           Kr ~ .,
                           weights = mod$weights,
                           data = k_dataframe)
          pars <- coef(modnew)
          se_pars <- sqrt(diag(vcov(modnew)))
          pred <- lincomb %*% coef(modnew)
          se_pred  <- sqrt(diag(lincomb %*% vcov(modnew) %*% t(lincomb)))
        }
      }
      
      else {
        pars <- rep(NA, length(coef(mod)))
        se_pars <- rep(NA, length(coef(mod)))            
                    
        pred <- rep(NA, length(resids))
        se_pred  <- rep(NA, length(resids))
      }
      return(list(pred = pred, se_pred = se_pred, 
                  pars = pars, se_pars = pars))
    },
    mod = mods,
    resids = resids,
    MoreArgs = list(indx = samp, lincomb = lincomb),
    SIMPLIFY = FALSE
  )
  return(preds.all)
}
