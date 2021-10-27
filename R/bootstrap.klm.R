#' bootstrapping function for klm models to obtain confidence intervals.

#' @param mods A \code{\link{klm}} object.
#' @param lin_comb Linear combination of the fixed parameters

#' @return A bootstrap sample of parameters, predictions, their standard errors,
#' fixed effects and variance covariance matrix for all distances.


bootstrap.klm <- function(mods, lin_comb)
{
  resids <-
    lapply(mods, residHomogenise.klm) ## extract exchangable residuals
  
  samp <-
    sample(1:max(sapply(resids, length)), replace = T) ## set up sample to be
  
  
  # repeated at all distances
  ## extract the bootstraped K function for 1 iteration
  preds.all <- mapply(
    function(mod, resids, lin_comb,  indx) {
      k_dataframe <- as.data.frame(mod$model)
      mod <- eval(getCall(mod))
      
      if (length(resids[indx]) == length(mod$weights)) {
        newK <- fitted(mod) +   resids[indx] / (weights(mod) ^ 0.5)
        
        k_dataframe$Kr <- newK
        if (!any(is.na(newK))) {
          k_dataframe$weights <- mod$weights
          
          modnew <- update(mod,
                           formula = Kr ~ .,
                           weights = weights,
                           data = k_dataframe)
          pars <- coef(modnew)
          
          se_pars <- sqrt(diag(vcov(modnew)))
          pred <- lin_comb %*% coef(modnew)
          se_pred  <-
            sqrt(diag(lin_comb %*% vcov(modnew) %*% t(lin_comb)))
        }
      }
      
      else {
        pars <- rep(NA, length(coef(mod)))
        se_pars <- rep(NA, length(coef(mod)))
        
        pred <- rep(NA, length(resids))
        se_pred  <- rep(NA, length(resids))
      }
      return(list(
        pred = pred,
        se_pred = se_pred,
        pars = pars,
        se_pars = pars
      ))
    },
    mod = mods,
    resids = resids,
    MoreArgs = list(indx = samp, lin_comb = lin_comb),
    SIMPLIFY = FALSE
  )
  return(preds.all)
}
