
#' Extract Useful Parameters from Model and a Linear Contrast Matrix.
#'
#' @param mod Model to extract parameters from.
#' @param lin_comb Linear combination.
#'
#' @return A list with predictions, their standard errors, fixed effects and
#' the variance covariance matrix.
#' @export
#'

getPars <- function(mod, lin_comb) {
  if(!is.null(mod)){
    beta_r <- lme4::fixef(mod)  ## fixed effecgts
    vcov_r <- as.matrix(vcov(mod)) ## variance covariance of fixed effects
    est_Kmean_r <- as.vector(lin_comb %*% beta_r) ## predicted values
    ## standard errors of the predicted vals
    est_Kse_r <- sqrt(diag(lin_comb %*% vcov_r %*% t(lin_comb)))

  }
  ## if model failed, then just make everything NA
  else{
    beta_r <- rep(NA, ncol(lin_comb))
    vcov_r <- matrix(NA, ncol=length(beta_r),
                     nrow=length(beta_r))
    est_Kmean_r <-  rep(NA, nrow(lin_comb))
    est_Kse_r <- rep(NA, nrow(lin_comb))
  }
  return(list(pred_r=est_Kmean_r, se_pred_r=est_Kse_r,
              beta_r=beta_r, vcov_r=vcov_r))
}
