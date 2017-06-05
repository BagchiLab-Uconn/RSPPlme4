
#' Randomise Homogenised Level 1 Residuals
#'
#' @param resids_e = Homogenised level 1 residuals from
#' \code{\link{residHomogenise}}.
#' @param mods klmer models.
#'
#' @return A list of randomised level 1 residuals.
#' @export

residRandomise <- function(resids_e, mods){

  N <- max(sapply(resids_e, length)) ## extract sample size

  indx <- sample(1:N, replace=T) ## make sample for bootstrap to be replicated
  # at all distances

  resids_r <- lapply(resids_e, function(x) return(x[indx])) ## resample

  ## reapply the inhomogeneities to the data
  resid_raw_r <- mapply(function(mod, res) {
    if(is.null(mod))
      return(NULL)
    else
    {
      ## extract variance covariate
      wts <-   weights(mod)
      res_raw_r <- res/sqrt(wts) ## apply back transform
      return(res_raw_r)
    }
  }, mod=mods, res=resids_r, SIMPLIFY=F)

  return(resid_raw_r)
}
