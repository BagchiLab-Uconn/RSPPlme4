#' Rescale BLUPs to be exchangable.
#'
#' @param U Random effects.
#' @param Sigma The estimated variance-covariance matrix.
#' @param forcePD Force all matrices to be positive definite.
#' @return A list of corrected BLUPs
#' @export


rescaleblups <- function(U, Sigma, forcePD=TRUE)
  mapply(function(U, Sigma){
  ## correct for mean
  U <- as.matrix(U)
  U <- apply(U, 2, function(x) x-mean(x)) ## centre blups

  ##  calcuate empirical variance-covarance
  S <- (t(U) %*% U)/(NROW(U))

  ## LOWER cholesky decomposition of Sigma
  if(!all(eigen(Sigma)$values>0)){
    warning("variance-covariance matrix not positive definite")
        if(forcePD)
        {
          warning("replacing with nearest PD matrix")
          Sigma <- Matrix::nearPD(Sigma)$mat
        }
    else
      stop("Set correctPD=TRUE to ensure VarCorr is positive definite")
  }
  Lsig <- t(chol(Sigma))## now do cholesky decomposition

  if(!all(eigen(S)$values>0)){
    warning("Empirical variance-covariance matrix not positive definite")
    if(forcePD)
    {
      warning("replacing with nearest PD matrix")
      S <- Matrix::nearPD(S)$mat
    }
    else
      stop("Need to ensure VarCorr is positive definite.\n
           You can set correctPD to TRUE")
  }
  Ls <- t(chol(S))
  ## Correction factor
  A <-   t(Lsig %*% solve(Ls))

  Ucorr <- U %*% A
  return(Ucorr)
}, U= U, Sigma=Sigma, SIMPLIFY=FALSE)
