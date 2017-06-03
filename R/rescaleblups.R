#' Rescale BLUPs to be exchangable.
#'
#' @param U Random effects.
#' @param Sigma The estimated variance-covariance matrix.
#' @param forcePD Force all matrices to be positive definite.
#' @return A list of corrected BLUPs
#' @export


rescaleblups <- function(U, Sigma, forcePD=TRUE)
{
  mapply(function(U, Sigma){

    warn <- NULL
    ##if(any(Sigma==0))
    if(any(!is.finite(suppressWarnings(log(Sigma)))))
    {
      warn <- c(warn, 1)
      Ucorr <- U ## just return the original blups, which will all be 0s
      attr(Ucorr, 'warncode') <- warn
       return(Ucorr)
    }
        else
        {
          ## correct for mean
          U <- as.matrix(U)
          U <- apply(U, 2, function(x) x-mean(x)) ## centre blups

          ##  calcuate empirical variance-covarance
          S <- (t(U) %*% U)/(NROW(U))

          ## LOWER cholesky decomposition of Sigma
          if(!all(eigen(Sigma)$values>0) | Matrix::det(Sigma) <= 0){
            warn <- c(warn, 2)
            if(forcePD)
            {
              ##warning("replacing with nearest PD matrix")
              Sigma <- Matrix::nearPD(Sigma)$mat
              Sigma <- as.matrix(Sigma)
            }
            else
              stop("variance-covariance matrix not positive definite:\n
               Set correctPD=TRUE")
          }

          Lsig <- t(chol(Sigma))## now do cholesky decomposition

          if(!all(eigen(S)$values>0) | Matrix::det(S) <= 0){
            warn <- c(warn, 3)
            if(forcePD)
            {
              S <- Matrix::nearPD(S)$mat
              S <- as.matrix(S)
            }
            else
              stop("Empirical variance-covariance matrix not positive definite:\n
           Set correctPD to TRUE")
          }
          Ls <- t(chol(S))
          ## Correction factor
          A <-   t(Lsig %*% solve(Ls))

          Ucorr <- U %*% A
          attr(Ucorr, 'warncode') <- warn
          return(Ucorr)
        }
  }, U= U, Sigma=Sigma, SIMPLIFY=FALSE)
}
