################################################################################
## Function that homogenises the residuals from a linear mixed effects model
## to make them exchangable. Handles multiple levels of random effects
################################################################################
residual.homogenise.lme <- function(mod){
  if(is.null(mod))
    return(NULL)
  ## Extract grouping levels for each level
  grps <-  getME(mod, "flist")


  wts <-  getME(
  ## Modified until here.


  ## pull out the variance covariate (which is the inverse of the
  ## weights, but confusingly labelled wts - sorry!
  if(!is.null(mod$modelStruct$varStruct))
    wts <-   getCovariate(mod$modelStruct$varStruct)[order(order(getGroups(mod)))]
  ##    wts <- getData(mod)$wts

  else wts <- rep(1,mod$dims$N) ## if no weights, set them all as equal -
  # this shouldn't happen!
  ## Pull out the variances and co-variances of blups at all levels
  var.comps <- lapply(as.matrix(mod$modelStruct$reStruct), function(x, sig)
    x*sig^2,
    sig=mod$sigma)

  ## for the random effects
  Unew <- mapply(function(u, Sigma){
    ## correct for mean
    u <- as.matrix(u)
    u <- apply(u, 2, function(x) x-mean(x))
    ##  calcuate empirical variance-covarance
    S <- (t(u) %*% u)/(NROW(u))
    ## LOWER cholesky decomposition of Sigma
    Lsig <- t(chol(Sigma))
    Ls <- t(chol(S))
    A <-   t(Lsig %*% solve(Ls))
    unew <- u %*% A
    return(unew)
  }, u= ranef(mod), Sigma=var.comps, SIMPLIFY=FALSE)

  ## extract the variance-covariance of the residuals
  Clarge <- diag(mod$dims$N) ## if no correlation just an identity matrix
  ## populate with the correct values extracted from the model if there is
  ## a correlation
  if(!is.null(mod$modelStruct$corStruct)){
    for(i in levels(grps)) {
      Cj <- cov2cor(getVarCov(mod, individuals=i, type='conditional')[[1]])
      Clarge[grps==i, grps==i] <- Cj
    }
  }

  ## note that really we are using the inverse of the wts here,
  ## not the wts - this is because lme needs to be given
  ## a variance covariate which is the inverse of the weights.
  ## remove correlation in residuals
  transform <- solve(t(chol(diag(sqrt(wts)) %*% Clarge %*%
                              diag(sqrt(wts)))))
  resids <- as.vector(transform %*% resid(mod))
  resids <- resids - mean(resids) ## centre
  Ls1 <- sqrt(mean(resids^2)) ## empirical resid variance
  A1 <- mod$sigma/Ls1   ## ratio of modelled to observed
  residNew <- resids*A1  ## make empirical resid variance = to modelled variance
  result <- c(level1resids=list(residNew), Unew)
  attr(result, 'zmat') <- Clarge
  return(result)
}
