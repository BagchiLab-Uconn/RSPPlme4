
################################################################################
# Function that samples transformed level 1 residuals with replacement
## to supply bootstrap replicates
# is called by estimator.bootstrap.distribution
# is called by test.bootstrap.distribution.lin
###############################################################################
#' Samples Transformed Level 1 Residuals with Replacement
#'
#' @param mods list of lmer models, usually from \code{\link{klmerHyper}}
#' @param resids
#'
#' @return
#' @export
#'
#' @examples
residualRandomise.klmerHyper <- function(mods, resids)
{
  ## extract the level 1 residuals
  level1.resid <- lapply(resids, function(x) x[['level1resids']])

  ## mods is a list of models, which also contain some extra info.
  ##level1.resid is list of homogenised residuals from models for each distance
  N <- max(sapply(level1.resid, length)) ## extract sample size

  indx <- sample(1:N, replace=T) ## make sample for bootstrap to be replicated
  # at all distances

  level1.resid.r <- lapply(level1.resid, function(x) return(x[indx])) ## resample

  ## reapply the inhomogeneities to the data
  Cmat <- sapply(resids, function(x) attr(x, 'zmat'), simplify=FALSE)

  level1.resid.raw.r <- mapply(function(mod, res, Clarge) {
    if(is.null(mod))
      return(NULL)
    else
    {
      ## extract variance covariate
      wts <-   getCovariate(mod$modelStruct$varStruct)[order(order(getGroups(mod)))]
      transform1 <- t(chol( diag(sqrt(wts)) %*% Clarge %*% diag(sqrt(wts)) ))
      level1.res.raw.r <- as.vector(transform1 %*% res) ## apply back transform
      return(level1.res.raw.r)
    }
  }, mod=mods, res=level1.resid.r, Clarge=Cmat, SIMPLIFY=F)

  ## randomise the random effects
  ## first get sample for each level
  samp <- lapply(mods, function(mod){
    if(is.null(mod))
      return(NULL)
    else
    {
      re <- ranef(mod)
      return(lapply(re, function(rj){
        sample(1:NROW(rj), replace=T)}))
    }})
  ## At this point I can think of no good reason why the length of the
  ## vector would differ between distances
  ## this would be possible if some plots were a lot smaller perhaps, but not sure
  ## about the logic of including plots where the maximum distance analysed is
  ## larger than possible with a plot. For now therefore, just taking the first
  ## set of randomisations, but this might need to change in future.
  samp <- samp[[1]]

  ranef.r <- mapply(function(mod, res, samp){
    if(is.null(mod))
      return(NULL)

    else
    {
      ## extract the homogenised random effects
      ranef.res <- res[-which(names(res)=='level1resids')] ## remove level 1
      ranef.res.r <- mapply(function(r, ord){
        rnew <- as.matrix(r[ord,])  ## sample ranefs according to the index
        rownames(rnew) <- rownames(r) ## return to original names.
        return(rnew)
      }, r=ranef.res, ord=samp, SIMPLIFY=FALSE )

      ## assign values to replicates
      ranef.res.new <- mapply(function(j, r, mod){
        g <- as.character(getGroups(mod, level=j))  ## pull out the group
        # assignments
        if(is.null(attr(r, 'rownames')))
          rownames(r) <- rownames(ranef(mod, level=j)) ## if null, replace with
        # original ones
        return(r[g,])  ## extract ranefs for corresponding group (which may have
        # been switched.
      }, j=as.list(1:length(ranef.res)), r=ranef.res.r,
      MoreArgs=list(mod=mod),
      SIMPLIFY=FALSE)
    }
  }, mod=mods, res=resids, MoreArgs=list(samp=samp), SIMPLIFY=FALSE)

  ## put things together to be returned.
  resids <- mapply(function(level1, ranef) {
    list(level1.resid.raw.r=level1, ranef.r=ranef)
  }, level1=level1.resid.raw.r, ranef=ranef.r, SIMPLIFY=F)
  return(resids)
}
