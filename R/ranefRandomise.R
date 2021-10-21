#' Randomise BLUPs After Correcting for Shrinkage.
#'
#' @param mods A list of models returned by \code{\link{klmer}}
#' @param ranefs A list of random effects corresponding to mods.
#'
#' @return A list of resampled random effects.

ranefRandomise <- function(mods, ranefs)
  {
  ## first get sample for each level
  samp <- lapply(mods, function(mod)
    {
    if(is.null(mod))
      return(NULL)
    else
    {
      re <- ranef(mod)
      return(lapply(re, function(rj){
        sample(1:NROW(rj), replace=T)}))
    }
  })
  ## I can't think of a good reason why the length of the vector would differ
  ## between distances. For now taking the first
  ## set of randomisations, but this might need to change in future.
  samp <- samp[[1]]

  ranef_r <- mapply(function(mod, res, samp)
  {
    if(is.null(mod))
      return(NULL)

    else
    {
      ## extract the homogenised random effects
      ranef_res_r <- mapply(function(r, ord)
      {
        rnew <- as.matrix(r[ord,])  ## sample ranefs according to the index
        rownames(rnew) <- rownames(r) ## return to original names.
        return(rnew)
      }, r=res, ord=samp, SIMPLIFY=FALSE)

      ## assign values to replicates
      ranef_res_new <- mapply(function(g, r, ranef_orig, mod)
      {
        g <- as.character(g)  ## pull out the group
        # assignments
        if(is.null(attr(r, 'rownames')))
          rownames(r) <- rownames(ranef_orig) ## if null, replace with
        # original ones
        return(r[g,])  ## extract ranefs for corresponding group (which may have
        # been switched.
      }, g=getME(mod, "flist"), r=ranef_res_r, ranef_orig=ranef(mod),
      MoreArgs=list(mod=mod),
      SIMPLIFY=FALSE)
    }
    return(ranef_res_new)
  }, mod=mods, res=ranefs, MoreArgs=list(samp=samp), SIMPLIFY=FALSE)
  return(ranef_r)
}
