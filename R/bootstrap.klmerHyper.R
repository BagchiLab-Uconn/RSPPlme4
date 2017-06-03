
#' Bootstrap on klmer models to obtain confidence intervals.
#' @import parallel

#' @param mods A \code{\link{klmerHyper}} object.
#' @param lin_comb linear combintaion of the fixed parameters
#' @param nboot number of bootstrap simulations.
#' @param ncore Number of cpus to use.
#' @param cltype Type of cluster to use.
#' @param iseed Random number seed.
#'
#' @return A bootstrap sample of predictions, their standard errors,
#' fixed effects and variance covariance matrix for all distances.
#' @export

bootParallel.klmerHyper <- function(mods, lin_comb, nboot,
                                   ncore=1, cltype='PSOCK', iseed=NULL)
{

  ## Make the residuals and BLUPs exchangable.
  resids <- residHomogenise(mods)

  cl <- makeCluster(ncore, type=cltype) ## make connections
  RNGkind("L'Ecuyer-CMRG")
  clusterSetRNGStream(cl = cl, iseed = iseed)

  on.exit({stopCluster(cl); message('clusters closed on exit')}) ## close connectons on
  # exit of function
  ## export all the functions and objects to the clusters
  clusterExport(cl, list('mods', 'resids', 'lin_comb'), envir=environment())
  ##load package on all remote cores
  clusterEvalQ(cl, {
    library(RSPPlme4)
  })

  pars <- parSapply(cl, 1:nboot, function(i, mods, resids, lin_comb)
  {

    do_again <- TRUE ## to repeat if model doesn't converge

    while(do_again)
    {
      ## Simulate new K function.
      K_r <- simulate(mods, resids)
      ## refit the models
      mods_r <- refit(mods, K_r)

      ## set do.again to TRUE if the model didn't converge
      do_again <- any(sapply(mods_r, function(x)
      {
        if(is.null(x))
          return(FALSE)

        else if(class(x)=='try-error')
          return(TRUE)

        else
          return(any(is.na(lme4::fixef(x))))}))

      if(do_again)
        message('repeating sample')
    }
    ## pull out the parameters from the bootstrapped model
    pars_r <- sapply(mods_r, getPars, lin_comb =lin_comb,
                   simplify=FALSE)
    return(pars_r)
  },  mods=mods, resids=resids, lin_comb=lin_comb,
  simplify=FALSE)

  attr(pars, "linear.combination") <- lin_comb
  return(pars)
}
