#' Bootstrap on klmer models to obtain confidence intervals.
#' @import parallel

#' @param mods A \code{\link{klmer}} object.
#' @param lin_comb Linear combintaion of the fixed parameters
#' @param nboot Number of bootstrap simulations.
#' @param maxit Number of attempts to try fitting a bootstrap iteration before
#' giving up. Defaults to 10, which should be enough for most applications.
#' @param ncore Number of cpus to use.
#' @param cltype Type of cluster to use.
#' @param iseed Random number seed.
#'
#' @return A bootstrap sample of predictions, their standard errors,
#' fixed effects and variance covariance matrix for all distances.
#' @export

bootstrap.klmer <- function(mods, lin_comb, nboot, maxit =10,
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
  #
  #   clusterExport(cl, varlist=c("mods", "resids", "lin_comb", "maxit"),
  #                 envir=environment())
  
  ##load package on all remote cores
  clusterEvalQ(cl, library(RSPPlme4))
  
  pars <- parSapply(cl, 1:nboot, function(i, mods, resids, lin_comb, maxit)
    
  {
    
    do_again <- maxit ## to repeat if model doesn't converge
    
    while(do_again > 0)
    {
      ## Simulate new K function.
      K_r <- simulate(mods, resids=resids)
      ## refit the models
      mods_r <- refit.klmer(mods=mods, newK=K_r)
      
      ## If any errors repeat iteration, until maxit iterations (then give up)
      
      do_again <- (do_again - 1) *
        (any(sapply(mods_r, inherits, "try-error")) |
           any(is.na(sapply(mods_r, fixef))))
      
      if(do_again > 0)
        message('repeating sample')
    }
    
    ## set models that did not converge to NULL.
    mods_r[sapply(mods_r, inherits, "try-error")] <- NULL
    ## pull out the parameters from the bootstrapped model
    pars_r <- sapply(mods_r, getPars, lin_comb =lin_comb,
                     simplify=FALSE)
    attr(pars_r, "bootmod") <- mods_r
    return(pars_r)
  },  mods=mods, resids=resids, lin_comb=lin_comb, maxit=maxit,
  simplify=FALSE)
  
  attr(pars, "linear.combination") <- lin_comb
  return(pars)
}
