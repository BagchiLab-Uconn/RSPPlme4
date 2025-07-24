#' Compare Models with and without a Parameter of Interest.
#'
#' @param object An object of class \code{\link{klmer}}
#' @param term The parameter of interest.
#' @param dists Distances at which to test the effect of term. Can be a list with
#' several distance ranges specified.
#' @param nboot Number of bootstrap interations to compute the null distribution.
#' @param ncore Number of cpus to use.
#' @param maxit Maximum number of samples to try before giving up on refitting
#' a model at a given distance.
#' @param cltype The cluster type, see \code{\link[parallel]{makeCluster}}
#' @param iseed The random number seed to use for repeatability.
#' @param ... Additional arguments, currently not implemented.
#'
#' @return A list of class klmerAnova.
#'
#' @export
#'

anova.klmer <- function (object, term = NULL, dists, nboot,  maxit=50,
                     ncore=1, cltype='PSOCK', iseed=NULL, ...)
{
  if(is.null(term))
    stop("No term defined")

  ## manage distances if several ranges are to be tested
  testdists <-  dists
  if(!inherits(testdists, "list"))
    testdists <- list(testdists)

  if(inherits(dists, "list")){
    dists <- unique(do.call('c', dists))
    dists <- dists[order(dists)]
  }

  if(!all(as.character(dists) %in% names(object)))
    stop("Some test distances have not been modelled")

  atts <- attributes(object)
  object <- object[as.character(dists)]
  attr(object, "class") <- atts$class
  attr(object, "call") <- atts$class

  modsH1 <- refitMLklmer(object)
  modsH0 <- update(modsH1, term=term)

  ## remove models that didn't converge
  badmods <-  mapply(function(m0, m1) is.null(m0) | is.null(m1),
                     m0=modsH0, m1=modsH1)
  if(any(badmods))
  {
    warning(paste('removed model for distances:', dists[badmods]))

    modsH0 <-  modsH0[!badmods]
    modsH1 <- modsH1[!badmods]
  }

  obs_stat <- modCompare(modsH1=modsH1, modsH0=modsH0)

  cl <- makeCluster(ncore, type = cltype)
  RNGkind("L'Ecuyer-CMRG")
  clusterSetRNGStream(cl = cl, iseed = iseed)

  on.exit({
    stopCluster(cl)
    print("clusters closed on exit")
  })

  clusterEvalQ(cl, library(RSPPlme4))

  ## Extract residuals and BLUPs and make them exchangable.
  resids <- residHomogenise(object)


  ## Estimate the null distribution - first refit models to random data
  boot_stat <- parSapply(cl, 1:ceiling(round(nboot*1.1)), function(i, modsH0, modsH1,
                                                        resids, maxit)
  {
    modCompareBoot(modsH0 = modsH0, modsH1 = modsH1, resids = resids,
                   maxit = maxit)

  },modsH0, modsH1 = modsH1, resids = resids, maxit = maxit,
  simplify=FALSE)

  goodsims <- sapply(boot_stat, function(x) all(is.numeric(x)))

  if (sum(goodsims) < nboot)
    warning(paste("Only ", sum(goodsims),
                  "completed simulations - increase iterations?"))

  boot_stat <- boot_stat[goodsims][1:nboot]

  Dstats <- lapply(testdists,  Dcalc,
                   obsD=obs_stat, bootD=boot_stat, rmmods=badmods)

  Tstats <- lapply(testdists, Tcalc,
                   obsD=obs_stat, bootD=boot_stat, rmmods=badmods)

  result <- list(T=Tstats, D=Dstats, term = term, nsim=nboot,
                 sims=boot_stat)
  class(result) <-  'klmerAnova'
  return(result)
}
