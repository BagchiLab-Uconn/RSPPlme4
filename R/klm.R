#' Fits klmer at specified distances from a hyperframe with kfunctions and covariates.
#' @import lme4 spatstat stats
#'
#' @param formula Model formula following \code{\link[stats]{lm}} syntax, with
#' left hand side a k-function object.
#' @param hyper \code{\link[spatstat.geom]{hyperframe}} with  k functions, weights,
#'  grouping factors and covariates
#' @param weights  List of weights for each point pattern.
#' @param r Distances at which to fit model
#' @param correction Edge correction. See \code{\link[spatstat.core]{Kest}}.
#' @param minsamp Minimum number of points to include point pattern in model.
#' Not currently used.
#' @param na.action How to deal with missing data.
#' @param printwarnings Print warnings about distances with no variance?
#'
#' @return Model output of class klmerHyper
#' @export
#'
#'
klm <- function(formula, hyper, weights, r, correction,
                       minsamp=NA, na.action="na.omit", printwarnings=TRUE)
{

  mc <- match.call()

  if(min(r) > 0)
    r <- c(0, r) ## Kest requires a 0 distance.

  hyper[["weights"]] <- hyper[, deparse(substitute(weights))]
  
  
  zero_wts <- sapply(hyper$weights, function(x) any(x == 0))
  if(any(zero_wts))
    stop(paste("Rows ", 
               paste(which(zero_wts), collapse = ", "), 
               " have zero weights")
         )
  ## set k as LHS of model formula
  hyper$k <- hyper[ , all.vars(update(formula, .~0))]

      ## Do not model distances where the variance is 0
  dist.keep <-  (apply(sapply(hyper$k, function(K) K[[correction]][K$r %in% r]), 1,
                       function(x) var(x)) > 0)
  if(printwarnings)
    if(any(!dist.keep[r!=0]))
      warning(paste('Not modelling K at distances ',
                    paste(r[!dist.keep], collapse=', '),
                    "due to zero variance"))
  modr <-  match(r[dist.keep], r )
  ## fit the models

  kmods <- sapply(modr, function(ri)
  {

    k_i <- sapply(hyper$k, function(k, ri)
    {
      k[[correction]][ri]
    }, ri=ri)

    weights_i <- sapply(hyper$weights, function(w, ri)
    {
      unlist(w)[ri]
    }, ri=ri)

    mod <- klm_i(formula=formula, k=k_i, data=as.data.frame(hyper, warn=FALSE),
                               weights=weights_i, na.action=na.action)
    return(mod)
  }, simplify=FALSE)

  names(kmods) <- r[dist.keep]

  class(kmods) <- 'klm'
  attr(kmods, "call") <- mc
  return(kmods)
}