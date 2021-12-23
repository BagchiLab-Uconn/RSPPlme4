#' Fits klmer at specified distances from a hyperframe with kfunctions and covariates.
#' @import lme4 spatstat stats
#'
#' @param formula Model formula following \code{\link[lme4]{lmer}} syntax, with
#' left hand side a K function object from \code{\link[spatstat.core]{Kest}} or 
#' a \code{link[spatstat.geom]{ppp}}
#' @param hyper \code{\link[spatstat.geom]{hyperframe}} with  k functions, weights,
#'  grouping factors and covariates
#' @param weights  name of user defined weights column.
#' @param weights_type The type of weights to be used if weights is not defined above. 
#' Must be a type listed in \code{\link[RSPPlme4]{kfuncWeightsCalc}}. Ignored if weights is defined.
#' @param r Distances at which to fit model
#' @param correction Edge correction. See \code{\link[spatstat.core]{Kest}}.
#' @param ppx A set of \code{\link[spatstat.geom]{ppp}} objects to use in calculating weights
#' @param minsamp Minimum number of points to include point pattern in model.
#' Not currently used.
#' @param remove_zero_weights Whether to remove point patterns with no focal points after 
#' edge corrections. If TRUE (the defaults) removes such rows with a warning.
#' @param na.action How to deal with missing data.
#' @param printwarnings Print warnings about distances with no variance?
#'
#' @return Model output of class klmer
#' @export
#'
klmer <- function(formula, hyper, weights = NULL,  weights_type = NULL, 
                  r, correction, ppx = NULL, 
                  minsamp=NA, remove_zero_weights = TRUE,
                  na.action="na.omit", printwarnings=TRUE)
{
  mc <- match.call()

  ppx <- deparse(substitute(ppx))
  if(ppx == "NULL") ppx <- NULL ## ugly but works
  
  weights <- deparse(substitute(weights))
  if(weights == "NULL") weights <- NULL ## ugly but works
  
  if(min(r) > 0)
    r <- c(0, r) ## Kest requires a 0 distance.

  rhs <- all.vars(update(formula, 0~.))
  missing <- !(rhs %in% names(hyper))
  if(any(missing))
    stop(paste("variable", rhs[missing], "not found\n"))
         
  lhs <- all.vars(update(formula, .~0))
  if(!(lhs %in% names(hyper)))
    stop(paste("Response variable", lhs,  "not found\n"))
  
  lhs_class <-  class(hyper[,lhs, drop = TRUE][[1]])
  
  if(!any(lhs_class %in% c("ppp", "fv")))
    stop("Response must be a K-function (class fv) or a spatial point 
         pattern (ppp) object")


  if("fv" %in% lhs_class)
    hyper$k <- hyper[ , all.vars(update(formula, .~0))]
  
  else
    if("ppp" %in% lhs_class)
    {
      if(!is.null(ppx))
        stop("ppx must be NULL if a ppp object is used as the response")
      else
        hyper$k <- lapply(hyper[, lhs, drop = TRUE], spatstat.core::Kest, 
                          correction = correction, r = r)
    } 
  
  if(is.null(weights))
  {
    if(is.null(weights_type) | (is.null(ppx) & !("ppp" %in% lhs_class)))
      stop("You must do one of: \n
           (1) provide weights \n
           (2) provide ppx and weights_type\n
           (3) set a ppp object as the response and provide weights_type")

    if(!is.null(ppx))
    {
      hyper[, "weights"] <- lapply(hyper[, ppx, drop = TRUE],
                              kfuncWeightsCalc,
                              r = r,
                              type = weights_type, 
                              correction = correction)
    }
    else
      if("ppp" %in% lhs_class)
        hyper[, "weights"] <- lapply(hyper[, lhs, drop = TRUE],
                                     kfuncWeightsCalc,
                                     r = r,
                                     type = weights_type, 
                                     correction = correction)
  }
  
  else
    #   #hyper$weights <- hyper[, deparse(substitute(weights)), drop = TRUE]
    hyper$weights <- hyper[, weights, drop = TRUE]
  
  
  zero_wts <- sapply(hyper$weights, function(x) any(x ==0))
  
  if(any(zero_wts))
    if(!remove_zero_weights)
      stop(paste("Rows ", 
                 paste(which(zero_wts), collapse = ", "), 
                 "have zero weights")
      )
  else
  {
    hyper <- hyper[-which(zero_wts),]
    warning(paste("Removed rows ", 
                  paste(which(zero_wts), collapse = ", "), 
                  "which have zero weights"))
  }

      ## Do not model distances where the variance is 0
  dist.keep <-  apply(sapply(hyper$k, function(K) K[[correction]][K$r %in% r]), 1,
                       function(x) !(var(x) == 0 | is.na(var(x))))
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

    mod <- klmer_i(formula=formula, k=k_i, data=as.data.frame(hyper, warn=FALSE),
                               weights=weights_i, na.action=na.action)
    return(mod)
  }, simplify=FALSE)

  names(kmods) <- r[dist.keep]

  class(kmods) <- 'klmer'
  attr(kmods, "call") <- mc
  attr(kmods, "removed_rows") <- which(zero_wts)
  return(kmods)
}
