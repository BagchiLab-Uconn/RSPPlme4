################################################################################
## Function repeats klmer at all distances
klmerHyper <- function(formula, hyper, r, correction,
                       minsamp=NA, na.action="na.omit", printwarnings=FALSE)
{
  if(min(r) > 0)
    r <- c(0, r) ## Kest requires a 0 distance.

   if(!all(c("k", "weights") %in% names(hyper)))
     stop("hyperframe object must include columns 'k' and 'weights'")

  ## Do not model distances where the variance is 0
  dist.keep <-  (apply(sapply(hyper$k, function(K) K[[correction]][K$r %in% r]), 1,
                       function(x) var(x)) > 0)
  if(printwarnings)
    if(any(!dist.keep[r!=-0]))
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

    klmer(formula=formula, k=k_i,
          data=as.data.frame(hyper, warn=FALSE),
          weights=weights_i, na.action="na.omit")
  }, simplify=FALSE)

    names(kmods) <- r[dist.keep]

  class(kmods) <- 'klmerHyper'
  return(kmods)
}
