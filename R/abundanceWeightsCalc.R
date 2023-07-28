#' A function to calculate the weights based on the abundance
#' of points only.

#' @param pppx A point pattern.
#' @param pppy A second point pattern for bivariate analyses.
#' @param square Scale weights to the number of pairs.
#' @param Acorr Correct for pattern area.
#' @param r A vector of distances to evaluate K at.
#' @param correction The edge correction to use. Refer to
#' \code{\link[spatstat.explore]{Kest}} for options.
#' Only \code{border} is currently implemented, although NULL will also work
#' when no edge correction is required (e.g. in plus-sampling designs).
#'
#' @return A vector of weights of length r.
#'

#' @family RSPP weight calculations

abundanceWeightsCalc <- function(pppx, pppy=NULL,
                                   square=FALSE, Acorr = FALSE,
                                   r=NULL, correction=NULL)
  {


  ## first need to restrict calculation of n to
  ## points < r away if border correction
  if(!is.null(correction)) ## if correction - implement the correction
  {
    if(correction=='border')
      pppx_c <- sapply(r, function(ri) pppx[spatstat.geom::erosion(pppx$window, ri)],
                       simplify=FALSE)
    else
      stop(paste("Correction for", correction,
                 "method not implemented yet"))
  }

  else ## for null corrections - i.e. no correction requested
      pppx_c <-  sapply(r, function(r) pppx, simplify=FALSE)

  wx <- sapply(pppx_c, spatstat.geom::npoints) ## number of points

  if(square)
  {
    if(!is.null(pppy))
      wx <- wx*spatstat.geom::npoints(pppy)
    else
      wx <- wx*spatstat.geom::npoints(pppx)
  }

  if(Acorr)
    if(!is.null(pppy))
      wx <- wx/spatstat.geom::area(pppy)
  else
    wx <- wx/spatstat.geom::area(pppx)

  return(wx)
}
