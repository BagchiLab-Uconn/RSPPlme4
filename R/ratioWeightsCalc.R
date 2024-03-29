
#' Takes a Point Pattern and Calculates the Denominator of the K Function.
#'
#' @param pppx A point pattern.
#' @param pppy A second point pattern for bivariate analyses.
#' @param r A vector of distances to evaluate K at.
#' @param correction The edge correction to use - refer to Kest for options.
#'
#' @return A vector with the denominator of the K functions for each point
#' @import spatstat
#'
#' @export
#'
#' @examples

#' x <- spatstat.random::rpoint(20)
#' y <- spatstat.random::rpoint(20)
#' ratioWeightsCalc(x, y, r=seq(0, 0.2, 0.05), correction = "border")
#'
#' @family RSPP weight calculations

ratioWeightsCalc <- function(pppx, pppy=NULL, r=NULL, correction='border')
{
  if(is.null(correction))
    stop('you must define a correction for this weights argument')
  if(is.null(pppy))
    Kx <- spatstat.explore::Kest(pppx, r=r, correction=correction, ratio=TRUE)
  else
    Kx <- spatstat.explore::Kcross(spatstat.geom::superimpose(x=pppx, y=pppy, W=spatstat.geom::Window(pppy)),
                           r=r, correction=correction, ratio=TRUE)
  wts <-  attr(Kx, 'denominator')[[correction]]
  return(wts)
}


