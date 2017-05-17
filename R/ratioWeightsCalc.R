#' takes a point patterns and calculates the denominator of
#' the K function.
#'
#' @param pppx A point pattern.
#' @param pppy A second point pattern for bivariate analyses.
#' @param r A vector of distances to evaluate K at.
#' @param correction The edge correction to use - refer to Kest for options.
#' @return A vector with the denominator of the K functions for each point
#' pattern.
#' @import spatstat
#' @examples
#' x <- spatstat::rpoint(20)
#' y <- spatstat::rpoint(20)
#' ratioWeightsCalc(x, y, r=seq(0, 0.2, 0.05), correction = "border")
#'
#' @family RSPP weight calculations

ratioWeightsCalc <- function(pppx, pppy=NULL, r=NULL, correction='border'){
  if(is.null(correction))
    stop('you must define a correction for this weights argument')
  if(is.null(pppy))
    Kx <- spatstat::Kest(pppx, r=r, correction=correction, ratio=TRUE)
  else
    Kx <- spatstat::Kcross(spatstat::superimpose(x=pppx, y=pppy, W=Window(pppy)),
                 r=r, correction=correction, ratio=TRUE)
  wts <-  attr(Kx, 'denominator')[[correction]]
  return(wts)}
