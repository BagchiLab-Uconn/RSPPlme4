#' Constructs the weights argument from a few simple rules.
#' @param pppx A point pattern.
#' @param pppy A second point pattern for bivariate analyses.
#' @param r A vector of distances to evaluate K at.
#' @param type A string designating the types of weights to be calculated.
#' @param correction The edge correction to use - refer to
#' \code{\link[spatstat]{Kest}} for options.
#' @return A vector of weights for the K function with length equal to that of
#' r.
#' @examples
#' x <- spatstat::rpoint(20)
#' y <- spatstat::rpoint(20)
#' ratioWeightsCalc(x, y, r=seq(0:0.25, 0.05), correction = "border")
#' @family RSPP weight calculations
#' @details Uses the functions abundanceWeightsCalc and ratioWeightsCalc
#' to do the actual calculations.

kfuncWeightsCalc <- function(pppx, pppy=NULL, r=NULL, correction=NULL,
                               type=c('nx', 'nx_A', 'nx2', 'nx2_A',
                                      'sqrtnxny', 'nxny', 'nxny_A', 'sqrtnxny_A')) {
  if(is.null(r))
    stop('you must specify a distance range')

  switch(type,
         nx = abundanceWeightsCalc(pppx=pppx, r=r, correction=correction),
         nx_A = abundanceWeightsCalc(pppx=pppx, Acorr=TRUE,
                                       r=r, correction=correction),
         nx2 = abundanceWeightsCalc(pppx=pppx, square=TRUE,
                                      r=r, correction=correction),
         nx2_A = ratioWeightsCalc(pppx=pppx, pppy=NULL,
                                    r=r, correction=correction),
         sqrtnxny = sqrt(abundanceWeightsCalc(pppx=pppx, pppy=pppy,
                                                square=TRUE,  r=r, correction=correction)),
         nxny = abundanceWeightsCalc(pppx=pppx, pppy=pppy, square=TRUE,
                                       r=r, correction=correction),
         nxny_A=ratioWeightsCalc(pppx=pppx, pppy=pppy,
                                   r=r, correction=correction),
         sqrtnxny_A=sqrt(ratioWeightsCalc(pppx=pppx, pppy=pppy,
                                            r=r, correction=correction)),
         stop(paste(type, 'is not a valid selection.',
                    "\nChoose from 'nx', 'nx_A','nx2', 'nx2_A','sqrtnxny', 'nxny', 'nxny_A', 'sqrtnxny_A'"))
  )

}
