#' Compares the Deviances of Two Models.
#'
#' @param modsH1 A more complex model of class \code{\link{klmer}}
#' @param modsH0 A simpler (null) model of class \code{\link{klmer}}
#'
#' @return The difference in deviances between the null and complex model.
#'
#' @export
#'

modCompare <- function(modsH1, modsH0)
{
  d_diff <-  mapply(function(mod, mod0) {
    if (is.null(mod) | is.null(mod0))
      return(NULL)
    else
      {
        devmod <- deviance(mod0) - deviance(mod)
        return(devmod)
      }
  }, mod = modsH1, mod0 = modsH0, SIMPLIFY=TRUE)
  return(d_diff)
}
