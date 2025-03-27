#' Function to Convert klmer Objects from REML to ML
#'
#' @import lme4
#'
#' @param mods model to be converted
#'
#' @return A klmer object with models fitted via ML
#'
#' @export
#'

refitMLklmer <- function(mods)
{
  mods <- lapply(mods, function(mod) {
    if (!is.null(mod)) {
      mod <- try(refitML(mod),
                 silent = TRUE)
      if (inherits(mod, "try-error")) {
        warning("ML full models  did not converge at some distances")
        mod <- NULL
      }
    }
    else mod <- NULL
    return(mod)
  })
  class(mods) <- 'klmer'
  return(mods)
}
