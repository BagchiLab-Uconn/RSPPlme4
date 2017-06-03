#' Updates klmerHyper Objects by Dropping Specified Term from Model
#'
#' @param mods klmerHyper object.
#'
#' @param term Term to be tested.
#'
#' @return Returns a klmerHyper object with the term of interest dropped.
#'
#' @export

update.klmerHyper <- function(mods, term)
{

  mods0 <- lapply(mods, function(mod, term, na.action) {

    mod0 <- NULL

    if (!is.null(mod)) {

      # mod0 <- update(mod, paste("~.-", term), data=model.frame(mod),
      #                    weights = weights(mod), REML=FALSE,
      #                    na.action=na.action)

      mod0 <- try(
        do.call("update", args=list(
          object = mod,
          formula = update(formula(mod), paste("~.-", term)),
          data = model.frame(mod), weights = weights(mod),
          REML = FALSE, na.action=na.action)),
        silent = TRUE)

      if (inherits(mod0, "try-error"))
      {
        warning("Null model fit did not converge at some distances")
##        mod0 <- NULL
      }
    }
    else
      mod0 <- NULL

    return(mod0)
  }, term = term, na.action=attr(mods, "call")$na.action)

  class(mods0) <- "klmerHyper"
  return(mods0)
}
