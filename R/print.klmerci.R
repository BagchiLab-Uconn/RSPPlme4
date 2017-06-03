#' Print klmer confidence interval objects
#'
#' @param obj A klmerHyper object.
#' @param type Print parameters, (pars, the default), or predictions (preds)
#'
#' @return No return value.
#' @export

print.klmerci <- function(obj, type="pars")
{
  switch(type,
         pars = print(aperm(obj$pars_fixed, c(2, 1, 3))),
         preds = print(aperm(obj$preds, c(2, 1, 3))),
         stop(paste(type, "is not a valid selection.",
                    "\nChoose from 'pars' or 'preds'"))
  )
}
