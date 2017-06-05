#' Print klmer confidence interval objects
#'
#' @param x A klmerHyper object.
#' @param type Print parameters, (pars, the default), or predictions (preds)
#' @param ... Additoinal arguments to print. Not implemented yet.
#'
#' @return No return value.
#' @export

print.klmerci <- function(x, type="pars", ...)
{
  switch(type,
         pars = print(aperm(x$pars_fixed, c(2, 1, 3))),
         preds = print(aperm(x$preds, c(2, 1, 3))),
         stop(paste(type, "is not a valid selection.",
                    "\nChoose from 'pars' or 'preds'"))
  )
}
