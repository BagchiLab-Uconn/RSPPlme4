#' Print klm confidence interval objects
#'
#' @param x A klmci object.
#' @param type Print parameters, (pars, the default), or predictions (predictions)
#' @param ... Additional arguments to print. Not implemented yet.
#'
#' @return No return value.
#' @export

print.klmci <- function(x, type="pars", ...)
{
  switch(type,
         pars = print(aperm(x$pars, c(3, 2, 1))),
         predictions = print(aperm(x$predictions, c(3, 2, 1))),
         stop(paste(type, "is not a valid selection.",
                    "\nChoose from 'pars' or 'predictions'"))
  )
}
