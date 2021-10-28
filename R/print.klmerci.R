#' Print klmer confidence interval objects
#'
#' @param x A klmer object.
#' @param type Print parameters, (pars, the default), or predictions (predictions)
#' @param ... Additional arguments to print. Not implemented yet.
#'
#' @return No return value.
#' @export

print.klmerci <- function(x, type="pars", ...)
{
  
  alpha <- 1 - attr(x, "level")
  dimnames(x$pars)[[3]][2:3] <- 
    paste0(
      dimnames(x$pars)[[3]][2:3], 
      100 * c(alpha/2,    1 - alpha/2),  "%"
    )
  
  dimnames(x$preds)[[3]][2:3] <- 
    paste0(
      dimnames(x$pars)[[3]][2:3], 
      100 * c(alpha/2,    1 - alpha/2),  "%"
    )
  
  switch(type,
         pars = print(aperm(x$pars_fixed, c(3, 2, 1))),
         predictions = print(aperm(x$preds, c(1, 3, 2))),
         stop(paste(type, "is not a valid selection.",
                    "\nChoose from 'pars' or 'predictions'"))
  )
}
