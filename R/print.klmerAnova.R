
#' Print a klmerAnova Object
#'
#' @param x An object of class \code{\link{anova.klmerHyper}}
#' @param stat Statistic to print. Options are T (default) or D.
#' @param ... Additional arguments, currently ignored.
#'
#' @return Prints to screen.
#' @export

print.klmerAnova <- function(x, stat=T, ...){
  print(paste(x$term, '( nsim = ', x$nsim, ')'))

  sapply(x[[stat]], function(obj)
    {
    cat("\n")
    cat("Distances =", paste(obj$dists, collapse=" "), "\n")
    print(obj$stat)
  })

  cat("")
}

