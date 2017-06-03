
#' Print a klmerAnova Object
#'
#' @param obj An object of class \code{\link{anova.klmerHyper}}
#'
#' @return Prints to screen.
#' @export

print.klmerAnova <- function(obj, stat=T){
  print(paste(obj$term, '( nsim = ', obj$nsim, ')'))

  sapply(obj[[stat]], function(x)
    {
    cat("\n")
    cat("Distances =", paste(x$dists, collapse=" "), "\n")
    print(x$stat)
  })

  cat("")
}

