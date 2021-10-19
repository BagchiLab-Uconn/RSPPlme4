
#' Print klm Objects
#'
#' @param x object to be printed
#' @param ... Additional arguments to print. Not implemented yet.
#'
#' @export
#'

print.klm <- function(x, ...){
  dists <- as.numeric(names(x))
  cat('linear model fitted to k function \n')
  cat("distances modelled:\n", length(dists), 'distances\n',
      'range =', range(dists), '\n')
  cat('Coefficients\n')
  print(do.call('cbind', sapply(x, coef, simplify=FALSE)))
}

