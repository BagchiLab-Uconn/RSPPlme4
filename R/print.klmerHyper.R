
#' Print KlmerHyper Objects
#'
#' @param x object to be printed
#' @param ... Additoinal arguments to print. Not implemented yet.
#'
#' @export
#'

print.klmerHyper <- function(x, ...){
  dists <- as.numeric(names(x))
  cat('linear mixed model fitted to k function \n')
  cat("distances modelled:\n", length(dists), 'distances\n',
      'range =', range(dists), '\n')
  cat('Fixed effects\n')
  print(do.call('cbind', sapply(x, fixef, simplify=FALSE)))
}

