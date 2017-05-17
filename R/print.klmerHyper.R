## method to print klmerHyper objects
print.klmerHyper <- function(x, ...){
  dists <- as.numeric(names(x))
  cat('linear mixed model fitted to k function \n')
  cat("distances modelled:\n", length(dists), 'distances\n',
      'range =', range(dists), '\n')
  cat('Fixed effects\n')
  print(rbind(dists, sapply(x, fixef)))
}

