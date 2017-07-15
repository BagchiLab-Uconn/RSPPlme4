Dcalc <- function(d, obsD, bootD, rmmods){
  d <- d[!(d %in% names(rmmods)[rmmods])]
  D.obs <-sum(obsD[as.character(d)])
  D.boot <- sapply(bootD[!sapply(bootD, is.null)],
                   function(x, d) {
                     return(sum(x[as.character(d)]))
                   }, d = d, simplify = TRUE)
  p.val <- (sum(D.obs < D.boot) + 1)/(1 + length(D.boot))
  D = c(D=D.obs, p=p.val)
  return(list(stat = D, D.boot=D.boot, dists=d))
}

