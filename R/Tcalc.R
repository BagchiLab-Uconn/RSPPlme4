Tcalc <- function(d, obsD, bootD, rmmods){
  d <- d[!(d %in% names(rmmods)[rmmods])]
  boot.mat <- do.call('rbind', bootD)
  boot.sd <- apply(boot.mat, 2, sd, na.rm=T)
  boot.mn <- apply(boot.mat, 2, mean, na.rm=T)
  obsD <- obsD/boot.mn
  T.obs <-mean(obsD[as.character(d)])
  bootD <- lapply(bootD[!sapply(bootD, is.null)],
                  function(D, mnD) D/mnD, mnD=boot.mn)
  T.boot <- sapply(bootD, function(x, d) {
    return(mean(x[as.character(d)]))
  }, d = d, simplify = TRUE)
  p.val <- (sum(T.obs < T.boot) + 1)/(1 + length(T.boot))
  T = c(T=T.obs, p=p.val)
  return(list(stat=T, T.boot=T.boot, dists=d))
}
