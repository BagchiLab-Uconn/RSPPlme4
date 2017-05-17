# ## find point patterns that have a defined number of individuals individuals
# checkSample <- function(
# if(!is.na(minsamp))
#
# {
#   sp.keep <-sapply(
#     with.hyperframe(hyper,
#                     list(kfunc.weights.calc(pppx, r=K$r,
#                                             correction=correction,
#                                             type='nx'))),
#     function(x) all(unlist(x[r]) >= minsamp))
#
#   removed.species <- row.names(hyper)[!sp.keep]
#
#   if(printwarnings & length(removed.species) > 0)
#     warning(paste("Removed", length(removed.species),
#                   "species with insufficient numbers"))
#
#   ## select these species
#   hyper <- hyper[sp.keep,]
# }
# else
#   removed.species <- NULL
