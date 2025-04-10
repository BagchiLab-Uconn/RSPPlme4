
#' function to fit mixed effects model to k functions at a set distance
#'
#'@import lme4
#'
#' @param formula A one-sided formula describing the model to be fit, following
#' @param k A list of Kfunctions.
#' @param data The covariates.
#' @param weights The weights vector, probably generated by KfuncWeightsCalc.
#' the lme4 syntax. See \code{\link[lme4]{lmer}}
#' @param na.action How to deal with missing values.
#' See \code{\link[stats]{na.action}}
#' 
#' @return An object of class kfunclmer.
#'
#' @export
#'


klmer_i <- function(formula, k, data, weights, na.action="na.omit")
{

  ## also need to make sure these weights have a mean of 1
  weights <- weights/mean(weights)

  ###require(nlme) ## load required packages
  k_dataframe <- data.frame(k=k, data, weights = weights) # make data frame


  ## add K to lhs of formula
  formula <-  stats::update(formula, k~.)

  ## Fit model using 'try' to avoid problems of non-convergence at some
  ## distances
  lmer_ki <- try(lmer(formula, k_dataframe, weights=weights,
         na.action=na.action), silent=TRUE)

  if(inherits(lmer_ki, "try-error"))
    lmer_ki <- NULL
  return(lmer_ki)
}
