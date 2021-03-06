% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.klmerHyper.R
\name{simulate.klmerHyper}
\alias{simulate.klmerHyper}
\title{Semi-Parametric Bootstrap sample form a klmerHyper object.}
\usage{
\method{simulate}{klmerHyper}(object, nsim = 1, seed = NULL, resids, ...)
}
\arguments{
\item{object}{Models to be simulated from.}

\item{nsim}{Number of simulations (ignored)}

\item{seed}{Seed for simulations (currently ignored)}

\item{resids}{The residuals to be resampled.}

\item{...}{Additional parameters for simulate method.}
}
\value{
A list of simulated Kfunctions for all distances in klmerHyper
object.
}
\description{
Semi-Parametric Bootstrap sample form a klmerHyper object.
}
