% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_conditioning_multiSAM2FLR.R
\name{multiSAMcay}
\alias{multiSAMcay}
\title{\subsection{Calculate predicted catches from sampled SAM parameters}{
}}
\usage{
multiSAMcay(SAMfit, variates, niter, option = 2)
}
\description{
Function takes a fitted SAM object and previously sampled parameter variates
and calculates the corresponding predicted catches.
}
\details{
There are two ways of doing this. Either updated the objective function with
the sampled parameter values and extract the predicted catches from TMB, or
directly calculate predicted catches from sampled parameters given inputted
estimates of natural mortality.

The advantage of the latter methods is that catch predictions are extended
into the aggregated data period.
}
