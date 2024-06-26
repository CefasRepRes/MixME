% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_conditioning_calculateQuotashare.R
\name{calculateQuotashare}
\alias{calculateQuotashare}
\title{Estimate stock quota-share from historical landings-share}
\usage{
calculateQuotashare(stks, flts, verbose = TRUE, useCpp = TRUE)
}
\arguments{
\item{stks}{An object of class \code{FLStocks} or \code{FLBiols}.
This is a required input but is not currently used in the function.}

\item{flts}{An object of class \code{FLFisheries}.}

\item{verbose}{\code{Logical}. Should progress be printed to console? Defaults to \code{TRUE}.}

\item{useCpp}{\code{Logical}. Should a C++ helper function be used to speed up
attachment of outputs. Defaults to \code{TRUE} - THIS IS HIGHLY RECOMMENDED.}
}
\value{
A named \code{list} containing the updated stocks and fleets
}
\description{
This function calculate the historical share of stock landings (by weight) for
each fleet and uses this as a proxy for the historical share of stock quota.
These data are attached as an \code{FLQuant} attribute to each \code{FLCatch} in each
\code{FLFishery}.
}
\examples{
\donttest{
## load example data
data("singlestock_MixME_om")

## calculate quota-share from landings share
out <- calculateQuotashare(stks = singlestock_MixME_om$stks, 
                           flts = singlestock_MixME_om$flts, verbose = FALSE)
                           
## Overwrite with updated objects
singlestock_MixME_om$stks <- out$stks
singlestock_MixME_om$flts <- out$flts
}
}
