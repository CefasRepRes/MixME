% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MP_hcr.R
\name{hcrICES}
\alias{hcrICES}
\title{ICES Harvest Control Rule implementation}
\usage{
hcrICES(stk, args, hcrpars, tracking)
}
\arguments{
\item{stk}{Observed stocks. An object of class \code{FLStock}.}

\item{args}{Additional arguments.}

\item{hcrpars}{Harvest Control Rule reference points. An object of class \code{FLPar}.}

\item{tracking}{A named list of tracking objects to monitor emergent dynamic
properties}
}
\value{
A control object containing the adjusted F target for the stock.
}
\description{
Applies the ICES Harvest Control Rule to estimated stock properties for
each stock to generate single-stock advice.
}
\details{
For the ICES Harvest Control Rule, the reference points (parameters) that
are used are $B_{trigger}$, $F_{target}$, $B_{pa}$, $F_{pa}$ and $B_{lim}$.
}
\author{
Adapted from WKNSMSE (2019) code by S. H. Fischer.
}
