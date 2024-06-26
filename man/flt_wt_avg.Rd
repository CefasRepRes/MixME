% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_conditioning_multiSAM2FLStock.R
\name{flt_wt_avg}
\alias{flt_wt_avg}
\title{\subsection{Summarise catch, landings, discards mean weights and landings fraction for multiple fleets}{
}}
\usage{
flt_wt_avg(ar, cn)
}
\description{
This function takes an array of mean weights or landings fraction
and a dataframe of catch numbers, each indexed by age, year and fleet.
Catch numbers are used to calculate a weighted mean for each age and year.
}
