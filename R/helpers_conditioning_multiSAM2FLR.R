# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'July 2022'
# ---
#
#' Convert SAM fitted object into \code{FLStock} and \code{FLFleet} objects
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns a list of \code{FLStock} and \code{FLFleet} objects.
#'
#' Returned \code{FLStock} objects contain data for:
#' \itemize{
#' \item stock numbers-at-age (\code{stock.n})
#' \item catch numbers-at-age (\code{catch.n})
#' \item landings numbers-at-age (\code{landings.n})
#' \item discards numbers-at-age (\code{discards.n})
#' \item proportion mature-at-age (\code{mat})
#' \item natural mortality-at-age (\code{m})
#' \item fishing mortality-at-age (\code{harvest})
#' \item stock mean weight-at-age (\code{stock.wt})
#' \item catch mean weight-at-age (\code{catch.wt})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item discards mean weight-at-age (\code{discards.wt})}
#'  
#' Catch numbers and fishing mortality-at-age are summed across commercial fleets,
#' whereas catch, landings and discards mean weight-at-age are averages weighted
#' by the catch number proportions by commercial fleets. Landings and discards
#' numbers-at-age are calculated using the catch proportion weighted mean
#' landings fraction.
#'  
#' The function also updates the Fbar range for the stock from the fitted
#' SAM object.
#' 
#' Returned \code{FLFleet} objects contain data for:
#' \itemize{
#' \item partial fishing mortality-at-age (\code{harvest})
#' \item catch numbers-at-age (\code{catch.n})
#' \item landings numbers-at-age (\code{landings.n})
#' \item discards numbers-at-age (\code{discards.n})
#' \item catch mean weight-at-age (\code{catch.wt})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item discards mean weight-at-age (\code{discards.wt})}
#'
#' @param SAMfit A SAM fitted stock assessment model object or
#'               a named list of SAM fitted stock assessment model objects
#' @param useSAMcatch Optional argument. If \code{TRUE}, the fitted catches estimated
#'                    by SAM are used. Otherwise, the observed catches are used.
#'                    Defaults to \code{TRUE}.
#'
#' @return A list containing an \code{FLStock} and \code{FLFleets} objects
#'
#' @section  Warning:
#' This function requires \code{FLCore} to operate.
#'
#' @export

multiSAM2FLR