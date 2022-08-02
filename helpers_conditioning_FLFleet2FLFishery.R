# ---
# title: 'Conditioning: FLFleets2FLFishery'
# author: 'Matthew Pace'
# date: 'July 2022'
# ---
#
#' Convert an \code{FLFleets} object into an \code{FLFisheries} object
#'
#' This function takes an \code{FLFleets} object, containing one or more
#' \code{FLMetiers} and \code{FLCatches} as input and
#' returns a \code{FLFisheries} object.
#'
#' Returned \code{FLFisheries} object contains data for one or more
#' \code{FLFishery} objects. Each \code{FLFishery} object contains the following
#' slots:
#' \itemize{
#' \item capacity (\code{catch.q})
#' \item effort (\code{landings.n})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item proportion catch retained-at-age (\code{landings.sel})
#' \item discards numbers-at-age (\code{discards.n})
#' \item discards mean weight-at-age (\code{discards.wt})}
#'
#' Additionally, each \code{FLFishery} object contains one or more \code{FLCatch}
#' object with the following slots:
#'
#' \itemize{
#' \item landings numbers-at-age (\code{landings.n})
#' \item landings mean weight-at-age (\code{landings.wt})
#' \item discards numbers-at-age (\code{discards.n})
#' \item discards mean weight-at-age (\code{discards.wt})
#' \item proportional selectivity-at-age (\code{catch.sel})
#' \item mean price-per-unit-weight-at-age (\code{price})}
#'
#' NOTE: To collapse \code{FLMetiers}, the metier effort-shares \code{...} are
#'       used to calculate a weighted mean for the catch properties of each 
#'       stock. Catch numbers are summed across metiers.
#'
#' @param flts Object of class \code{FLFLeets}
#'
#' @return An \code{FLFishery} object
#'
#' @section  Warning:
#' This function requires \code{FLCore} to operate.
#'
#' @export