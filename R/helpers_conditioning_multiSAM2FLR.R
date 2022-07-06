# ---
# title: 'Functions to help condition Operating Models'
# author: 'Matthew Pace'
# date: 'July 2022'
# ---
#
#' Convert SAM fitted object into \code{FLStock} and either
#' \code{FLFisheries} or \code{FLFleets} objects
#'
#' This function takes a SAM fitted stock assessment object as input and
#' returns a list of \code{FLStocks} and either \code{FLFisheries}
#' or \code{FLFleets} objects.
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
#' @param fltClass Class structure to be used for fleets, defaults to
#'                 \code{FLFishery}. Either \code{FLFishery} or \code{FLFleet}
#' @param stkname Stock name to be used in  \code{FLFishery} or \code{FLFleet}
#'                object. Ignored if object of class \code{sam_list} is supplied.
#' @param useSAMcatch Optional argument. If \code{TRUE}, the fitted catches estimated
#'                    by SAM are used. Otherwise, the observed catches are used.
#'                    Defaults to \code{TRUE}.

#'
#'
#' @return A list containing an \code{FLStock} and \code{FLFleets} objects
#'
#' @section  Warning:
#' This function requires \code{FLCore} and either \code{FLFishery} or
#' \code{FLFleet} to operate.
#'
#' @export


setGeneric("multiSAM2FLR", function(SAMfit,
                                    fltClass = "FLFishery",
                                    stkname = NULL,
                                    useSAMcatch = TRUE,
                                    ...) {

  standardGeneric("multiSAM2FLR")
})

## If SAMfit == "sam"
#' @rdname multiSAM2FLR
setMethod(f = "multiSAM2FLR",
          signature = signature(SAMfit = "sam"),
          definition = function(SAMfit,
                                fltClass = "FLFishery",
                                stkname = NULL,
                                useSAMcatch = TRUE,
                                add = FALSE) {

            # Generate FLStock
            stk <- multiSAM2FLStock(SAMfit = SAMfit,
                                    useSAMcatch = useSAMcatch)

            # Coerce to FLStocks and define name
            stks <- FLCore::FLStocks(stk)
            names(stks) <- stkname

            if(fltClass == "FLFishery") {

              flts <- multiSAM2FLFishery(SAMfit = SAMfit,
                                        stkname = stkname,
                                        useSAMcatch = useSAMcatch,
                                        add = add)

            } else if(fltClass == "FLFleet") {

              flts <- multiSAM2FLFleet(SAMfit = SAMfit,
                                      stkname = stkname,
                                      useSAMcatch = useSAMcatch,
                                      add = add)

            } else {

              stop("Fleet structure (fltClass) must be 'FLFishery' or 'FLFleet'")
            }



            return(list(stks = stks,
                        flts = flts))


          })

## If SAMfit == "sam_list"
#' @rdname multiSAM2FLR
setMethod(f = "multiSAM2FLR",
          signature = signature(SAMfit = "sam_list"),
          definition = function(SAMfit,
                                fltClass = "FLFishery",
                                useSAMcatch = TRUE,
                                ...) {

            if(is.null(names(SAMfit)))
              stop("SAMfit must be a names list")

            ## How many stocks?
            nstks <- length(SAMfit)

            ## min and max years
            dim_minyear <- min(sapply(1:nstks, function(x) min(SAMfit[[x]]$data$years)))
            dim_maxyear <- max(sapply(1:nstks, function(x) max(SAMfit[[x]]$data$years)))

            ## Process into FLStocks
            stks <- FLStocks(lapply(1:nstks, function(x){

              stk <- multiSAM2FLStock(SAMfit = SAMfit[[x]],
                                      useSAMcatch = useSAMcatch,
                                      yearRange = c(dim_minyear, dim_maxyear))
              stk@name <- names(SAMfit)[x]

              return(stk)

            }))

            ## Define names for FLStocks
            names(stks) <- names(SAMfit)

            ## Process into FLFishery
            if(fltClass == "FLFishery") {

              ## Loop over each stock object
              for(i in 1:nstks) {

                ## If first stock - create new fleet, otherwise append
                ## to existing object
                if(i == 1) {
                  flts <- multiSAM2FLFishery(SAMfit = SAMfit[[i]],
                                             stkname = names(SAMfit)[i],
                                             useSAMcatch = useSAMcatch,
                                             add = FALSE,
                                             yearRange = c(dim_minyear, dim_maxyear))
                } else {
                  flts <- multiSAM2FLFishery(SAMfit = SAMfit[[i]],
                                             stkname = names(SAMfit)[i],
                                             useSAMcatch = useSAMcatch,
                                             add = TRUE,
                                             fleets = flts,
                                             yearRange = c(dim_minyear, dim_maxyear))
                }
              }


            } else if(fltClass == "FLFleet") {

              ## Loop over each stock object
              for(i in 1:nstks) {

                ## If first stock - create new fleet, otherwise append
                ## to existing object
                if(i == 1) {
                  flts <- multiSAM2FLFleet(SAMfit = SAMfit[[i]],
                                           stkname = names(SAMfit)[i],
                                           useSAMcatch = useSAMcatch,
                                           add = FALSE,
                                           yearRange = c(dim_minyear, dim_maxyear))
                } else {
                  flts <- multiSAM2FLFleet(SAMfit = SAMfit[[i]],
                                           stkname = names(SAMfit)[i],
                                           useSAMcatch = useSAMcatch,
                                           add = TRUE,
                                           fleets = flts,
                                           yearRange = c(dim_minyear, dim_maxyear))
                }
              }

            } else {

              stop("Fleet structure (fltClass) must be 'FLFishery' or 'FLFleet'")
            }

            return(list(stks = stks,
                        flts = flts))

          })
