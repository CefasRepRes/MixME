# ---
# title: 'Operating Model projction methods'
# ---
#
# Summary
# =======
#
#' Forward projection of stocks - including process error
#'
#' This function takes the output of the implementation error model and
#' projects the stocks and fleets forward one time step.
#'
#' @param stks Object of class \code{FLBiols}
#' @param flts Object of class \code{FLFisheries}
#' @param ctrl Control object.
#'
#' @return A list containing
#'
#' @export

fwd_MIME <- function(stks,                     # FLStocks
                     flts,                     # FLFisheries
                     ctrl,                     # Control object
                     sr,                       # stock recruitment model
                     sr.residuals,             # recruitment residuals
                     sr.residuals.mult = TRUE, # are res multiplicative?
                     maxF = 2,                 # maximum allowed Fbar
                     proc_res = NULL,          # where is process error noise stored?
                     ...) {

  if(adviceType == "f") {


  } else if(adviceType == "TAC") {


  } else {
    stop("Advice format (adviceType) must be 'f' or 'TAC'")
  }

  ## Convert objects to FLBiols (~50 seconds for 1000 iterations)
  biols <- FLBiols(lapply(1:length(stkfltAll$stks),
                          function(x) as(stkfltAll$stks[[x]],"FLBiol")))

  biols$cod@rec@params <- sr_list[[1]]@params
  biols$cod@rec@model  <- sr_list[[1]]@model

  biols$had@rec@params <- sr_list[[2]]@params
  biols$had@rec@model  <- sr_list[[2]]@model

  biols$whg@rec@params <- sr_list[[3]]@params
  biols$whg@rec@model  <- sr_list[[3]]@model

  ## Project forward using FLasher::fwd

  ## Error: Trying to access element outside of quant, year, unit, season or area dim range.

  test <-FLasher::fwd(object = biols,
                      fishery = stkfltAll$flts,
                      control = ctrl)

  ## Add process error noise if available

  ## return()

}
