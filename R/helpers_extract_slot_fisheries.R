# ---
# title: 'Helper function to extract an FLFisheries slot as a dataframe'
# author: 'Paul Dolder'
# date: 'July 2025'
# ---
#
#' @title Extract FLFisheries
#'
#' @description \code{slot_fisheries} Function takes an FLFisheries object and a named slot to extract the slot as a dataframe.
#'
#' @param fisheries An FLFisheries object with historical (and optionally conditioned) parameter values.
#' @param slot. `character` vector of length=1. The name of the slot to extract as a dataframe.
#'
#' @return is a dataframe with outputs from the slot for all FLFisheries in the object.   
#'
#' @examples
#' \donttest{
#' data("mixedfishery_MixME_input")
#' fleets <- mixedfishery_MixME_input$om$flts
#' slot_fisheries(fisheries = fleets, slot. = "catch.q")
#' }
#'
#' @export 

setGeneric("slot_fisheries", function(fisheries, slot.){
  standardGeneric("slot_fisheries")
})

setMethod("slot_fisheries", signature(fisheries="FLFisheries"),
          function(fisheries,slot.)
          {
            sl. <- eval(parse("",text=slot.))
            res <- do.call(rbind, lapply(fisheries, function(x) {
              do.call(rbind, lapply(x, function(y) cbind(fishery = x@name, stock = y@name, as.data.frame(sl.(y)))))
            }))
            return(res)
          })