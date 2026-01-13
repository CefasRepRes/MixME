# ---
# title: 'Convert MixME operating model into FLStocks object'
# author: 'Matthew Pace'
# date: 'January 2026'
# ---
#
#' Convert \code{MixME} operating model into \code{FLFisheries} class object
#' 
#' Function converts a \code{MixME} operating model containing `stks` (`FLBiols`)
#' and `flts` (`FLFisheries`) into \code{FLStocks} by aggregating fleet data.
#' 
#' @param om A MixME om comprising a named lists of `stks` and `flts` with the
#'           classes `FLBiols` and `FLFisheries`, respectively.
#'           
#' @return object of class \code{FLStocks}
#' 
#' @export
#' @examples
#' ## load example data
#' data("mixedfishery_MixME_input")
#'
#' ## collapse Operating Model to FLStocks
#' res <- MixME2FLStocks(mixedfishery_MixME_input$om)

MixME2FLStocks <- function(om) {
  
  ## process each stock separately
  FLStocks(lapply(om$stks, function(x) {
    
    ## identify where corresponding catch occurs in fleet structure
    catch <- sapply(om$flts, function(y) which(names(y) %in% name(x)), 
                    simplify = FALSE, USE.NAMES = TRUE)
    
    ## find fleets that DO catch stock
    catch_idx <- which(sapply(catch, length) > 0)
    
    ## Coerce into FLStock
    xx <- as(x, "FLStock")
    
    ## summarise landings, discards and harvest
    xx@landings.n  <- MixME:::getC(om$flts, name(x), "landings.n", summarise = TRUE)
    xx@landings.wt <- MixME:::getC(om$flts, name(x), "landings.wt", summarise = TRUE)
    xx@discards.n  <- MixME:::getC(om$flts, name(x), "discards.n", summarise = TRUE)
    xx@discards.wt <- MixME:::getC(om$flts, name(x), "discards.wt", summarise = TRUE)
    xx@harvest     <- MixME:::getFage(om$stks, om$flts, name(x), use_fastF = T)
    
    discards(xx) <- computeLandings(xx)
    landings(xx) <- computeDiscards(xx)
    
    ## summarise catches
    catch.n(xx)  <- xx@landings.n + xx@discards.n
    catch.wt(xx) <- (xx@landings.n/xx@catch.n) * xx@landings.wt + (xx@discards.n/xx@catch.n) * xx@discards.wt
    catch(xx)    <- computeCatch(xx)
    
    ## return result
    return(xx)
  }))
}