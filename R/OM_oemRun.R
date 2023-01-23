# ---
# title: 'Wrapper function for observation error model'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Wrapper function for stock observation methods
#' 
#' Function calls stock observation method for each stock/management plan.
#'
#' @param om The operating model representing the true state and dynamics of 
#'           the system. A named list comprising stock information (\code{FLStocks}) 
#'           and fishing fleet information (\code{FLFisheries}).
#' @param deviances Environmental noise in stock catch and survey data for the 
#'                  historical and projection periods. A nested named list 
#'                  with the elements \code{stk} and \code{idx}. See details.
#' @param observations The structure of stock catch and survey data used to
#'                     estimate stock status.
#'                     A nested named list with the elements \code{stk} 
#'                     and \code{idx}.
#' @param args List of additional arguments
#' @param tracking A named list of tracking objects to monitor emergent dynamic
#'                 properties
#' @param catch_timing A named list of catch timing relative to assessment year
#' @param idx_timing A named list of survey index timing relative to assessment year
#' @param use_stk_oem Logical vector. Length \code{n} stocks. Should a supplied
#'                    Observation Error Model structure be used?
#' @param use_catch_residuals Logical vector. Length \code{n} stocks. Should
#'                            catch residuals be used to generate uncertainty?
#' @param use_idx_residuals Logical vector. Length \code{n} stocks. Should
#'                          survey index residuals be used to generate uncertainty?
#' @param use_om_weights Logical vector. Length \code{n} stocks. Should stock
#'                       catch, landings and discard individual mean weights be 
#'                       updated with data from OM? Recommend \code{TRUE} if 
#'                       attempting perfect stock observations.
#'
#' @return A named list of stock observations, survey indices and updated tracking
#'         object
#'
#' @export

oemRun <- function(om,
                   deviances,
                   observations,
                   args,
                   tracking,
                   catch_timing = NULL, # catch timing relative to ay
                   idx_timing   = NULL, # index timing relative to ay
                   use_stk_oem         = FALSE,
                   use_catch_residuals = FALSE,
                   use_idx_residuals   = FALSE,
                   use_om_weights      = FALSE) {
  
  # SHOULD I BE CLIPPING THE OBSERVATION TIMESERIES TO THE MOST RECENT DATA YEAR?
  
  # ---------------------------------#
  # Error checking
  # ---------------------------------#
  
  ## Check that correct structures supplied
  if(!all(names(observations) %in% c("stk","flt","idx")))
    stop("In 'oemMixME': 'observations' must be a named list. Names may be 'stk', 'flt' or 'idx'")
  if(!all(names(deviances)    %in% c("stk","flt","idx")))
    stop("In 'oemMixME': 'Deviances' must be a named list. Names may be 'stk', 'flt' or 'idx'")
  
  ## Check that the correct object classes are supplied
  if(!(class(observations$stk) %in% c("FLStocks","FLBiols")))
    stop("In 'oemMixME': observations 'stk' must be of class FLStocks or FLBiols")
  
  if(!is.null(observations$flt)) {
    if(class(observations$flt) != "FLFisheries")
      stop("In 'oemMixME': observations 'flt' must be of class FLFisheries")
  }
  
  if(class(observations$stk) == "FLBiols" & is.null(observations$flt)) {
    stop("In 'oemMixME': If observations 'stk' is of class FLBiols, 'flt' cannot be NULL")
  }
  
  ## Check that options are logical
  if(any(use_catch_residuals == TRUE) & is.null(deviances$stk)) {
    stop("In 'oemMixME': 'use_catch_residuals' is TRUE but no catch deviances supplied")
  }
  if(any(use_idx_residuals == TRUE) & is.null(deviances$idx)) {
    stop("In 'oemMixME': 'use_idx_residuals' is TRUE but no index deviances supplied")
  }
  
  # -----------------------------------#
  # Process global arguments
  # -----------------------------------#
  
  ## Process logical arguments if required
  if(length(use_stk_oem) == 1 & is.logical(use_stk_oem)) {
    use_stk_oem <- sapply(observations$stk@names, 
                          function(x) use_stk_oem,
                          USE.NAMES = TRUE)
  }
  if(length(use_catch_residuals) == 1 & is.logical(use_catch_residuals)) {
    use_catch_residuals <- sapply(observations$stk@names, 
                                  function(x) use_catch_residuals,
                                  USE.NAMES = TRUE)
  }
  if(length(use_idx_residuals) == 1 & is.logical(use_idx_residuals)) {
    use_idx_residuals <- sapply(observations$stk@names, 
                                function(x) use_idx_residuals,
                                USE.NAMES = TRUE)
  }
  if(length(use_om_weights) == 1 & is.logical(use_om_weights)) {
    use_om_weights <- sapply(observations$stk@names, 
                             function(x) use_om_weights,
                             USE.NAMES = TRUE)
  }
  
  ## Process catch and index timings
  if(is.null(catch_timing)) {
    catch_timing <- sapply(observations$stk@names, function(x) 0,
                           USE.NAMES = TRUE, simplify = FALSE)
  }
  
  if(is.null(idx_timing)) {
    idx_timing <- sapply(observations$stk@names, function(x) 0,
                         USE.NAMES = TRUE, simplify = FALSE)
  }
  
  # -----------------------------------#
  # Run OEM
  # -----------------------------------#
  
  ## Loop over each observed stock
  oemList <- lapply(observations$stk@names, 
                    oemMixME,
                    om           = om,
                    deviances    = deviances,
                    observations = observations,
                    args         = args,
                    tracking     = tracking,
                    catch_timing = catch_timing, # catch timing relative to ay
                    idx_timing   = idx_timing,   # index timing relative to ay
                    use_stk_oem  = use_stk_oem,
                    use_catch_residuals = use_catch_residuals,
                    use_idx_residuals   = use_idx_residuals,
                    use_om_weights      = use_om_weights)
  
  # -------------------------------------#
  # Return objects
  # -------------------------------------#
  
  ## Add names to list
  names(oemList) <- observations$stk@names
  
  ## Extract list elements
  for(x in observations$stk@names) {
    tracking[[x]]$stk <- oemList[[x]]$tracking
  }
  stk <- FLStocks(lapply(oemList, "[[", "stk"))
  idx <- lapply(oemList, "[[", "idx")
  
  
  ### return observations
  return(list(stk          = stk,
              idx          = idx,
              observations = observations,
              tracking     = tracking))
}