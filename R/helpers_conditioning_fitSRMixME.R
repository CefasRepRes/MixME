# ---
# title: 'Functions to fit stock-recruit models'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Wrapper function to fit stock-recruit models
#' 
#' @export

fitSRMixME <- function(om, 
                       rec_yrs_start = NULL, 
                       rec_model = "segreg",
                       iy,
                       ny,
                       uncertainty = FALSE,
                       verbose = TRUE) {
  
  # ==========================#
  # Process arguments
  # ==========================#
  
  ## extract 
  
  ## If null, find minimum years to start recruitment. 
  if(is.null(rec_yrs_start)) {
    rec_yrs_start <- sapply(om$stks, function(x) {
      
      ssb <- FLCore::ssb(x)
      ssb_idx <-which(apply(ssb, c(2), sum, na.rm = TRUE) > 0)
      
      rec <- FLCore::n(x)[1,]
      rec_idx <- which(apply(rec, c(2), sum, na.rm = TRUE) > 0)
      
      minyr <- max(min(dimnames(ssb)$year[ssb_idx]),
                   min(dimnames(rec)$year[rec_idx]))
      
      return(minyr)
    }, USE.NAMES = TRUE)
  }
  
  # ==========================#
  # Fit Stock-recruit models
  # ==========================#
  
  sr_list <- lapply(names(om$stks), function(x) {
    
    ## optional print stock names
    if(verbose) cat(x, "; ")
    
    # Assumption of 1 year recruitment lag -- not always true!!!
    
    ## extract rec and ssb data
    sr <- FLCore::FLSR(model = rec_model,
                       ssb = window(FLCore::ssb(om$stks[[x]]), 
                                    start = as.numeric(rec_yrs_start[x]),
                                    end   = iy+ny-2),
                       rec = window(FLCore::n(om$stks[[x]]),  
                                    start = as.numeric(rec_yrs_start[x])+1,
                                    end   = iy+ny-1)[1,]#, 
                       #range = as.numeric(range(dimnames(om$stks[[x]])$age))
                       )
    
    ## fit model to each iteration (suppress output to screen)
    suppressWarnings(. <- capture.output(sr <- fmle(sr)))
    
    ## return stock-recruit model
    return(sr)
  })
  names(sr_list) <- names(om$stks)
  
  # ==========================#
  # Process uncertainty
  # ==========================#
  
  if(uncertainty == TRUE) {
    stop("Recruitment uncertainty is not yet implemented")
  }
  
  if(uncertainty == FALSE) {
    ## In this first pass, we're not including uncertainty so just have zero deviation
    sr_list_res <- lapply(1:length(sr_list), function(x) {
      
      residuals(sr_list[[x]])[,ac((iy):((iy-1)+ny))] <- 0
      return(exp(residuals(sr_list[[x]])))
    })
  }
  
  # ==========================#
  # (Optional) Plotting
  # ==========================#
  
  ## plot recruitment deviation
  if(verbose) {
    plotlist <- lapply(1:length(sr_list_res), function(x) 
      ggplotFL::plot(sr_list_res[[x]]))
    
  } else {
    plotlist <- NULL
  }
  
  # ===============================#
  # Organise objects for returning
  # ===============================#
  
  if(class(om$stks) == "FLBiols") {
    for(i in names(om$stks)) {
      om$stks[[i]]@rec@params <- sr_list[[i]]@params
      om$stks[[i]]@rec@model  <- sr_list[[i]]@model
      om$stks[[i]]@rec$rec    <- NULL
    }
    
    return(list(om       = om,
                srResid  = sr_list_res,
                plotlist = plotlist))
  }
  
  if(class(om$stks) == "FLStocks") {
    
    return(list(om       = om,
                sr       = sr_list,
                srResid  = sr_list_res,
                plotlist = plotlist))
    
  }
}
