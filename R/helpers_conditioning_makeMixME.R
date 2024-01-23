# ---
# title: 'Functions to assemble MixME objects'
# author: 'Matthew Pace'
# date: 'August 2023'
# ---
#
#' Function to assemble inputs to a MixME simulation. Outputs a list with correct
#' arguments for the specified simulation. Object slots may require updating.
#' 
#' 
#' @export

makeMixME <- function(om,
                      catch_obs = NULL,
                      index_obs = NULL,
                      catch_dev = NULL,
                      index_dev = NULL,
                      management_lag  = 1,
                      management_type = "HCR",
                      parallel = FALSE) {
  
  # Catch errors
  # ============#
  
  if (missing(om))
    stop("om must be provided")
  
  if (management_lag < 0)
    stop("management_lag cannot be negative")
  
  if (!(management_type %in% c("HCR", "fixedC", "fixedF")))
    stop("management_type must be either 'HCR', 'fixedC' or 'fixedF'")
  
  if ((management_lag > 0) & (management_type == "fixedC"))
    warning("management_type is fixed catch but management lag > 0")
  
  if (any(names(om) != c("stks","flts")))
    stop("om must be a named list with elements 'stks' and 'flts'")
  
  if (!is.null(catch_obs)) {
    if (!is.list(catch_obs) | is.null(names(catch_obs)))
      stop("catch_obs must be a named list of stocks")
    
    if (!all(names(catch_obs) %in% names(om$stks)))
      stop("catch_obs names must match om stock names")
  }

  
  if(!is.null(catch_dev)) {
    if(!is.list(catch_dev) | is.null(names(catch_dev)))
      stop("catch_dev must be a named list of stocks")
    
    if (!all(names(catch_dev) %in% names(om$stks)))
      stop("catch_dev names must match om stock names")
  }
  
  if(!is.null(index_obs)) {
    if(!is.list(index_obs) | is.null(names(index_obs)))
      stop("index_obs must be a named list of stocks")
    
    if (!all(names(index_obs) %in% names(om$stks)))
      stop("index_obs names must match om stock names")
  }

  
  if(!is.null(index_dev)) {
    if(!is.list(index_dev) | is.null(names(index_dev)))
      stop("index_dev must be a named list of stocks")
    
    if (!all(names(index_dev) %in% names(om$stks)))
      stop("index_dev names must match om stock names")
  }

  
  # Process input arguments
  # =======================#
  
  printmessage <- "MixME simulation prepared. The following arguments may need updating: \n"

  ## extract simulation timings
  fy <- dims(om$stks[[1]])$maxyear
  y0 <- dims(om$stks[[1]])$minyear
  iy <- as.integer(dimnames(om$stks[[1]]@n)$year[tail(which(!is.na(apply(om$stks[[1]]@n, c(2), sum))),1)])
  
  ## process parallelisation
  if (parallel == TRUE) {
    nworkers <- parallel::detectCores()-1
    
    ## update print message
    printmessage <- paste0(printmessage," - parallelisation with 'args$nworkers': ",nworkers,"\n")
    
  } else {
    nworkers <- 1
  }
  
  ## Infer whether there is perfect stock observation from timing lags
  if(management_lag == 0) {
    use_om_weights = TRUE
    
    estmethod <- sapply(om$stks@names, 
                        function(x) "perfectObs",
                        USE.NAMES = TRUE, 
                        simplify = FALSE)
    isysmethod = sapply(om$stks@names, 
                        function(x) NULL, 
                        USE.NAMES = TRUE)
    
    ## combine into complete lists
    est  <- mseCtrl(args = list(estmethod = estmethod))
    isys <- mseCtrl(args = list(isysmethod = isysmethod))
    
  } else {
    use_om_weights = FALSE
    
    estmethod <- sapply(om$stks@names, 
                        function(x) MixME::SAMassessment,
                        USE.NAMES = TRUE, 
                        simplify = FALSE)
    
    fitList <- sapply(om$stks@names,
                      function(x) list(conf      = NULL,  # SAM configuration
                                       newtonsteps = 0,   # switch off newton steps
                                       rel.tol   = 0.001, # reduce relative tolerance
                                       par_ini   = NULL,  # initial parameters
                                       track_ini = TRUE), # store ini for next year
                      USE.NAMES = TRUE,
                      simplify = FALSE)
    
    fwdList <- sapply(om$stks@names,
                      function(x) list(forecast = TRUE,          # short-term forecast?
                                       fwd_trgt = c("fsq","fsq"),# what to target in forecast
                                       fwd_yrs = 2,              # number of years to add (I need SSB in ay+1)
                                       fwd_yrs_fsq = -2:0,       # years used to calculate fsq
                                       fwd_yrs_average = -2:0,   # years used for averages
                                       fwd_yrs_rec_start = dimnames(om$stks[[x]])$year[1], # start year for recruitment 
                                       fwd_yrs_sel = NULL,       # years used for selectivity
                                       fwd_yrs_lf_remove = NULL, # years to overwrite landings fraction with most recent data year
                                       fwd_splitLD = FALSE),     # separate landings and discards in forecast
                      USE.NAMES = TRUE,
                      simplify = FALSE)
    
    isysmethod <- sapply(om$stks@names, 
                         function(x) MixME::SAMimplementation,
                         USE.NAMES = TRUE, 
                         simplify = FALSE)
    
    isysList <- sapply(om$stks@names,
                       function(x) list(fwd_trgt = c("fsq", "hcr"), # what to target in forecast
                                        fwd_yrs = 2,                # number of years to add
                                        fwd_yrs_fsq = -2:0,         # years used to calculate fsq
                                        fwd_yrs_average = -2:0,     # years used for averages
                                        fwd_yrs_rec_start = dimnames(om$stks[[x]])$year[1],   # start year for recruitment 
                                        fwd_yrs_sel = NULL,         # years used for selectivity
                                        fwd_yrs_lf_remove = NULL,   # years to overwrite landings fraction with most recent data year
                                        fwd_splitLD = FALSE),       # separate landings and discards in forecast
                       USE.NAMES = TRUE,
                       simplify = FALSE)

    
    ## combine into complete lists
    est  <- mseCtrl(args = list(estmethod = estmethod,
                                fitList   = fitList,
                                fwdList   = fwdList))
    isys <- mseCtrl(args = list(isysmethod = isysmethod,
                                isysList   = isysList))
  }
  
  # Process management type
  # -----------------------#
  
  if (management_type == "HCR") {
    phcr <- mseCtrl(args = list(hcrpars = sapply(om$stks@names,
                                      function(x) c(Ftrgt = NA,  Btrigger = NA, Blim =  NA),
                                      USE.NAMES = TRUE,
                                      simplify = FALSE)))
    hcr <- mseCtrl(args = list(hcrmethod = sapply(om$stks@names, 
                                       function(x) "hcrICES", 
                                       USE.NAMES = TRUE, 
                                       simplify = FALSE)))
    ## update print message
    printmessage <- paste0(printmessage," - NA in 'ctrl_obj$phcr@args$hcrpars' \n")
    
  }
  if (management_type == "fixedC") {
    phcr <- NULL
    hcr <- mseCtrl(args = list(hcrmethod = sapply(om$stks@names, 
                                       function(x) "hcrFixedC", 
                                       USE.NAMES = TRUE, 
                                       simplify = FALSE),
                    ctrg      = sapply(om$stks@names,
                                       function(x) 1,
                                       USE.NAMES = TRUE,
                                       simplify = FALSE)))
    
    ## update print message
    printmessage <- paste0(printmessage," - Catch target in 'ctrl_obj$hcr@ctrg' is 1 for all stocks \n")
    
  }
  if (management_type == "fixedF") {
    phcr <- NULL
    hcr <- mseCtrl(args = list(hcrmethod = sapply(om$stks@names, 
                                       function(x) "hcrFixedF", 
                                       USE.NAMES = TRUE, 
                                       simplify = FALSE),
                    ftrg      = sapply(om$stks@names,
                                       function(x) 1,
                                       USE.NAMES = TRUE,
                                       simplify = FALSE)))
    
    ## update print message
    printmessage <- paste0(printmessage," - Fishing mortality target in 'ctrl_obj$hcr@ftrg' is 1 for all stocks \n")
  }
  
  # Process stock observations
  # --------------------------#
  
  ## Process stock observation and catch timing
  if (is.null(catch_obs)) {
    
    ## No stock observations
    use_stk_oem <- FALSE
    stk_obs <- NULL
    
    if(management_lag > 0) {
      
      ## If no stock observations assume - 1
      catch_timing <- sapply(om$stks@names, function(x) -1,
                             USE.NAMES = TRUE, simplify = FALSE)
      
      ## update print message
      printmessage <- paste0(printmessage," - 'oem@args$catch_timing' is -1 for all stocks \n")
      
    } else {
      catch_timing <- NULL
    }
  } else {
    
    ## Use weights and parameters from catch observations
    use_stk_oem <- TRUE
    
    ## process stock observations
    stk_obs <- FLStocks(catch_obs)
    
    ## Extract catch timing from catch observations
    catch_timing <- sapply(om$stks@names, 
                           function(x) {
                             cy <- as.integer(dimnames(catch_obs[[x]]@catch.n)$year[tail(which(!is.na(apply(catch_obs[[x]]@catch.n, c(2), sum))),1)])
                             return(cy - iy)
                           },
                           USE.NAMES = TRUE, 
                           simplify = FALSE)
  }
  
  if (is.null(catch_dev)) {
    stk_dev <- NULL
    use_catch_residuals <- FALSE
    
  } else {
    stk_dev <- catch_dev
    use_catch_residuals <- TRUE
  }
  
  # Process index observations
  # --------------------------#
  
  ## Process index observations and timing
  if (is.null(index_obs)) {
    idx_obs <- NULL
    idx_timing <- NULL
    
  } else {
    
    ## process index observations
    idx_obs <- index_obs
    
    ## Extract index timings from index observations
    idx_timing <- sapply(names(index_obs), function(x) 
      sapply(seq_along(index_obs[[x]]), 
             function(y) {
               cy <- as.integer(dimnames(index_obs[[x]][[y]]@index)$year[tail(which(!is.na(apply(index_obs[[x]][[y]]@index, c(2), sum))),1)])
               return(cy - iy)
             },
             USE.NAMES = FALSE, 
             simplify = TRUE),
      USE.NAMES = TRUE, 
      simplify = FALSE)
    
  }
  
  ## Process index deviates
  if (is.null(index_dev)) {
    idx_dev <- NULL
    use_idx_residuals <- FALSE
    
  } else {
    
    idx_dev <- index_dev
    use_idx_residuals <- TRUE
  }
  
  # Build MixME object
  # ===================#
  
  ## Make empty Observation Error Model structure
  oem <- mse::FLoem(observations = list(stk = stk_obs,
                                        idx = idx_obs),
                    deviances    = list(stk = stk_dev,
                                        idx = idx_dev),
                    args = list(use_stk_oem = use_stk_oem,
                                use_catch_residuals = use_catch_residuals,
                                use_idx_residuals   = use_idx_residuals,
                                use_om_weights      = use_om_weights,
                                catch_timing        = catch_timing,
                                idx_timing          = idx_timing))
  
  ## Make Management Procedure arguments
  ctrl_obj <- mpCtrl(list(est  = est,
                          phcr = phcr,
                          hcr  = hcr,
                          isys = isys,
                          fwd  = mseCtrl(args   = list(sr_residuals = NULL,
                                                       proc_res     = NULL))))
  
  ## Make simulation arguments
  args <- list(fy = fy, # final simulation year
               y0 = y0, # first data year
               iy = iy, # initial year
               management_lag = management_lag,  # management lag
               frq            = 1,             # advice period duration
               parallel = parallel,              # parallel processing?
               nworkers = nworkers,              # >1 if parallel processing
               frange = sapply(om$stks@names, 
                               function(x) c(NA, NA),
                               USE.NAMES = TRUE, 
                               simplify = FALSE),
               adviceType = "catch",
               adviceInit = sapply(om$stks@names, 
                                   function(x) matrix(NA, nrow = 1, ncol = dims(om$stks[[1]])$iter),
                                   USE.NAMES = TRUE, 
                                   simplify = FALSE), # The catch advice in the first projection year
               testfwd    = FALSE,  # FALSE == use c++ to update fleets
               maxRetry   = 10,     # number of times to attempt to identify choke-stocks
               useTMB     = TRUE,   # TRUE == use TMB to generate objective function
               seed       = NULL,   # seed for Random Number Generator
               verbose    = TRUE)   # TRUE == print simulation progress to console or file
  
  
  ## update print message
  printmessage <- paste0(printmessage," - age range for fbar in 'args$frange' is NA for all stocks \n")
  printmessage <- paste0(printmessage," - initial advice in 'args$adviceInit' is 1 for all stocks \n")
  message(printmessage)
  
  ## Return outputs
  return(list(om       = om,
              oem      = oem,
              ctrl_obj = ctrl_obj,
              args     = args))
}