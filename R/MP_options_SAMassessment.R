# ---
# title: 'Functions to implement SAM-based stock assessment'
# author: 'Various'
# date: 'December 2022'
# ---
# 
# Adapted from code by S.H. Fischer.
#
#' Estimate stock status used the SAM state-space assessment model
#' 
#' Stock status estimation using bridging functions from the \code{FLfse} to
#' fit a SAM state-space model. Time-series of commercial catch and
#' survey indices are used as observation data to which the model is fitted.
#' 
#' @param stk Object of class \code{FLStock} containing observed stock 
#'            information including commercial catch data, individual mean 
#'            weights and biological parameters.
#' @param idx Object of class \code{FLIndices} containing survey indices
#' @param tracking Tracking object
#' @param args Additional arguments
#' @param parallel Logical. Should model fitting be parallelised? Defaults to 
#'                 \code{FALSE}
#' @param conf List. SAM configuration
#' @param par_ini List. Initial parameter values for SAM
#' @param track_ini Logical. Should fitted parameters be stored for use as 
#'                  initial values next year? Defaults to \code{FALSE}
#' @param forecast Logical. Should a short-term forecast be carried out? Defaults
#'                 to \code{TRUE}
#' @param fwd_trgt Character. Catch or effort target to use during forecast. 
#'                 Defaults to 'fsq'.
#' @param fwd_yrs Integer. The number of years to forecast. Defaults to 1.
#' @param fwd_yrs_average Integer vector. The historical data years over which
#'                        biological parameter are averaged for use during 
#'                        forecast. Defaults to -3:-1.
#' @param fwd_yrs_rec_start Integer. Starting historical year from which to sample
#'                          projection period recruitment during forecast.
#' @param fwd_yrs_sel Integer vector. The historical data years over which
#'                    catch selection-at-age is averaged for use during forecast.
#'                    Defaults to -3:-1.
#' @param fwd_yrs_lf_remove Integer Vector. ... Defaults to -2:-1.
#' @param fwd_splitLD Logical. Defaults to \code{TRUE}
#' 
#' @return a named list containing the assessed stock and updated tracking 
#'         object.
#' 
#' @export

SAMassessment <- function(stk, idx, tracking,
                          args,                     # contains ay (assessment year)
                          parallel = FALSE,
                          conf = NULL,              # SAM configuration
                          par_ini = NULL,           # initial parameters
                          track_ini = FALSE,        # store ini for next year
                          forecast = FALSE,
                          newtonsteps = 0,          # switch off newton steps
                          rel.tol = 0.001,          # reduce relative tolerance
                          fwd_trgt = "fsq",         # what to target in forecast
                          fwd_yrs = 1,              # number of years to add
                          fwd_yrs_average = -3:-1,  # years used for averages
                          fwd_yrs_rec_start = NULL, # recruitment 
                          fwd_yrs_sel = -3:-1,      # selectivity
                          fwd_yrs_lf_remove = -2:-1,
                          fwd_splitLD = TRUE,
                          ...) {
  
  # --------------------------------------------#
  # 1. Set up arguments
  # 2. Fit SAM model
  # 3. (Optional) Carry out short-term forecast
  # 3.1 ... Prepare objects for forecast
  # 3.2 ... Carry out forecast
  # --------------------------------------------#
  
  # ===========================#
  # Some checks
  # ===========================#
  
  ## years used to average must be 0 or negative
  if(forecast == TRUE & any(fwd_yrs_average > 0)) 
    stop("'fwd_yrs_average' cannot have elements > 0")
  if(forecast == TRUE & any(fwd_yrs_sel > 0)) 
    stop("'fwd_yrs_sel' cannot have elements > 0")
  if(forecast == TRUE & any(fwd_yrs_lf_remove > 0)) 
    stop("'fwd_yrs_lf_remove' cannot have elements > 0")
  
  if(forecast == TRUE & fwd_yrs < 1)
    stop("The number of forecast years 'fwd_yrs' cannot be < 1")
  
  if(forecast == TRUE &(fwd_yrs > length(fwd_trgt)))
    stop("In 'fwd_trgt', a target must be defined for each forecast year")
  
  # ===========================#
  # SECTION 1: Set up arguments
  # ===========================#
  
  ## get additional arguments
  args <- c(args, list(...))
  
  ## get current (assessment) year
  ay <- args$ay
  
  # Check if initial parameter values for SAM exist from last year's fit
  # and reuse if possible. This overrides generic initial parameters, if they 
  # exist in par_ini (they are only used in first simulation year)
  
  if (isTRUE(track_ini) & !is.null(tracking$par_ini)) {
    par_ini <- tracking$par_ini
  }
  
  # ==========================#
  # SECTION 2: Fit SAM model
  # ==========================#
  
  ## fit SAM to provided data
  fit <- FLfse::FLR_SAM(stk = stk, 
                        idx = idx, 
                        conf        = conf, 
                        par_ini     = par_ini,
                        DoParallel  = parallel, 
                        newtonsteps = newtonsteps,
                        rel.tol     = rel.tol, 
                        ...)
  
  # store parameter values and provide them as initial values next year
  # store in tracking object, this is the only object which does not get 
  # over-ridden next year
  
  if (isTRUE(track_ini)) {
    
    ### check class
    if (is(fit, "sam") | is(fit, "list")) {
      fit <- list(fit)
      class(fit) <- "sam_list"
      
    } else if (is(fit, "sam_list")) {
      ### do nothing
    } else {
      stop("unknown fit object")
    }
    
    ### go through fits and extract parameters
    pars <- lapply(fit, function(fit_i) {
      tryCatch({
        p <- fit_i$pl
        p$missing <- NULL
        attr(p, "what") <- NULL
        return(p)
      }, error = function(e) NULL)
    })
    
    ### return list or params
    if (length(pars) == 1) {
      pars <- pars[[1]]
    }
    
    ## store par
    tracking$par_ini <- pars
    
  }
  
  ## convert into FLStock
  stk0 <- FLfse::SAM2FLStock(object = fit, stk = stk) 
  
  # ===================================================#
  # SECTION 3: (Optional) Carry out short-term forecast
  # ===================================================#
  
  # This section carries out an optional short-term forecast to get spawning
  # stock biomass in assessment year + 1.
  
  ## perform forecast to get SSB ay+1
  if (isTRUE(forecast)) {
    
    # ------------------------------------------#
    # SECTION 3.1: Prepare objects for forecast
    # ------------------------------------------#
    
    # check how to do forecast
    # currently, can only do F status quo
    if (!all(fwd_trgt %in% c("fsq","TAC"))) {
      stop("only fsq and TAC supported in forecast")
    }
    
    ## years for average values
    ave.years <- range(stk0)[["maxyear"]] + fwd_yrs_average
    
    ## years for sampling of recruitment years
    rec.years <- seq(from = fwd_yrs_rec_start, to = range(stk0)[["maxyear"]] - 1)
    
    ## years where selectivity is not used for mean in forecast
    overwriteSelYears <- range(stk0)[["maxyear"]] + fwd_yrs_sel
    
    ## last data year
    lst_yr <- range(stk0)[["maxyear"]]
    
    ## extend stk0
    stk0 <- window(stk0, end = range(stk0)[["maxyear"]] + fwd_yrs)
    
    # If catch data are not available in final year, extend forecast by one year
    # to estimate missing catch
    
    ## modify fwd_yrs if last data year does not include catch
    if (all(is.na(catch(stk0)[, ac(lst_yr)]))) fwd_yrs <- fwd_yrs + 1
    
    ## forecast years
    yrs <- seq(to = dims(stk0)$maxyear, length.out = fwd_yrs)
    
    ## coerce fit into list if only 1 iteration
    if (is(fit, "sam")) {
      fit <- list(fit)
      class(fit) <- "sam_list"
    }
    
    # NOTE: I think the following might cause issues if fwd_trgt length > 1
    
    ## template for forecast targets
    fscale <- ifelse(fwd_trgt == "fsq", 1, NA)
    catchval <- ifelse(fwd_trgt == "TAC", -1, NA)
    
    ## SAM forecasts consider year 1 to be the final datayear, so account for this
    ## in forecast targets
    # fscale   <- c(1,  fscale)
    # catchval <- c(NA, catchval)
    
    ## recycle target if necessary
    if (fwd_yrs > length(fwd_trgt)) {
      fscale <- c(fscale, tail(fscale, 1))
      catchval <- c(catchval, tail(catchval, 1))
    }
    
    # The following applies if the intermediate year assumption is based on
    # advised TAC from the previous year.
    
    ## get recent TAC
    if (args$iy == ay) {
      
      ## in first year of simulation, use value from OM saved earlier in ay
      TAC_last <- tracking$stk["C.om", ac(ay)]
    } else {
      
      ## in following years, use TAC advised the year before
      TAC_last <- FLCore::FLQuant(tracking$advice[, ac(ay - 1),], 
                                  dim = c(1,1,1,1,1,dim(tracking$advice)[3]), 
                                  dimnames = list(advice = 1, 
                                                  year = ac(ay-1), 
                                                  unit = "all", 
                                                  season = "all", 
                                                  area = "unique", 
                                                  iter = dimnames(tracking$advice)$iter))
    }
    
    # -------------------------------#
    # SECTION 3.2: Carry out forecast
    # -------------------------------#
    
    ## do forecast for all iterations
    fc <- lapply(seq_along(fit), function(iter_i){
      
      ## extract i'th iteration
      fit_i <- fit[[iter_i]]
      
      fcn_i <- tryCatch({
        
        ## overwrite landing fraction with last year, if requested
        if (!is.null(fwd_yrs_lf_remove)) {
          
          ## index for years to remove/overwrite
          idx_remove <- nrow(fit_i$data$landFrac) + fwd_yrs_lf_remove
          
          ## overwrite
          fit_i$data$landFrac[idx_remove, ,] <- fit_i$data$landFrac[rep(nrow(fit_i$data$landFrac), 
                                                                       length(idx_remove)), ,]
        }
        
        ## define forecast targets for current iteration
        fscale_i <- fscale
        
        ## load TAC as catch target
        catchval_i <- ifelse(catchval == -1, c(TAC_last[,,,,, iter_i]), catchval)
        
        ## arguments for forecast
        fc_args <- list(fit = fit_i, fscale = fscale_i, catchval = catchval_i,
                        ave.years = ave.years, rec.years = rec.years,
                        overwriteSelYears = overwriteSelYears, 
                        splitLD = fwd_splitLD)
        
        ## for compatibility of stockassessment's commit a882a11 and later:
        if ("savesim" %in% names(formals(base::args(stockassessment::forecast)))) {
          fc_args$savesim <- TRUE
        }
        
        ## run forecast
        fc_i <- do.call(stockassessment::forecast, fc_args)
        
        ## get numbers at age for all forecast years
        numbers <- lapply(seq(fwd_yrs), function(x) {
          
          ## index for numbers at age
          idx <- seq(length(fit_i$conf$keyLogFsta[1, ]))
          
          ## get simulated numbers
          n <- exp(fc_i[[x]]$sim[, idx])
          
          ## median
          apply(n, 2, median)
        })
        numbers <- do.call(cbind, numbers)
        
        return(list(stock.n = numbers))
      }, error = function(e) e)
      
      return(fcn_i)
    })
    
    ## if forecast failed for a iteration, the list element will for this
    ## iteration will be an error message
    
    ## get numbers
    fc_stock.n <- lapply(fc, "[[", "stock.n")
    
    ## insert stock numbers
    for (iter_i in seq(dim(stk0)[6])) {
      
      ## do not insert numbers if forecast failed
      if (!is.numeric(fc_stock.n[[iter_i]])) next()
      FLCore::stock.n(stk0)[, ac(yrs),,,, iter_i] <- fc_stock.n[[iter_i]]
    }
    
    ## extend stock characteristics required for calculation of SSB: 
    ## weights, natural mortality, selectivity etc.
    
    ## find years to fill (do not fill years, if there is already data inside)
    yrs_fill <- setdiff(yrs, lst_yr)
    
    ## forecast stock individual mean weight at age
    FLCore::stock.wt(stk0)[, ac(yrs_fill)] <- FLCore::yearMeans(stock.wt(stk0)[, ac(ave.years)])
    
    ## forecast natural mortality and proportion mature
    FLCore::m(stk0)[, ac(yrs_fill)] <- FLCore::yearMeans(m(stk0)[, ac(ave.years)])
    FLCore::mat(stk0)[, ac(yrs_fill)] <- FLCore::yearMeans(mat(stk0)[, ac(ave.years)])
    
    ## forecast proportion fishing mortality and natural mortality before spawning
    FLCore::harvest.spwn(stk0)[, ac(yrs_fill)] <- 
      FLCore::yearMeans(FLCore::harvest.spwn(stk0)[, ac(ave.years)])
    FLCore::m.spwn(stk0)[, ac(yrs_fill)] <- FLCore::yearMeans(m.spwn(stk0)[, ac(ave.years)])
    
    ## forecast selectivity 
    FLCore::harvest(stk0)[, ac(yrs_fill)] <- FLCore::yearMeans(FLCore::harvest(stk0)[, ac(ave.years)])
    
    # PLEASE NOTE:
    # SSB value slightly different from SSB value generated from SAM:
    # SAM calculates SSB per simulation and gives median
    # here: calculate median of numbers at age and calculate SSB from
    #       median numbers
    
  }
  
  ## save convergence for all iterations
  tracking$stk["conv.est", ac(ay)] <- sapply(fit, function(x) {
    if (isTRUE(is(x, "sam"))) {
      return(x$opt$convergence)
    } else {
      return(2)
    }
  })
  
  ## estimate final data year
  dy <- dims(stk)$maxyear
  
  ## Store estimated properties
  if (dy %in% dimnames(tracking$stk)$year) {
    tracking$stk["F.est", ac(dy)]  <- FLCore::fbar(stk0)[,ac(dy)]
    tracking$stk["B.est", ac(dy)]  <- FLCore::stock(stk0)[,ac(dy)]
    tracking$stk["SB.est", ac(dy)] <- FLCore::ssb(stk0)[,ac(dy)]
    
    tracking$stk["C.est", ac(dy)] <- FLCore::catch(stk0)[,ac(dy)]
    tracking$stk["L.est", ac(dy)] <- FLCore::landings(stk0)[,ac(dy)]
    tracking$stk["D.est", ac(dy)] <- FLCore::discards(stk0)[,ac(dy)]
    
    tracking$sel_est[,ac(dy)] <- sweep(FLCore::harvest(stk0)[,ac(dy)], 
                                               c(2:6), fbar(stk0)[,ac(dy)], "/")
  }
  
  ## save model fit (list) as attribute in stk0
  attr(stk0, "fit") <- fit
  
  # return assessed stock, tracking & model fit 
  # (model fit required later for TAC calculation)
  return(list(stk0      = stk0, 
              tracking = tracking))
}