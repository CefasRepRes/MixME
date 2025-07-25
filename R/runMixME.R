# ---
# title: 'Main function to run MixME'
# author: 'Matthew Pace'
# date: 'August 2022'
# ---
#
#' Run mixed fishery management strategy simulation
#'
#' This function carries out a single run for a set of one or more management
#' procedures (MP) to evaluate MP performance given mixed fishery technical
#' interactions. Function uses a conditioned operating model (OM) to simulate
#' true system dynamics, whereas the observation error model (OEM), MP and
#' additional arguments drive the imperfect evaluation of stock status and the
#' generation of management advice.
#'
#' @param om operating model (OM)
#' @param oem observation error model (OEM)
#' @param ctrl_obj management procedure (MP)
#' @param args named list. Additional MSE simulation arguments.
#'
#' @return A named list containing the projected operating model, a tracking
#'         object containing summary metrics for observed, estimated and true
#'         stock and fishery properties and simulation performance statistics,
#'         and the unaltered \code{ctrl_obj} and \code{args} objects.
#'
#' @export
#' @examples
#' \donttest{
#' ## load example data
#' data("mixedfishery_MixME_input")
#'
#' ## run MixME simulation
#' res <- runMixME(om  = mixedfishery_MixME_input$om, 
#'                 oem = mixedfishery_MixME_input$oem,
#'                 ctrl_obj = mixedfishery_MixME_input$ctrl_obj,
#'                 args     = mixedfishery_MixME_input$args)
#' 
#' ## plot summary time-series
#' plot_timeseries_MixME(res, quantity = "ssb")
#' plot_timeseries_MixME(res, quantity = "fbar")
#' plot_timeseries_MixME(res, quantity = "catch")
#' plot_timeseries_MixME(res, quantity = "uptake")
#' }

runMixME <- function(om,
                     oem,
                     ctrl_obj,
                     args,
                     ...) {
  
  # ===========================================================================#
  # Run check on inputs
  # ===========================================================================#
  
  # This first chunk of code check inputs
  #  1. correct naming of stock and fleet input objects
  #  2. correct class for stock and fleet input objects
  #  3. correct naming of fleet catches
  #  4. availability of quota-share data
  #  5. correct dimensions for catchability parameters
  #  6. correct control object fwd arguments
  #  7. correct args arguments
  
  ## om must contain "stks" and "flts"
  if (!any(names(om) == "stks") | !any(names(om) == "flts")) 
    stop("'om' must contain stock and fleet data in 'stks' and 'flts' respectively")
  
  ## "stks" must be "FLBiol" or "FLStocks"
  if (!(class(om$stks) %in% c("FLBiols","FLStocks")))
    stop("'stks' must be class 'FLBiols' or 'FLStocks'")
  
  ## stocks in "stks" must be named
  if (any(is.na(names(om$stks)))|any(is.null(names(om$stks)))) stop("stocks in 'stks' must be named")
  if (any(sapply(om$stks, function(x) x@name) %in% "")) stop("stock FLR objects must be named")
  if (any(is.na(names(om$flts)))|any(is.null(names(om$flts)))) stop("fleets in 'flts' must be named")
  
  ## stock names in "stks", "flts" must match
  if (!all(names(om$stks) %in% unique(unlist(lapply(om$flts, names))))|
      !all(unique(unlist(lapply(om$flts, names))) %in% names(om$stks)))
    stop("stock names in 'stks' and catches names in 'flts' must match")
  
  ## OM must contain critical elements
  if(any(unlist(lapply(om$flts, function(x){ lapply(x, function(y) is.null(attr(y,"quotashare")))}))))
    stop("each FLCatch must have an FLQuant attached as an attibute named 'quotashare'")
  if(any(unlist(lapply(om$flts, function(x) {lapply(x, function(y) is.null(dimnames(y@catch.q)$year))}))))
    stop("catchability 'catch.q' must contain a year dimension")
  
  ## args must contain critical elements
  if (!any(names(ctrl_obj$fwd@args) == c("adviceType"))) stop("'adviceType' missing in 'ctrl_obj$fwd@args'.")
  if (!any(names(ctrl_obj$fwd@args) == c("effortType"))) stop("'effortType' missing in 'ctrl_obj$fwd@args'.")
  if (!any(names(args) == c("fy"))) stop("final year 'fy' missing in 'args'.")
  if (!any(names(args) == c("iy"))) stop("Intermediate year 'iy' missing in 'args'.")
  if (!any(names(args) == c("frange"))) stop("fishing mortality range 'frange' missing in 'args'")
  if (!any(names(args) == c("management_lag"))) stop("management lag 'management_lag' missing in 'args'")
  
  ## check values of critical elements
  if (!all(ctrl_obj$fwd@args$adviceType %in% c("landings","catch"))) stop("'adviceType' must be 'landings' or 'catch'")
  if (!all(ctrl_obj$fwd@args$effortType %in% c("min","max","sqE")))  stop("'effortType' must be 'min','max' or 'sqE'")
  if (args$iy > args$fy) stop("Final year 'fy' must be greater than intermediate year 'iy'")
  
  # This second chunk handles missing inputs
  # 1. handle missing managment lag
  # 2. handle missing advice frequency
  # 3. handle missing exceptions
  # 4. handle missing multiplier
  # 5. handle non-matrix effortType
  # 6. handle non-matrix exceptions
  # 7. handle matrix exceptions with missing dimnames, incorrect dims
  
  ## handle missing arguments
  if (is.null(args$management_lag)) args$management_lag <- 1 # default management lag to 1
  if (is.null(args$frq)) args$frq <- 1                       # default advice frequency to 1
  
  if (args$management_lag > 0 & is.null(args$adviceInit)) stop("'adviceInit' missing in 'args'")
  
  ## handle exceptions and multiplier inputs
  if (is.null(ctrl_obj$fwd@args$exceptions)) ctrl_obj$fwd@args$exceptions <- matrix(1, nrow = length(om$stks), ncol = length(om$flts), dimnames = list(names(om$stks),names(om$flts)))
  if (is.null(ctrl_obj$fwd@args$multiplier)) ctrl_obj$fwd@args$multiplier <- matrix(1, nrow = length(om$stks), ncol = length(om$flts), dimnames = list(names(om$stks),names(om$flts))) 
  
  ## handle non-matrix effortType
  if (!is.matrix(ctrl_obj$fwd@args$effortType)) {
    tmp_effortType <- matrix(0, 
           nrow = length(om$flts), 
           ncol = length(args$iy:args$fy),
           dimnames = list(names(om$flts), args$iy:args$fy))
    tmp_effortType[] <- ctrl_obj$fwd@args$effortType
    ctrl_obj$fwd@args$effortType <- tmp_effortType
  }
  
  ## check that we don't have simultaneous min and max in a single year
  if(any(apply(ctrl_obj$fwd@args$effortType, 2, function(x) "min" %in% unique(x) & "max" %in% unique(x)))) 
    stop("'effortType' cannot be both 'min' and 'max' in a single year")
  
  # This third chunk handle exceptions inputs in the form of a list.
  # The value of this is that users can simple provide a list of fleets
  # containing a vector stocks for which the fleet is not effort constrained.
  
  ## handle exceptions list inputs
  if (is.list(ctrl_obj$fwd@args$exceptions)) {
    tmp_exception <- matrix(1, nrow = length(om$stks), ncol = length(om$flts), dimnames = list(names(om$stks),names(om$flts)))
    for(ii in names(ctrl_obj$fwd@args$exceptions)){
      tmp_exception[ctrl_obj$fwd@args$exceptions[[ii]],ii] <- 0
    }
    ctrl_obj$fwd@args$exceptions <- tmp_exception
  }
  
  ## handle exceptions and multiplier dimension errors
  if (any(dim(ctrl_obj$fwd@args$exceptions) != c(length(om$stks),length(om$flts)))) stop("'exceptions' dimensions must be [nstks, nflts]")
  if (any(dim(ctrl_obj$fwd@args$multiplier) != c(length(om$stks),length(om$flts)))) stop("'multiplier' dimensions must be [nstks, nflts]")
  
  ## handle exceptions and multiplier dimension name errors
  # if (any(dimnames(exceptions) != list(names(om$stks),names(om$flts)))) stop("'exceptions' dimnames must match stock names and fleet names")
  # if (any(dimnames(multiplier) != list(names(om$stks),names(om$flts)))) stop("'multiplier' dimnames must match stock names and fleet names")

  ## handle exceptions and multiplier content errors
  if(!all(c(ctrl_obj$fwd@args$exceptions) %in% c(0,1))) stop("'exceptions' must contain only 0 or 1 values") # make sure that 'exceptions' are 1 or 0
  if(any(c(ctrl_obj$fwd@args$multiplier) < 0)) stop("'multiplier' must contain positive values only")
  if(all(c(ctrl_obj$fwd@args$multiplier) == 0)) stop("'multiplier' cannot all be zero!")
  
  # if (any(apply(ctrl_obj$fwd@args$exceptions, 2, "max") == 0)) 
  #   stop("'exceptions' must contain at least one '1' for each fleet")
  
  ## Check that there are no NAs in critical slots
  if (!is.null(ctrl_obj$phcr))
    if(all(is.na(unlist(ctrl_obj$phcr@args$hcrpars))))
      stop("'hcrpars' elements are all NA in 'ctrl_obj'")
  
  if(any(is.na(sapply(om$flts, function(x) FLCore::effort(x)[,as.character(args$iy:args$fy),,,,,], simplify = "array"))))
    stop("fleet effort slots cannot contain NA values in the projection period")
  
  ## Infer some simulation arguments if these are not provided
  if (is.null(args$verbose))
    args$verbose <- FALSE
  if (is.null(args$use_fastF))
    args$use_fastF <- TRUE
  if (is.null(args$use_fastfwd))
    args$use_fastfwd <- TRUE
  
  ## If banking and borrowing is used make sure forecast extends to TACyr+1 
  ## --- do I really want to hard code this procedure?? Maybe better to bundle
  ##     into implementation system?? 
  
  ## Define discarding options if not already specified -- PROBABLY DELETE (specify in fwd)
  # overquotaDiscarding <- TRUE
  # sizeselectDiscarding <- TRUE
  
  # ===========================================================================#
  # Set up objects
  # ===========================================================================#
  
  ## If FLStocks are provided, convert FLStocks into FLBiols
  if (class(om$stks) == "FLStocks") {
    om$stks <- FLCore::FLBiols(lapply(om$stks@names,
                                      function(x) {
                                        biol <- as(om$stks[[x]],"FLBiol")
                                        
                                        biol@rec@params <- sr_list[[x]]@params
                                        biol@rec@model  <- sr_list[[x]]@model
                                        biol@rec$rec    <- NULL
                                        return(biol)
                                      }))
  }
  
  ## if stock estimation methods are used, add metrics
  if (is.function(ctrl_obj$est@args$estmethod)) {
    addmetrics <- c("conv.est") # assessment model fit convergence code
  } else {
    addmetrics <- NULL
  }
  
  ## Generate tracker for optimisation and warnings
  tracking <- makeTracking(om = om, projyrs = (args$iy):(args$fy), addmetrics = addmetrics)
  
  if (!is.null(args$adviceInit)) {
    for (i in names(om$stks)) {
      tracking[[i]]$advice[,ac(args$iy),] <- args$adviceInit[[i]]
    }
  }
  
  ## Set random number seed if provided
  if (!is.null(args$seed)) set.seed(args$seed)
  
  # ===========================================================================#
  # If parallelising - split model objects into a number of blocks
  # ===========================================================================#
  
  ## Check parallelisation
  if (is.null(args$parallel)) args$parallel <- FALSE
  if (args$parallel == TRUE) {
    
    # if no workers specified
    if (is.null(args$nworkers)) {  
      args$nworkers <- parallel::detectCores()-1 
    } 
    
    # if multiple workers available
    if (args$nworkers > 1) {
      
      ## Distribution iterations across workers
      ## - code from FLR mse because it's much nicer than my solution!
      iter_assignment <- split(seq(dims(om$stks[[1]])$iter), 
                               sort(seq(dims(om$stks[[1]])$iter) %% args$nworkers))
      
      ## set up parallel environment
      cl <- beginParallel(args$nworkers)
      
      ## stop cluster on exit
      on.exit({
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
      }, add = TRUE)
      
    } else {
      warning("'parallel' is TRUE but only 1 worker specified")
    }
  } # END if parallel = TRUE
  
  # ===========================================================================#
  # Run mp
  # ===========================================================================#
  
  if (args$parallel & (args$nworkers > 1)) {
    
    simList <- foreach(it = iter_assignment,
                       .export = c("iterOM","iterTracking","simMixME"),
                       .errorhandling = "remove",
                       .inorder = TRUE) %dorng% {
      
      ## subset operating model
      om0 <- iterOM(om, it)
      
      ## subset tracking object
      tracking0 <- iterTracking(tracking, it)
      
      ## subset observation error model
      oem0 <- oem
      if (!is.null(oem0@observations$stk))
        oem0@observations$stk <- iter(oem0@observations$stk, it)
      if (!is.null(oem0@observations$idx))
        oem0@observations$idx <- lapply(oem0@observations$idx, function(x) iter(x, it))
      if (!is.null(oem0@deviances$stk))
        oem0@deviances$stk <- lapply(oem0@deviances$stk, function(x) x[,,,,,it,,drop = FALSE])
      if (!is.null(oem0@deviances$idx))
        oem0@deviances$idx <- lapply(oem0@deviances$idx, function(x) iter(x, it))

      ## subset parts of MP control and global arguments
      ctrl_obj0 <- ctrl_obj
      if (!is.null(ctrl_obj$fwd@args$sr_residuals)) {
        ctrl_obj0$fwd@args$sr_residuals <- lapply(ctrl_obj$fwd@args$sr_residuals, function(x) iter(x, it))
      }
      if (!is.null(ctrl_obj$fwd@args$proc_res)) {
        ctrl_obj0$fwd@args$proc_res <- lapply(ctrl_obj$fwd@args$proc_res, function(x) iter(x, it))
      }
      
      args0 <- args
      args0$adviceInit <- lapply(args$adviceInit, function(x) x[,it])
      
      ## run simulation
      return(simMixME(om0,
               oem0,
               tracking0,
               ctrl_obj0,
               args0))
    }
    
    ## combine outputs
    om       <- Reduce("combineOM", lapply(simList, "[[","om"))
    tracking <- Reduce("combineTracking", lapply(simList, "[[","tracking"))
    
  } else {
    
    ## run simulation
    simList <- simMixME(om,
                        oem,
                        tracking,
                        ctrl_obj,
                        args)
    ## extract objects
    om <- simList$om
    tracking <- simList$tracking
    
  }
  
  # ===========================================================================#
  # Output results
  # ===========================================================================#

  return(list(om       = om,
              tracking = tracking,
              ctrl_obj = ctrl_obj,
              args     = args))
}
