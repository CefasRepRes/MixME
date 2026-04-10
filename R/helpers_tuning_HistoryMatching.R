# ---
# title: 'Metropolis-Hastings algorithm for MixME'
# author: 'Matthew Pace'
# date: 'February 2026'
#
# Summary
# =======
#
# A simple R implementation of the History-Matching algorithm described by
# Spence (2025) for exploring Management Procedure parameter grids.
#
# The function fits an emulator to runs of the objective function to quantify
# the uncertainty across the parameter grid

runHM <- function(f,                  # function to calculate management performance 
                  init,               # initial parameter values
                  maxiter,            # the maximum number of iterations
                  input = NULL,       # a list of inputs to 'runMixME'
                  lower = NULL,       # lower bound for each parameter
                  upper = NULL,       # upper bound for each parameter
                  step  = 1,          # step size between parameter levels
                  neval = 8,          # number of objective function evaluations per iteration
                  tol  = 1e-3,        # stopping criterion
                  ...,
                  verbose = FALSE){
  
  # Input checks
  # ------------
  
  ## check management performance function
  if (!is.function(f)) stop("f must be a function.")
  
  ## check parameters
  if (!is.numeric(init)) stop("init must be numeric (scalar or vector).")
  d <- length(init)
  
  ## bounds handling
  if (is.null(lower)) lower <- rep.int(-.Machine$double.xmin, d)
  if (is.null(upper)) upper <- rep.int( .Machine$double.xmax, d)
  lower <- as.numeric(lower); upper <- as.numeric(upper)
  if (length(lower) == 1L) lower <- rep.int(lower, d)
  if (length(upper) == 1L) upper <- rep.int(upper, d)
  if (length(step)  == 1L) step  <- rep.int(step, d)
  if (length(lower) != d || length(upper) != d || length(step) != d) stop("lower/upper/step must be length 1 or length(init).")
  if (any(lower > upper)) stop("Some lower > upper.")
  if (any(init < lower) || any(init > upper)) stop("init is outside bounds.")
  
  ## expand parameter ranges into full grid
  parlist <- lapply(seq_len(d), function(x) {
    seq(from = lower[x], to = upper[x], by = step[x])
  })
  names(parlist) <- names(init)
  if (prod(sapply(parlist, length)) > 1e10) stop("grid is too large!")
  pars <- expand.grid(parlist)
  
  ## check maximum iterations
  if (length(maxiter) != 1L || maxiter < 1L) stop("maxiter must be >= 1.")
  
  ## initialise iteration counter
  m <- 0

  ## check if input arguments supplied separately
  if (is.null(input)) {
    x <- args(...)
  }
  
  ## don't output progress of individual runs
  input$args$verbose <- F
  
  # Prepare outputs
  # ---------------
  
  ## prepare array to store proposed parameter values
  chain <- array(NA_real_, dim = c(d, maxiter, neval))
  rownames(chain) <- if (!is.null(names(init))) names(init) else paste0("x", seq_len(d))
  
  ## prepare vector to log whether proposed parameters were accepted
  accept <- logical(maxiter)
  
  ## prepare matrix to store values of evaluated parameters
  trace <- matrix(NA_real_, nrow = maxiter, ncol = neval)
  
  # iteration loop
  # ------------------------
  
  while (nrow(pars) > 0 && m < maxiter) {
    
    ## increment counter
    m = m + 1
    
    if (verbose) cat(m,"; remaining points: ", nrow(pars), "\n")
    
    ## sample points
    if (m == 1) {
      
      ## insert initial values
      chain[,1, 1]  <- init 
      
      ## sample remaining values
      sample_n   <- min(nrow(pars), neval-1)
      sample_idx <- sample(seq_len(nrow(pars)), sample_n)
      chain[,1, -1] <- t(pars[sample_idx,,drop=F])
      
      ## remove initial/sampled values from grid
      init_idx <- which(rowSums(sapply(seq_len(d), function(x) init[x] == pars[,x]))==3)
      pars <- pars[-init_idx,,drop=F]
      pars <- pars[-sample_idx,,drop=F]
      
    } else {
      
      ## sample values
      sample_n   <- min(nrow(pars), neval)
      sample_idx <- sample(seq_len(nrow(pars)), sample_n)
      chain[,m,] <- t(pars[sample_idx,,drop=F])
      
      ## remove sampled values from grid
      pars <- pars[-sample_idx,,drop=F]
    }
    
    for (ev in 1:neval) {
      
      ## update inputs
      for (di in 1:d) {
        dd <- unlist(strsplit(names(chain[di,m,ev]),"_"))
        input$ctrl_obj$phcr@args$hcrpars[[dd[1]]][dd[2]] <- chain[di,m,ev]
      }
      
      if (verbose) cat(chain[,m,ev])
      
      ## Evaluate initial parameters
      # run0 <- tryCatch(do.call(runMixME, input), 
      #                  error = function(e) stop("Initial state could not be evaluated. Check starting parameters."))
      
      run0 <- tryCatch(do.call(runMixME, input), 
                       error = function(e) NA)
      
      ## Evaluate performance at initial parameters
      if(!any(is.na(run0)) && !is.null(run0$om)) {
        perf0 <- f(run0)
      } else {
        perf0 <- 0
      }

      
      ## Error check
      if (!is.finite(perf0)) stop("Initial state has non-finite log_target; choose a different init.")
      
      ## Store performance
      trace[m, ev]  <- perf0
      
      ## Extract performance metrics
      if (verbose) cat(": ", perf0,"\n")
      
    }
    
    # if (m > 1) browser()

    ## current best point
    a <- max(trace[1:m,])
    
    ## fit Gaussian Process emulator
    mod0 <- DiceKriging::km(design   = as.data.frame(t(as.data.frame(chain[,1:m,,drop=F]))),
                            response = c(trace[1:m,,drop=F]))
    
    ## predict performance across the parameter grid and update beliefs
    pred <- predict(mod0, newdata = pars, type = "UK")
    
    ## Exclude points if the probability that they exceed the current best point
    ## is below a threshold
    
    ## probability that unevaluated points are better than evaluated points 
    pBetter <- 1 - pnorm(a, mean = pred$mean, sd = pred$sd)
    
    keep <- pBetter >= tol
    pars <- pars[keep,,drop=F]
  }
  
  ## Trace performance
  best <- max(trace[1:m,])
  max_col <- apply(trace[1:m,],1, function(x) best %in% x)
  max_row <- apply(trace[1:m,],2, function(x) best %in% x)
  best_pars <- chain[,max_col,max_row]
  
  return(list(best = best,
              best_pars = best,
              chain = chain,
              trace = trace,
              pred  = cbind(pars, 
                            mean = pred$mean, 
                            sd = pred$sd, 
                            lower95 = pred$lower95, 
                            upper95 = pred$upper95)))
}