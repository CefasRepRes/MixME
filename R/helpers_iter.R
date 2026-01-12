# subset tracking object along iteration dimension 
# ================================================#
# 
# Function subsets a MixME tracking for a specified vector of iterations
#
#' @export

iterTracking <- function(tracking, it) {
  
  ## iterate over each element of tracking
  tracking0 <- lapply(tracking, function(i) {
    ## process stocks
    if (is.list(i)) {
      
      ## subset each stock summary object
      return(list(advice = i$advice[,,it, drop = FALSE],
                  sel_om  = iter(i$sel_om, it),
                  sel_est = iter(i$sel_est, it),
                  stk     = iter(i$stk, it)))
    }
    ## process arrays
    if(is.array(i)) {
      d <- dim(i)
      return(eval(parse(text = paste0("i[",
                                      paste0(rep(",",length(d)-1),collapse = ""),
                                      "c(",
                                      paste(it, collapse = ","),"),drop = FALSE]",collapse = ""))))
    }
  })
  return(tracking0)
}

# combine two MixME tracking objects into a single object 
# ==============================================================#
# 
# Function combines two MixME tracking objects
#
#' @export

combineTracking <- function(x, y) {
  
  ## iterate over each element of tracking
  x <- Map(function(i, j) {
    ## process stocks
    if (is.list(i)) {
      di <- dim(i$advice)
      dj <- dim(j$advice)
      xi <- array(c(i$advice,j$advice), 
                  dim = c(di[1],di[2],di[3]+dj[3]),
                  dimnames = Map(function(m, n) {unique(c(m, n))}, m = dimnames(i$advice), n = dimnames(j$advice)))
      
      ## combine each stock summary object
      return(list(advice  = xi,
                  sel_om  = do.call(FLCore::combine, 
                                    list(x = i$sel_om,
                                         y = j$sel_om)),
                  sel_est = do.call(FLCore::combine, 
                                    list(x = i$sel_est,
                                         y = j$sel_est)),
                  stk     = do.call(FLCore::combine, 
                                    list(x = i$stk,
                                         y = j$stk))))
    }
    
    ## process arrays
    if (is.array(i)) {
      di <- dim(i)
      dj <- dim(j)
      dnij <- Map(function(m, n) {unique(c(m, n))}, m = dimnames(i), n = dimnames(j))
      dij <- sapply(dnij, length)
      
      xi <- array(c(i, j), dim = dij, dimnames = dnij)
      return(xi)
      
    }
  }, i = x, j = y)
  
  return(x)
}

# subset MixME operating model object along iteration dimension 
# ==============================================================#
# 
# Function subsets a MixME operating model for a specified vector of iterations
#
#' @export

iterOM <- function(om, it) {
  
  ## subset main structure
  om0  <- lapply(om, function(x) iter(x, it))

  ## subset quotashare
  for(x in seq_along(om0$flts)){
    for(y in seq_along(om0$flts[[x]]))
      attr(om0$flts[[x]][[y]], "quotashare") <- iter(attr(om0$flts[[x]][[y]], "quotashare"), it)
  }
  return(om0)
}

# combine two MixME operating models into a single object 
# ==============================================================#
# 
# Function combines two MixME operating model objects
#
#' @export

combineOM <- function(x, y) {
  
  ## Combine biols
  x$stks <- FLBiols(Map(FLCore::combine, x = x$stks, y = y$stks))
  
  ## Combine fisheries
  x$flts <- Map(FLCore::combine, x = x$flts, y = y$flts)
  
  ## Combine quotashare
  for(i in seq_along(x$flts)){
    for(j in seq_along(x$flts[[i]]))
      attr(x$flts[[i]][[j]], "quotashare") <- do.call(FLCore::combine, 
                                                      list(x = attr(x$flts[[i]][[j]], "quotashare"),
                                                           y = attr(y$flts[[i]][[j]], "quotashare")))
  }
  return(x)
}

# subset MixME object along iteration dimension
# ==============================================#
#
# Function subsets a MixME operating model for a specified vector of iteration
#
#' @export

iterMixME <- function(om, tracking = NULL, oem, ctrl_obj, args, it) {
  
  ## subset operating model
  om0 <- iterOM(om, it)
  
  ## subset tracking object
  if(!is.null(tracking)) {
    tracking0 <- iterTracking(tracking, it)
    tracking  <- tracking0
  }
  
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
  
  return(list(om       = om0,
              oem      = oem0,
              tracking = tracking,
              ctrl_obj = ctrl_obj0,
              args     = args0))
}