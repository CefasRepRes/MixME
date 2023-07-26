# ---
# title: 'Functions to help with parallelisation of MixME'
# author: 'Various'
# date: 'July 2023'
# ---

#' Set up an environment for parallel computation with \code{foreach}
#'
#' Function configures an environment for parallel computation. This will
#' depend on the operating system - windows and linux are supported.
#'
#' @param parallel Either a logical argument denoting whether parallelisation
#'                 should be carried out or an integer denoting the number of
#'                 parallel processes to generate. 
#'
#' @return cl
#'
#' @export

beginParallel <- function(parallel) {
  
  ## Check that parallel, doParallel and foreach are available
  if (!all(requireNamespace("parallel", quietly = TRUE),
           requireNamespace("doParallel", quietly = TRUE),
           requireNamespace("foreach", quietly = TRUE))) {
    stop("packages 'parallel', 'doParallel' and 'foreach' are needed for parallelisation")
  }
  
  ## setup number of workers
  if(is.numeric(parallel)) {
    nworkers <- parallel
    if(nworkers > parallel::detectCores())
      parallel <- TRUE
  }
  if(parallel == TRUE) {
    nworkers <- parallel::detectCores()-1
  }
  
  ## check operating system
  osType <- .Platform$OS.type
  
  ## setup parallel environment
  if (osType == "windows") {
    
    ## create cluster
    cl <- parallel::makeCluster(nworkers, type = "PSOCK")
    
    ## bring in variables from parent environment
    varlist <- ls(envir = parent.frame(), all.names = TRUE)
    parallel::clusterExport(cl = cl,
                            varlist = varlist[varlist != "..."],
                            envir = parent.frame())
    
    ## bring in packages from global environment
    pkgs <- .packages()
    lapply(pkgs, function(pkg)
      parallel::clusterCall(cl,library, package = pkg, character.only = TRUE))
    
    ## Start cluster
    doParallel::registerDoParallel(cl, cores = nworkers)
    
  } else {
    
    ## start cluster
    cl <- parallel::makeCluster(nworkers, type = "FORK")
    doParallel::registerDoParallel(cl, cores = nworkers)
    
  }
  
  return(cl)
}