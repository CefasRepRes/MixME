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
#' Structure of the function is heavily influenced by similar parallelisation
#' helper function developed by Luca Scrucca (2013).
#' GA: A Package for Genetic Algorithms in R.
#' Journal of Statistical Software, 53(4), 1-37.
#' https://doi.org/10.18637/jss.v053.i04
#'
#' @param parallel logical or numeric. If \code{TRUE}, the number of clusters
#'                 will be the number of logical processors available minus 1.
#'                 If numeric, the number of clusters will match the supplied
#'                 value (up to the number of logical processors available
#'                 minus 1).
#'
#' @return cluster object
#'
#' @export

beginParallel <- function(parallel) {
  
  ## Check that parallel, doParallel and foreach are available
  if (!all(requireNamespace("parallel", quietly = TRUE),
           requireNamespace("doParallel", quietly = TRUE),
           requireNamespace("foreach", quietly = TRUE),
           requireNamespace("doRNG", quietly = TRUE))) {
    stop("packages 'parallel', 'doParallel' and 'foreach' are needed for parallelisation")
  }
  
  ## setup number of workers
  if(is.numeric(parallel)) {
    nworkers <- round(parallel)
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