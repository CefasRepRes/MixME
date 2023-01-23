# ---
# title: 'Helper functions to plotting outputs'
# author: 'Matthew Pace'
# date: 'December 2022'
# ---
#
#' Plot time-series of Operating Model properties
#' 
#' Function takes an Operating model as input and plots a time-series of
#' a specified property for one or more stocks.
#' 
#' @export

plot_timeseries_MixME <- function(object,
                                  quantity,
                                  minyr = NULL,
                                  maxyr = NULL,
                                  stknames = NULL,
                                  fltnames = NULL,
                                  trajectories = NULL,
                                  quantiles = c(0.05, 0.25, 0.75, 0.95)) {
  
  # -------------------------------------
  # extract elements and define arguments
  # -------------------------------------
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## define quantiles if needed
  if(isTRUE(quantiles))
    quantiles <- c(0.05, 0.25, 0.75, 0.95)
  
  # -------------------------------------
  # calculate requested quantity
  # -------------------------------------
  
  if(quantity == "ssb") {
    res <- summary_ssb_MixME(object = object, minyr = minyr, maxyr = maxyr,
                             stknames = stknames)
    
    out <- plot_ssb_MixME(res = res, trajectories = trajectories, 
                          quantiles = quantiles)
  }
  
  if(quantity == "effort"){
    res <- summary_effort_MixME(object = object, minyr = minyr, maxyr = maxyr,
                                fltnames = fltnames)
    
    out <- plot_effort_MixME(res = res, trajectories = trajectories, 
                             quantiles = quantiles)
  }
  
  if(quantity == "catch") {
    res <- summary_catch_MixME(object = object, minyr = minyr, maxyr = maxyr,
                               stknames = stknames)
    
    out <- plot_catch_MixME(res = res, trajectories = trajectories, 
                             quantiles = quantiles)
  }
  
  if(quantity == "uptake") {
    res <- summary_uptake_MixME(object = object, minyr = minyr, maxyr = maxyr,
                                stknames = stknames)
    
    out <- plot_uptake_MixME(res = res, trajectories = trajectories, 
                            quantiles = quantiles)
  }
  
  if(quantity == "fbar"){
    res <- summary_fbar_MixME(object = object, minyr = minyr, maxyr = maxyr,
                              stknames = stknames)
    
    out <- plot_fbar_MixME(res = res, trajectories = trajectories,
                           quantiles = quantiles)
  }
  
  if(quantity == "f") {
    res <- summary_f_MixME(object = object, minyr = minyr, maxyr = maxyr,
                           fltnames = stknames)
  }
  
  
  return(out)
}

# ----------------------
# Spawning stock biomass
# ----------------------

plot_ssb_MixME <- function(res, 
                           trajectories = NULL,
                           quantiles = c(0.05, 0.25, 0.75, 0.95)) {
  
  ## Calculate median ssb
  summary_ssb <- aggregate(res, SSB ~ year + stk, quantile, probs = 0.5)
  
  ## summarise quantiles
  if(is.numeric(quantiles)){
    
    if(!(length(quantiles) %in% c(2,4))) {
      stop("quantiles must be a vector of 2 or 4 values")
    }
    
    for(q in quantiles){
      
      if(length(quantiles) == 4)
        l <- c("min","low","upp","max")[q == quantiles[order(quantiles)]]
      if(length(quantiles) == 2)
        l <- c("min","max")[q == quantiles[order(quantiles)]]
      
      summary_ssb[,l] <- 
        aggregate(res, SSB ~ year + stk, quantile, probs = q)$SSB
      
    }
  }
  
  ## sample trajectories
  if(!is.null(trajectories)){
    
    itraj <- sample(1:max(res$iter), trajectories, replace = FALSE)
    traj_ssb <- res[res$iter %in% itraj,]
    traj_ssb$iter <- as.character(traj_ssb$iter)
    
  }
  
  ## build plot
  plot_out <- ggplot2::ggplot(data = summary_ssb,
                              aes(x = year)) +
    facet_wrap(~stk, scales = "free_y") +
    scale_y_continuous("Spawning stock biomass (tonnes)") +
    theme_bw()
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2) +
        geom_ribbon(aes(ymin = low, ymax = upp), fill = "steelblue", alpha = 0.2)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(y = SSB, colour = iter), data = traj_ssb) +
      theme(legend.position = "none")
    
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = SSB), size = 1.2)
  
  return(plot_out)
}

# ----------------------
# effort
# ----------------------

plot_effort_MixME <- function(res, 
                              trajectories = NULL,
                              quantiles = c(0.05, 0.25, 0.75, 0.95)) {
  
  ## Calculate median effort
  summary_effort <- aggregate(res, effort ~ year + flt, quantile, probs = 0.5)
  
  ## summarise quantiles
  if(is.numeric(quantiles)){
    
    if(!(length(quantiles) %in% c(2,4))) {
      stop("quantiles must be a vector of 2 or 4 values")
    }
    
    for(q in quantiles){
      
      if(length(quantiles) == 4)
        l <- c("min","low","upp","max")[q == quantiles[order(quantiles)]]
      if(length(quantiles) == 2)
        l <- c("min","max")[q == quantiles[order(quantiles)]]
      
      summary_effort[,l] <- 
        aggregate(res, effort ~ year + flt, quantile, probs = q)$effort
      
    }
  }
  
  ## sample trajectories
  if(!is.null(trajectories)){
    
    itraj <- sample(1:max(res$iter), trajectories, replace = FALSE)
    traj_effort <- res[res$iter %in% itraj,]
    traj_effort$iter <- as.character(traj_effort$iter)
    
  }
  
  ## build plot
  plot_out <- ggplot2::ggplot(data = summary_effort,
                              aes(x = year)) +
    facet_wrap(~flt, scales = "free_y") +
    scale_y_continuous("Effort (...)") +
    theme_bw()
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2) +
        geom_ribbon(aes(ymin = low, ymax = upp), fill = "steelblue", alpha = 0.2)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(y = effort, colour = iter), data = traj_effort) +
      theme(legend.position = "none")
    
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = effort), size = 1.2)
  
  return(plot_out)
  
}

# ----------------------
# Catch
# ----------------------

plot_catch_MixME <- function(res, 
                             trajectories = NULL,
                             quantiles = c(0.05, 0.25, 0.75, 0.95)) {
  
  ## Calculate median catch
  summary_catch <- aggregate(res, catch ~ year + stk, quantile, probs = 0.5)
  
  ## summarise quantiles
  if(is.numeric(quantiles)){
    
    if(!(length(quantiles) %in% c(2,4))) {
      stop("quantiles must be a vector of 2 or 4 values")
    }
    
    for(q in quantiles){
      
      if(length(quantiles) == 4)
        l <- c("min","low","upp","max")[q == quantiles[order(quantiles)]]
      if(length(quantiles) == 2)
        l <- c("min","max")[q == quantiles[order(quantiles)]]
      
      summary_catch[,l] <- 
        aggregate(res, catch ~ year + stk, quantile, probs = q)$catch
      
    }
  }
  
  ## sample trajectories
  if(!is.null(trajectories)){
    
    itraj <- sample(1:max(res$iter), trajectories, replace = FALSE)
    traj_catch <- res[res$iter %in% itraj,]
    traj_catch$iter <- as.character(traj_catch$iter)
    
  }
  
  ## build plot
  plot_out <- ggplot2::ggplot(data = summary_catch,
                              aes(x = year)) +
    facet_wrap(~stk, scales = "free_y") +
    scale_y_continuous("Catch (tonnes)") +
    theme_bw()
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2) +
        geom_ribbon(aes(ymin = low, ymax = upp), fill = "steelblue", alpha = 0.2)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(y = catch, colour = iter), data = traj_catch) +
      theme(legend.position = "none")
    
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = catch), size = 1.2)
  
  return(plot_out)

}

# ----------------------
# Uptake
# ----------------------

plot_uptake_MixME <- function(res, 
                              trajectories = NULL,
                              quantiles = c(0.05, 0.25, 0.75, 0.95)) {
  
  ## Calculate median catch
  summary_uptake <- aggregate(res, uptake_percentage ~ year + stk, quantile, probs = 0.5)
  
  ## summarise quantiles
  if(is.numeric(quantiles)){
    
    if(!(length(quantiles) %in% c(2,4))) {
      stop("quantiles must be a vector of 2 or 4 values")
    }
    
    for(q in quantiles){
      
      if(length(quantiles) == 4)
        l <- c("min","low","upp","max")[q == quantiles[order(quantiles)]]
      if(length(quantiles) == 2)
        l <- c("min","max")[q == quantiles[order(quantiles)]]
      
      summary_uptake[,l] <- 
        aggregate(res, uptake_percentage ~ year + stk, quantile, probs = q)$uptake_percentage
      
    }
  }
  
  ## sample trajectories
  if(!is.null(trajectories)){
    
    itraj <- sample(1:max(res$iter), trajectories, replace = FALSE)
    traj_uptake <- res[res$iter %in% itraj,]
    traj_uptake$iter <- as.character(traj_uptake$iter)
    
  }
  
  ## build plot
  plot_out <- ggplot2::ggplot(data = summary_uptake,
                              aes(x = year)) +
    facet_wrap(~stk, scales = "free_y") +
    scale_y_continuous("Quota uptake (%)") +
    theme_bw()
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2) +
        geom_ribbon(aes(ymin = low, ymax = upp), fill = "steelblue", alpha = 0.2)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(y = uptake_percentage, colour = iter), data = traj_uptake) +
      theme(legend.position = "none")
    
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = uptake_percentage), size = 1.2)
  
  return(plot_out)
  
}

# -----------------------------
# Mean fishing mortality - fbar
# -----------------------------

plot_fbar_MixME <- function(res, 
                            trajectories = NULL,
                            quantiles = c(0.05, 0.25, 0.75, 0.95)) {
  
  ## Calculate median fbar
  summary_fbar <- aggregate(res, fbar ~ year + stk, quantile, probs = 0.5)
  
  ## summarise quantiles
  if(is.numeric(quantiles)){
    
    if(!(length(quantiles) %in% c(2,4))) {
      stop("quantiles must be a vector of 2 or 4 values")
    }
    
    for(q in quantiles){
      
      if(length(quantiles) == 4)
        l <- c("min","low","upp","max")[q == quantiles[order(quantiles)]]
      if(length(quantiles) == 2)
        l <- c("min","max")[q == quantiles[order(quantiles)]]
      
      summary_fbar[,l] <- 
        aggregate(res, fbar ~ year + stk, quantile, probs = q)$fbar
      
    }
  }
  
  ## sample trajectories
  if(!is.null(trajectories)){
    
    itraj <- sample(1:max(res$iter), trajectories, replace = FALSE)
    traj_fbar <- res[res$iter %in% itraj,]
    traj_fbar$iter <- as.character(traj_fbar$iter)
    
  }
  
  ## build plot
  plot_out <- ggplot2::ggplot(data = summary_fbar,
                              aes(x = year)) +
    facet_wrap(~stk, scales = "free_y") +
    scale_y_continuous("Quota uptake (%)") +
    theme_bw()
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2) +
        geom_ribbon(aes(ymin = low, ymax = upp), fill = "steelblue", alpha = 0.2)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(ymin = min, ymax = max), fill = "steelblue", alpha = 0.2)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(y = fbar, colour = iter), data = traj_fbar) +
      theme(legend.position = "none")
    
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = fbar), size = 1.2)
  
  return(plot_out)
  
}