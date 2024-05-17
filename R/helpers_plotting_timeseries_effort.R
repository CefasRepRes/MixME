# ----------------------
# effort
# ----------------------

plot_effort_MixME <- function(res, 
                              trajectories = NULL,
                              quantiles = c(0.05, 0.25, 0.75, 0.95),
                              iy = NULL,
                              add    = NULL,
                              fill   = "steelblue",
                              colour = "black") {
  
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
  if (is.null(add)) {
    plot_out <- ggplot2::ggplot() +
      facet_wrap(~flt, scales = "free_y") +
      scale_y_continuous("Effort (...)") +
      theme_bw()
  } else {
    plot_out <- add
  }
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = fill, alpha = 0.2, data = summary_effort) +
        geom_ribbon(aes(x = year, ymin = low, ymax = upp), fill = fill, alpha = 0.2, data = summary_effort)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = fill, alpha = 0.2, data = summary_effort)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(x = year, y = effort, colour = iter), data = traj_effort) +
      theme(legend.position = "none")
    
  }
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out + geom_vline(xintercept = iy, linetype = 2)
  }
  
  plot_out <- plot_out +
    geom_line(aes(x = year, y = effort), colour = colour, data = summary_effort)
  
  return(plot_out)
  
}
