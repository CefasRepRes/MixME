# ----------------------
# Uptake
# ----------------------

plot_uptake_MixME <- function(res, 
                              trajectories = NULL,
                              quantiles = c(0.05, 0.25, 0.75, 0.95),
                              iy = NULL) {
  
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
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out +
      geom_vline(aes(xintercept = iy), linetype = 2)
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = uptake_percentage))
  
  return(plot_out)
  
}