# ----------------------
# Spawning stock biomass
# ----------------------

plot_ssb_MixME <- function(res, 
                           trajectories = NULL,
                           quantiles = c(0.05, 0.25, 0.75, 0.95),
                           Refpts = NULL,
                           iy     = NULL) {
  
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
    scale_y_continuous("SSB (tonnes)") +
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
  
  ## (Optional) Add reference points 
  if(!is.null(Refpts)) {
    if(!is.null(Refpts$Btrigger)) {
      plot_out <- plot_out + 
        geom_hline(aes(yintercept = Btrigger), linetype = 3, data = Refpts)
    }
    if(!is.null(Refpts$Blim)) {
      plot_out <- plot_out + 
        geom_hline(aes(yintercept = Blim), linetype = 3, data = Refpts)
    }
  }
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out +
      geom_vline(aes(xintercept = iy), linetype = 2)
  }
  
  plot_out <- plot_out +
    geom_line(aes(y = SSB))
  
  return(plot_out)
}
