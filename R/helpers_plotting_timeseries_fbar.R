# -----------------------------
# Mean fishing mortality - fbar
# -----------------------------

plot_fbar_MixME <- function(res, 
                            trajectories = NULL,
                            quantiles = c(0.05, 0.25, 0.75, 0.95),
                            Refpts = NULL,
                            iy = NULL) {
  
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
    scale_y_continuous("Mean fishing mortality") +
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
  
  ## (Optional) Add reference points 
  if(!is.null(Refpts)) {
    if(!is.null(Refpts$Ftrgt)) {
      plot_out <- plot_out + 
        geom_hline(aes(yintercept = Ftrgt), linetype = 3, data = Refpts)
    }
  }
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out +
      geom_vline(aes(xintercept = iy), linetype = 2)
  }
  
  ## Assemble final plot
  plot_out <- plot_out +
    geom_line(aes(y = fbar))
  
  return(plot_out)
  
}