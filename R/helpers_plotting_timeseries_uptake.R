# ----------------------
# Uptake
# ----------------------

plot_uptake_MixME <- function(res, 
                              trajectories = NULL,
                              quantiles = c(0.05, 0.25, 0.75, 0.95),
                              iy = NULL,
                              add    = NULL,
                              fill   = "steelblue",
                              colour = "black") {
  
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
  if (is.null(add)) {
    plot_out <- ggplot2::ggplot() +
      facet_wrap(~stk, scales = "free_y") +
      scale_y_continuous("Quota uptake (%)") +
      theme_bw()
  } else {
    plot_out <- add
  }
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = fill, alpha = 0.2, data = summary_uptake) +
        geom_ribbon(aes(x = year, ymin = low, ymax = upp), fill = fill, alpha = 0.2, data = summary_uptake)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = fill, alpha = 0.2, data = summary_uptake)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(x = year, y = uptake_percentage, colour = iter), data = traj_uptake) +
      theme(legend.position = "none")
    
  }
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out + geom_vline(xintercept = iy, linetype = 2)
  }
  
  ## Add median line
  plot_out <- plot_out +
    geom_line(aes(x = year, y = uptake_percentage), colour = colour, data = summary_uptake)
  
  ## Improve aesthetic look when uptake is very flat
  uptakerange <- aggregate(res$uptake_percentage, by = list(stk = res$stk), function(x) diff(range(x)))
  whichrange  <- which(uptakerange$x < 1e-2)
  
  if(length(whichrange) > 0) {
    dummyrange <- expand.grid(stk  = uptakerange$stk[whichrange],
                              year = unique(res$year),
                              uptake_percentage = 0)
    plot_out <- plot_out + geom_blank(data = dummyrange,
                                      aes(x = year, 
                                          y = uptake_percentage))
  }
  
  return(plot_out)
  
}