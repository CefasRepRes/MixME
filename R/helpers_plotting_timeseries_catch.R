# ----------------------
# Catch
# ----------------------

plot_catch_MixME <- function(res, 
                             trajectories = NULL,
                             quantiles = c(0.05, 0.25, 0.75, 0.95),
                             iy = NULL,
                             add    = NULL,
                             fill   = "steelblue",
                             colour = "black") {
  
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
  if (is.null(add)) {
    plot_out <- ggplot2::ggplot() +
      facet_wrap(~stk, scales = "free_y") +
      scale_y_continuous("Catch (tonnes)") +
      theme_bw()
  } else {
    plot_out <- add
  }
  
  ## add quantiles
  if(is.numeric(quantiles)) {
    if(length(quantiles) == 4){
      plot_out <- plot_out + 
        geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = fill, alpha = 0.2, data = summary_catch) +
        geom_ribbon(aes(x = year, ymin = low, ymax = upp), fill = fill, alpha = 0.2, data = summary_catch)
    }
    if(length(quantiles) == 2){
      plot_out <- plot_out + 
        geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = fill, alpha = 0.2, data = summary_catch)
    }
  }
  
  ## add trajectories
  if(!is.null(trajectories)) {
    plot_out <- plot_out +
      geom_line(aes(x = year, y = catch, colour = iter), data = traj_catch) +
      theme(legend.position = "none")
    
  }
  
  ## (Optional) Add start line
  if(!is.null(iy)){
    plot_out <- plot_out + geom_vline(xintercept = iy, linetype = 2)
  }
  
  ## Add median line
  plot_out <- plot_out +
    geom_line(aes(x = year, y = catch), colour = colour, data = summary_catch)
  
  ## Improve aesthetic look when catch is very flat
  catchrange <- aggregate(res$catch, by = list(stk = res$stk), function(x) diff(range(x)))
  whichrange  <- which(catchrange$x < 1e-2)
  
  if(length(whichrange) > 0) {
    dummyrange <- expand.grid(stk  = catchrange$stk[whichrange],
                              year = unique(res$year),
                              catch = 0)
    plot_out <- plot_out + geom_blank(data = dummyrange,
                                      aes(x = year, 
                                          y = catch))
  }
  
  return(plot_out)
  
}
