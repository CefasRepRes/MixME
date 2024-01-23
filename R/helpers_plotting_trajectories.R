# ---
# title: 'Helper functions to plot outputs'
# author: 'Matthew Pace'
# date: 'November 2023'
# ---
#
#' Plot individual trajectories of Operating Model properties
#' 
#' Function takes an Operating model as input and plots a time-series of
#' a specified property for one or more stocks. Individual trajectories are 
#' plotted and failed trajectories are optionally highlighted or removed 
#' 
#' @export

plot_trajectories_MixME <- function(object,
                                    quantity,
                                    minyr = NULL,
                                    maxyr = NULL,
                                    stknames = NULL,
                                    fltnames = NULL,
                                    addRefpts = TRUE,
                                    failedIters = "highlight") { # highlight, partial, exclude, only
  
  # =====================================#
  # extract elements and define arguments
  # =====================================#
  
  if(!(failedIters %in% c("highlight","partial","exclude","only"))){
    stop("'failedIters' must be either 'highlight', 'partial', 'exclude' or 'only'")
  }
  
  ## extract object elements
  om       <- object$om
  tracking <- object$tracking
  
  ## define min and max year if null
  SSBmaxyr <- maxyr
  if(is.null(maxyr)) {
    maxyr    <- object$args$fy - object$args$management_lag
    SSBmaxyr <- object$args$fy
  }
  
  if(addRefpts == TRUE & !is.null(object$ctrl_obj$phcr)){
    Refpts <- as.data.frame(do.call(rbind, object$ctrl_obj$phcr@args$hcrpars))
    Refpts$stk <- rownames(Refpts)
  } else {
    Refpts <- NULL
  }
  
  ## starting projection year
  iy <- object$args$iy
  
  # =====================================#
  # calculate requested quantity
  # =====================================#
  
  if (quantity == "ssb") {
    res <- summary_ssb_MixME(object = object, minyr = minyr, maxyr = maxyr,
                             stknames = stknames)
  }
  if(quantity == "effort"){
    res <- summary_effort_MixME(object = object, minyr = minyr, maxyr = maxyr,
                                fltnames = fltnames)
  }
  if(quantity == "catch") {
    res <- summary_catch_MixME(object = object, minyr = minyr, maxyr = maxyr,
                               stknames = stknames)
  }
  if(quantity == "uptake") {
    res <- summary_uptake_MixME(object = object, minyr = minyr, maxyr = maxyr,
                                stknames = stknames)
  }
  if(quantity == "fbar"){
    res <- summary_fbar_MixME(object = object, minyr = minyr, maxyr = maxyr,
                              stknames = stknames)
  }
  if(quantity == "f") {
    res <- summary_f_MixME(object = object, minyr = minyr, maxyr = maxyr,
                           fltnames = stknames)
  }
  
  # =====================================#
  # handle failed iterations
  # =====================================#
  
  ## combine summary and iteration failure table
  res1 <- merge(res, aggregate(Freq ~ iter, data = as.data.frame.table(tracking$iterfail), FUN = sum))
  res1$year <- as.character(res1$year)
  res1$iter <- as.character(res1$iter) 
  
  ## categorise failed and successful trajectories
  res1$fail <- ifelse(res1$Freq == 0, "n", "y")
  res1      <- res1[,colnames(res1) != "Freq"]
  
  ## identify years following advice failure
  res2 <- merge(res1, as.data.frame.table(tracking$iterfail))
  res2$fail <- ifelse(res2$fail == "y" & res2$Freq > 0, "yy", res2$fail)
  
  ## identify year where first advice failure occurs
  res3 <- res2[res2$Freq > 0,]
  
  if (quantity %in% c("ssb","catch","fbar")) {
    res3 <- res3[!duplicated(res3[,c("iter","age","unit","season","area","stk")]),]
  }
  if (quantity %in% c("effort")) {
    res3 <- res3[!duplicated(res3[,c("iter","unit","season","area","flt")]),]
  }
  if (quantity %in% c("uptake")) {
    res3 <- res3[!duplicated(res3[,c("iter","stk")]),]
  }
  
  ## ensure continuous scale x-axis
  res2$year <- as.numeric(res2$year)
  res3$year <- as.numeric(res3$year)
  
  # ------------------------------#
  # highlight failed trajectories
  # ------------------------------#
  
  if (failedIters == "highlight") {
    if (quantity == "ssb") {
      out <- ggplot2::ggplot() + 
        geom_line(aes(x = year, y = SSB, group = iter, colour = fail), alpha = 0.5, data = res2) +
        geom_point(aes(x = year, y = SSB), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("SSB (tonnes)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "effort"){
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = effort, group = iter, colour = fail), alpha = 0.5, data = res2) +
        geom_point(aes(x = year, y = effort), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Effort (...)") +
        facet_wrap(~flt, scales = "free_y")
    }
    if(quantity == "catch") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = catch, group = iter, colour = fail), alpha = 0.5, data = res2) +
        geom_point(aes(x = year, y = catch), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Catch (tonnes)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "uptake") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = uptake_percentage, group = iter, colour = fail), alpha = 0.5, data = res2) +
        geom_point(aes(x = year, y = uptake_percentage), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Quota uptake (%)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "fbar") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = fbar, group = iter, colour = fail), alpha = 0.5, data = res2) +
        geom_point(aes(x = year, y = fbar), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Mean fishing mortality") +
        facet_wrap(~stk, scales = "free_y")
    }

    ## Apply colour pattern
    out <- out +
      scale_color_manual(values = c("steelblue", "grey30", "darkred"))
  }
  
  # ------------------------------#
  # partial failed trajectories
  # ------------------------------#
  
  if (failedIters == "partial") {
    if (quantity == "ssb") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = SSB, group = iter, colour = fail!="n"), alpha = 0.5, data = rbind(res2[res2$fail != "yy",], res3)) +
        geom_point(aes(x = year, y = SSB), colour = "gray20", alpha = 0.5, data = res3) +
        scale_y_continuous("SSB (tonnes)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "effort"){
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = effort, group = iter, colour = fail!="n"), alpha = 0.5, data = rbind(res2[res2$fail != "yy",], res3)) +
        geom_point(aes(x = year, y = effort), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Effort (...)") +
        facet_wrap(~flt, scales = "free_y")
    }
    if(quantity == "catch") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = catch, group = iter, colour = fail != "n"), alpha = 0.5, data = rbind(res2[res2$fail != "yy",], res3)) +
        geom_point(aes(x = year, y = catch), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Catch (tonnes)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "uptake") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = uptake_percentage, group = iter, colour = fail != "n"), alpha = 0.5, data = rbind(res2[res2$fail != "yy",], res3)) +
        geom_point(aes(x = year, y = uptake_percentage), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Quota uptake (%)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "fbar"){
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = fbar, group = iter, colour = fail != "n"), alpha = 0.5, data = rbind(res2[res2$fail != "yy",], res3)) +
        geom_point(aes(x = year, y = fbar), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Mean fishing mortality") +
        facet_wrap(~stk, scales = "free_y")
    }
    
    ## Apply colour pattern
    out <- out +
      scale_color_manual(values = c("steelblue", "darkred"))
  }
  
  # ------------------------------#
  # exclude failed trajectories
  # ------------------------------#
  
  if (failedIters == "exclude") {
    if (quantity == "ssb") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = SSB, group = iter), colour = "steelblue", alpha = 0.5, data = res2[res2$fail == "n",]) +
        scale_y_continuous("SSB (tonnes)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "effort"){
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = effort, group = iter), colour = "steelblue", alpha = 0.5, data = res2[res2$fail == "n",]) +
        scale_y_continuous("Effort (...)") +
        facet_wrap(~flt, scales = "free_y")
    }
    if(quantity == "catch") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = catch, group = iter), colour = "steelblue", alpha = 0.5, data = res2[res2$fail == "n",]) +
        scale_y_continuous("Catch (tonnes)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "uptake") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = uptake_percentage, group = iter), colour = "steelblue", alpha = 0.5, data = res2[res2$fail == "n",]) +
        scale_y_continuous("Quota uptake (%)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "fbar"){
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = fbar, group = iter), colour = "steelblue", alpha = 0.5, data = res2[res2$fail == "n",]) +
        scale_y_continuous("Mean fishing mortality") +
        facet_wrap(~stk, scales = "free_y")
    }
  }
  
  # ------------------------------#
  # keep only failed trajectories
  # ------------------------------#
  
  if (failedIters == "only") {
    if (quantity == "ssb") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = SSB, group = iter, colour = fail), alpha = 0.5, data = res2[res2$fail != "n",]) +
        geom_point(aes(x = year, y = SSB), colour = "gray20", alpha = 0.5, data = res3) +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "effort"){
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = effort, group = iter, colour = fail), alpha = 0.5, data = res2[res2$fail != "n",]) +
        geom_point(aes(x = year, y = effort), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Effort (...)") +
        facet_wrap(~flt, scales = "free_y")
    }
    if(quantity == "catch") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = catch, group = iter, colour = fail), alpha = 0.5, data = res2[res2$fail != "n",]) +
        geom_point(aes(x = year, y = catch), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Catch (tonnes)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "uptake") {
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = uptake_percentage, group = iter, colour = fail), alpha = 0.5, data = res2[res2$fail != "n",]) +
        geom_point(aes(x = year, y = uptake_percentage), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Quota uptake (%)") +
        facet_wrap(~stk, scales = "free_y")
    }
    if(quantity == "fbar"){
      out <- ggplot2::ggplot() +
        geom_line(aes(x = year, y = fbar, group = iter, colour = fail), alpha = 0.5, data = res2[res2$fail != "n",]) +
        geom_point(aes(x = year, y = fbar), colour = "grey20", alpha = 0.5, data = res3) +
        scale_y_continuous("Mean fishing mortality") +
        facet_wrap(~stk, scales = "free_y")
    }
    
    ## (Optional) Add reference points 
    if (quantity == "ssb") {
      if(!is.null(Refpts)) {
        if(!is.null(Refpts$Btrigger)) {
          out <- out + 
            geom_hline(aes(yintercept = Btrigger), linetype = 3, data = Refpts)
        }
        if(!is.null(Refpts$Blim)) {
          out <- out + 
            geom_hline(aes(yintercept = Blim), linetype = 3, data = Refpts)
        }
      }
    }
    if (quantity == "fbar") {
      if(!is.null(Refpts)) {
        if(!is.null(Refpts$Ftrgt)) {
          out <- out + 
            geom_hline(aes(yintercept = Ftrgt), linetype = 3, data = Refpts)
        }
      }
    }
    
    ## (Optional) Add start line
    if(!is.null(iy)){
      out <- out +
        geom_vline(xintercept = iy, linetype = 2)
    }
        
    ## Apply colour pattern
    out <- out +
      scale_color_manual(values = c("grey30", "darkred"))
  }
  
  out <- out +
    theme_bw() +
    theme(legend.position = "none")
  
  return(out)
}