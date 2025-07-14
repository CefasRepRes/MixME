# ---
# title: 'Function to calculate landings, discards numbers at age'
# author: 'Matthew Pace'
# date: 'January 2025'
# ---

## function to get total landings / discard numbers for a specified stock
getC <- function(object, x, sl, summarise = TRUE) {
  
  ## loop over each fleet
  res <- lapply(seq_along(object), function(ii) {
    
    ## subset object
    i = object[[ii]]
    
    ## find catch that corresponds to species name
    stki <- match(x, names(i))
    if (is.na(stki)) {
      
      ## return null if species not caught by fleet
      return(NULL)
    } else {
      
      ## return slot aggregated over area
      if (sl %in% c("landings.n","discards.n")) {
        return(apply(slot(i[[x]], sl), c(1:4,6), sum))
      }
      if (sl == "landings.wt") {
        LF <- sweep(slot(i[[x]], "landings.n"),c(1:4,6),apply(slot(i[[x]],"landings.n"),c(1:4,6),sum),"/")
        LF[is.na(LF)] <- 0
        return(apply(slot(i[[x]],"landings.wt")*LF,c(1:4,6),sum))
      }
      if (sl == "discards.wt") {
        DF <- sweep(slot(i[[x]], "discards.n"),c(1:4,6),apply(slot(i[[x]],"discards.n"),c(1:4,6),sum),"/")
        DF[is.na(DF)] <- 0
        return(apply(slot(i[[x]],"discards.wt")*DF,c(1:4,6),sum))
      }
    }
  })
  ## Assign fleet names
  names(res) <- names(object)
  
  ## remove list elements where stock is not caught
  id <- !sapply(res, is.null)
  if (summarise) {
    return(Reduce("+", res[id]))
  } else {
    return(simplify2array(res[id]))
  }
}

## function to get total landings, discards weights for a specified stock
getCW <- function(object, x, sl = "landings", summarise = TRUE) {
  
  ## loop over each fleet
  res <- sapply(seq_along(object), function(ii) {
    
    ## subset object
    i = object[[ii]]
    
    ## find catch that corresponds to species name
    stki <- match(x, names(i))
    if (is.na(stki)) {
      
      ## return null if species not caught by fleet
      return(NULL)
    } else {
      
      ## return slot aggregated over area
      return(apply(slot(i[[x]], paste0(sl,".n")) * slot(i[[x]], paste0(sl,".wt")), 
                   c(2:4,6), sum, na.rm=TRUE))
    }
  },simplify = FALSE, USE.NAMES = TRUE)
  
  ## remove list elements where stock is not caught
  id <- !sapply(res, is.null)
  
  if (summarise) {
    return(Reduce("+", res[id]))
    
  } else {
    
    ## simplify to a 7D array
    res = simplify2array(res[id])
    
    ## add fleet names
    dn = c(dimnames(res)[1:6], list(fleet = names(object)[id]))
    dimnames(res) <- dn
    
    return(res)
  }
}
