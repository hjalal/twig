 
  calculate_nmb <- function(slice, wtp) {
    cost <- slice[, 1]
    utility <- slice[, 2]
    nmb <- wtp * utility - cost
    return(nmb)
  }
  