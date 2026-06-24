
  calculate_nmb <- function(slice, wtp, col_names = c("cost", "utility")) {
    cost <- slice[, col_names[1]]
    utility <- slice[, col_names[2]]
    nmb <- wtp * utility - cost
    return(nmb)
  }
  