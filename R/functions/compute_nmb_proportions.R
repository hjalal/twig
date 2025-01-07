compute_nmb_proportions <- function(rewards_sim, wtp_range, col_names = c("cost", "utility")) {

  # Initialize a list to store the results
  nmb_proportions_mat <- matrix(NA, nrow = length(wtp_range), ncol = nrow(rewards_sim),
                                dimnames = list(NULL, dimnames(rewards_sim)[[1]]))
  
  i <- 0
  # Loop over each value in wtp_range
  for (wtp in wtp_range) {
    i <- i + 1
    # Apply the NMB formula to each slice of the 3D array
    nmb_results <- apply(rewards_sim, 3, calculate_nmb, wtp = wtp)
    
    # Transpose the results to have simulations as rows and decisions as columns
    nmb_df <- as.data.frame(t(nmb_results))
    
    # Add column names for decisions
    # colnames(nmb_df) <- dimnames(rewards_sim)[[1]]
    
    # Identify the column with the highest value for each row
    max_nmb <- apply(nmb_df, 1, which.max)
    nmb_mat <- matrix(0, nrow = nrow(nmb_df), ncol = ncol(nmb_df))
    nmb_mat[cbind(1:nrow(nmb_df), max_nmb)] <- 1

    # Store the proportions in the list
    nmb_proportions_mat[i,] <- colMeans(nmb_mat)
  }
  
  # Convert the list to a data frame
  # Add the wtp_range as a column
  nmb_proportions_df <- cbind(wtp = wtp_range, nmb_proportions_mat)
  
    # Reshape nmb_proportions_df to long format using reshape2
  nmb_proportions_long <- reshape2::melt(as.data.frame(nmb_proportions_df), id.vars = "wtp", variable.name = "decision", value.name = "prob_cost_effective")
  
  return(nmb_proportions_long)
  #colnames(nmb_proportions_long) <- c("wtp", "decision", "prob_cost_effective")
}
