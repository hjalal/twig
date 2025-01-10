compute_nmb_proportions <- function(payoffs_sim, wtp_range, col_names = c("cost", "utility")) {

  nmb_proportions_mat <- matrix(NA, nrow = length(wtp_range), ncol = nrow(payoffs_sim),
                                dimnames = list(NULL, dimnames(payoffs_sim)[[1]]))

  i <- 0

  for (wtp in wtp_range) {
    i <- i + 1

    nmb_results <- apply(payoffs_sim, 3, calculate_nmb, wtp = wtp)

    nmb_df <- as.data.frame(t(nmb_results))

    max_nmb <- apply(nmb_df, 1, which.max)
    nmb_mat <- matrix(0, nrow = nrow(nmb_df), ncol = ncol(nmb_df))
    nmb_mat[cbind(1:nrow(nmb_df), max_nmb)] <- 1

    nmb_proportions_mat[i,] <- colMeans(nmb_mat)
  }

  nmb_proportions_df <- cbind(wtp = wtp_range, nmb_proportions_mat)

  nmb_proportions_long <- reshape2::melt(as.data.frame(nmb_proportions_df), id.vars = "wtp", variable.name = "decision", value.name = "prob_cost_effective")

  return(nmb_proportions_long)

}
