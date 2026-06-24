compute_nmb_proportions <- function(payoffs_sim, wtp_range, col_names = c("cost", "utility")) {

  nmb_proportions_mat <- matrix(NA, nrow = length(wtp_range), ncol = nrow(payoffs_sim),
                                dimnames = list(NULL, dimnames(payoffs_sim)[[1]]))

  i <- 0

  for (wtp in wtp_range) {
    i <- i + 1

    nmb_results <- apply(payoffs_sim, 3, calculate_nmb, wtp = wtp, col_names = col_names)
    # With a single strategy, apply() collapses to a vector; restore the
    # decision-by-simulation matrix shape.
    if (is.null(dim(nmb_results))) {
      nmb_results <- matrix(nmb_results, nrow = nrow(payoffs_sim),
                            dimnames = list(dimnames(payoffs_sim)[[1]], NULL))
    }

    nmb_t <- t(nmb_results)                       # simulations x decisions
    n_row <- nrow(nmb_t); n_col <- ncol(nmb_t)
    all_na <- rowSums(is.na(nmb_t)) == n_col      # draws with no usable NMB
    filled <- nmb_t
    filled[is.na(filled)] <- -Inf                 # so max.col ignores NA strategies
    best <- max.col(filled, ties.method = "first")

    indicator <- matrix(0, nrow = n_row, ncol = n_col)
    keep <- which(!all_na)
    if (length(keep) > 0) indicator[cbind(keep, best[keep])] <- 1

    nmb_proportions_mat[i,] <- if (length(keep) > 0) colSums(indicator) / length(keep)
                               else rep(NA_real_, n_col)
  }

  nmb_proportions_df <- cbind(wtp = wtp_range, nmb_proportions_mat)

  nmb_proportions_long <- reshape2::melt(as.data.frame(nmb_proportions_df), id.vars = "wtp", variable.name = "decision", value.name = "prob_cost_effective")

  return(nmb_proportions_long)

}
