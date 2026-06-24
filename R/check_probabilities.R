# Validate that evaluated probability-function values are finite and lie in
# [0, 1] (within a small numerical tolerance). Called during simulation so that
# a probability function returning, say, 1.2 or NA fails loudly instead of
# silently producing impossible transitions and nonsensical expected values.
check_prob_values <- function(prob_mat, prob_funs, tol = 1e-8) {
  for (i in seq_along(prob_funs)) {
    v <- prob_mat[, i]
    if (anyNA(v) || any(!is.finite(v))) {
      stop("Probability function '", prob_funs[i],
           "' returned a non-finite value (NA, NaN, or Inf). ",
           "Probabilities must be finite numbers in [0, 1].", call. = FALSE)
    }
    if (any(v < -tol | v > 1 + tol)) {
      stop("Probability function '", prob_funs[i],
           "' returned a value outside [0, 1] (range observed: ",
           format(min(v)), " to ", format(max(v)), "). ",
           "Probabilities must be in [0, 1].", call. = FALSE)
    }
  }
  invisible(TRUE)
}
