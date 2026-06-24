# The CEAC must respect col_names regardless of the payoff column order.

test_that("compute_nmb_proportions selects payoffs by name, not position", {
  # Payoff order is (qaly, cost): the opposite of the default (cost, utility).
  sim_ev <- array(
    c(0.9, 0.8,     # qaly  for A, B
      1000, 500,    # cost  for A, B
      0.9, 0.8,
      1000, 500),
    dim = c(2, 2, 2),
    dimnames = list(c("A", "B"), c("qaly", "cost"), NULL))

  long <- compute_nmb_proportions(sim_ev, wtp_range = c(1e6),
                                  col_names = c("cost", "qaly"))
  # At a very high willingness-to-pay, the higher-QALY strategy (A) is
  # cost-effective with probability 1.
  expect_equal(long$prob_cost_effective[long$decision == "A"], 1)
  expect_equal(long$prob_cost_effective[long$decision == "B"], 0)
})

test_that("plot_ceac runs with a non-default payoff order", {
  sim_ev <- array(
    c(0.9, 0.8, 1000, 500, 0.9, 0.8, 1000, 500),
    dim = c(2, 2, 2),
    dimnames = list(c("A", "B"), c("qaly", "cost"), NULL))
  p <- plot_ceac(sim_ev, wtp_range = seq(0, 1e5, by = 1e4),
                 col_names = c("cost", "qaly"))
  expect_s3_class(p, "ggplot")
})
