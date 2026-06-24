# Semantic checks for the visualization helpers: the diagrams must reflect the
# actual model, not just return an object of the right class.

test_that("draw_transitions reflects the transition matrix from a real run", {
  local_twig_funs(
    pDie = function(state) 0.1 * (state == "Alive"),
    cost = function() 1
  )
  mod <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = de, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(Dead, stay)) +
      payoffs(names = c(cost))
  )
  res <- suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 3,
                                   verbose = TRUE, parallel = FALSE, progress_bar = FALSE))
  dot <- draw_transitions(res, output = "dot")

  expect_match(dot, '"Alive" -> "Dead"')   # the modeled death transition
  expect_match(dot, '"Alive" -> "Alive"')  # staying alive
  expect_match(dot, "0.1")                  # P(Alive -> Dead) = 0.1
  # Death is absorbing: there must be no transition out of Dead back to Alive.
  expect_false(grepl('"Dead" -> "Alive"', dot))
})

test_that("draw_transitions selects the requested decision", {
  tp <- array(c(0.9, 0, 0.1, 1,     # decision A: P(Alive->Dead) = 0.1
                0.7, 0, 0.3, 1),    # decision B: P(Alive->Dead) = 0.3
              dim = c(2, 2, 2),
              dimnames = list(state = c("Alive", "Dead"),
                              dest = c("Alive", "Dead"),
                              decision = c("A", "B")))
  expect_match(draw_transitions(tp, decision = "A", output = "dot"), "0.1")
  expect_match(draw_transitions(tp, decision = "B", output = "dot"), "0.3")
})

test_that("plot_ce frontier uses only non-dominated strategies", {
  # C costs more than B and is less effective, so it is dominated.
  mean_ev <- matrix(c(0, 100, 150,
                      0.0, 0.7, 0.5), ncol = 2,
                    dimnames = list(c("A", "B", "C"), c("cost", "utility")))
  icers <- calculate_icers(mean_ev)
  expect_equal(icers$status[icers$decision == "C"], "D")

  sim_ev <- array(rep(as.vector(mean_ev), 3), dim = c(3, 2, 3),
                  dimnames = list(c("A", "B", "C"), c("cost", "utility"), NULL))
  p <- plot_ce(sim_ev, mean_ev = mean_ev, frontier = TRUE)
  expect_s3_class(p, "ggplot")

  # The frontier line layer should contain only the two non-dominated points.
  layer_data <- lapply(p$layers, function(l) tryCatch(l$data, error = function(e) NULL))
  frontier <- Filter(function(d) is.data.frame(d) && all(c("effect", "cost") %in% names(d)) &&
                       nrow(d) == 2, layer_data)
  expect_true(length(frontier) >= 1)
})
