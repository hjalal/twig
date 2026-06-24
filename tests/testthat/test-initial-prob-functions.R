# Initial-probability functions may depend on the decision and/or parameters.

test_that("a parameter-only initial-probability function gives correct values", {
  local_twig_funs(
    p0H  = function(start_well) start_well,
    pDie = function(state) 0.1 * (state == "H"),
    cost = function() 1
  )
  mod <- suppressMessages(
    twig() + decisions(names = c(A, B)) +
      states(names = c(H, D), init_probs = c(p0H, leftover)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) +
      payoffs(names = c(cost))
  )
  res <- suppressMessages(run_twig(mod, params = list(start_well = 0.8), n_cycles = 3,
                                   verbose = TRUE, parallel = FALSE, progress_bar = FALSE))
  # Cycle 0 of the trace is the initial distribution: H = 0.8, D = 0.2.
  expect_equal(unname(res$markov_trace[1, , 1]), c(0.8, 0.2))
})

test_that("a decision-dependent initial-probability function gives correct values", {
  local_twig_funs(
    p0H  = function(decision) ifelse(decision == "A", 0.9, 0.7),
    pDie = function(state) 0.1 * (state == "H"),
    cost = function() 1
  )
  mod <- suppressMessages(
    twig() + decisions(names = c(A, B)) +
      states(names = c(H, D), init_probs = c(p0H, leftover)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) +
      payoffs(names = c(cost))
  )
  res <- suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 3,
                                   verbose = TRUE, parallel = FALSE, progress_bar = FALSE))
  expect_equal(unname(res$markov_trace[1, , "A"]), c(0.9, 0.1))
  expect_equal(unname(res$markov_trace[1, , "B"]), c(0.7, 0.3))
})
