# Regression tests: a Markov model must run even when no parameter is used by
# any function (so there is no "sim" dimension in the internal arg sizes).

test_that("a Markov model with unused parameters runs and is correct", {
  local_twig_funs(
    pDie    = function(state) 0 * (state == "Alive"),   # no parameter used
    cost    = function(decision) 10 + 0 * (decision == "A"),
    utility = function(state) 1 * (state == "Alive")
  )
  mod <- build_markov_model()

  res <- suppressMessages(run_twig(mod, params = list(unused = 1), n_cycles = 5,
                                   parallel = FALSE, progress_bar = FALSE))
  expect_false(anyNA(res$mean_ev))
  expect_equal(unname(res$mean_ev["A", "cost"]), 50)     # 10 per cycle, no deaths
  expect_equal(unname(res$mean_ev["A", "utility"]), 5)
})

test_that("a tunnel-state model runs without used parameters", {
  local_twig_funs(
    pSick = function(state) 0.1 * (state == "H"),
    pDie  = function(state, cycle_in_state) 0.1 * (state == "S") * cycle_in_state,
    cost  = function(state) 1 * (state != "D")
  )
  mod <- suppressMessages(
    twig() +
      decisions(names = c(A)) +
      states(names = c(H, S, D), init_probs = c(1, 0, 0), max_cycles = c(1, 2, 1)) +
      event(name = ev, options = c(sick, die, none),
            probs = c(pSick, pDie, leftover), transitions = c(S, D, stay)) +
      payoffs(names = c(cost))
  )
  res <- suppressMessages(run_twig(mod, params = list(unused = 1), n_cycles = 5,
                                   parallel = FALSE, progress_bar = FALSE))
  expect_false(anyNA(res$mean_ev))
})

test_that("a decision-dependent initial-probability function runs without parameters", {
  local_twig_funs(
    initH = function(decision) 0.9 + 0 * (decision == "A"),
    pDie  = function(state) 0.1 * (state == "H"),
    cost  = function(state) 1 * (state == "H")
  )
  mod <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(H, D), init_probs = c(initH, leftover)) +
      event(name = ev, options = c(die, none),
            probs = c(pDie, leftover), transitions = c(D, stay)) +
      payoffs(names = c(cost))
  )
  expect_no_warning(
    res <- suppressMessages(run_twig(mod, params = list(unused = 1), n_cycles = 3,
                                     parallel = FALSE, progress_bar = FALSE))
  )
  expect_false(anyNA(res$mean_ev))
})
