# A health-economic model must not silently run invalid probability simplexes.

test_that("a probability function returning a value above 1 is rejected", {
  local_twig_funs(
    pBad = function(state) 1.2 * (state == "Alive"),
    cost = function() 1
  )
  mod <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = ev, options = c(die, none), probs = c(pBad, leftover),
            transitions = c(Dead, stay)) +
      payoffs(names = c(cost))
  )
  expect_error(
    suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 3,
                              parallel = FALSE, progress_bar = FALSE)),
    "\\[0, 1\\]"
  )
})

test_that("a non-finite probability (NA/NaN/Inf) is rejected", {
  local_twig_funs(
    pBad = function(state) ifelse(state == "Alive", NA_real_, 0),
    cost = function() 1
  )
  mod <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = ev, options = c(die, none), probs = c(pBad, leftover),
            transitions = c(Dead, stay)) +
      payoffs(names = c(cost))
  )
  expect_error(
    suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 3,
                              parallel = FALSE, progress_bar = FALSE)),
    "non-finite|finite"
  )
})

test_that("a valid probability simplex runs without a false positive", {
  local_twig_funs(
    pA   = function(state) 0.3 * (state == "H"),
    pB   = function(state) 0.2 * (state == "H"),
    cost = function() 1
  )
  mod <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, S, D), init_probs = c(1, 0, 0)) +
      event(name = ev, options = c(sick, die, none), probs = c(pA, pB, leftover),
            transitions = c(S, D, stay)) +
      payoffs(names = c(cost))
  )
  res <- suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 3,
                                   parallel = FALSE, progress_bar = FALSE))
  expect_false(anyNA(res$mean_ev))
})

test_that("initial probabilities summing above 1 are rejected", {
  local_twig_funs(
    pDie = function(state) 0.1 * (state == "H"),
    cost = function() 1
  )
  mod <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, S, D), init_probs = c(0.7, 0.7, leftover)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) +
      payoffs(names = c(cost))
  )
  expect_error(
    suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 3,
                              parallel = FALSE, progress_bar = FALSE)),
    "init_probs"
  )
})
