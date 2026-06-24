# Regression tests for GitHub issue #1:
# "having payoff functions that don't take in parameters can cause error".

test_that("a parameter-free payoff function works in a Markov model", {
  local_twig_funs(
    pDie      = function(state, p) p * (state == "Alive"),
    utility   = function(state) 1 * (state == "Alive"),
    flatfee   = function() 100            # no arguments
  )
  mod <- twig() +
    decisions(names = c(A, B)) +
    states(names = c(Alive, Dead), init_probs = c(1, 0)) +
    event(name = death_event, options = c(yes, none),
          probs = c(pDie, leftover), transitions = c(Dead, stay)) +
    payoffs(names = c(utility, flatfee))

  res <- suppressMessages(run_twig(mod, params = list(p = 0.1), n_cycles = 5,
                                   parallel = FALSE, progress_bar = FALSE))
  # The constant fee is charged once per cycle regardless of state: 100 * 5.
  expect_equal(unname(res$mean_ev["A", "flatfee"]), 500)
  expect_false(anyNA(res$mean_ev))
})

test_that("a parameter-free payoff function works in a decision tree", {
  local_twig_funs(
    pYes      = function() 0.5,
    residence = function() 0             # no arguments (the reported case)
  )
  dt <- twig() +
    decisions(names = c(A, B)) +
    event(name = ev, options = c(yes, none),
          probs = c(pYes, leftover), transitions = c(win, lose)) +
    payoffs(names = c(residence))

  res <- suppressMessages(run_twig(dt, params = list(z = 1),
                                   parallel = FALSE, progress_bar = FALSE))
  expect_false(anyNA(res$mean_ev))
  expect_true(all(res$mean_ev == 0))
})

test_that("a parameter-free probability function works", {
  local_twig_funs(
    pConst = function() 0.5,             # no arguments
    val    = function(outcome) 10 * (outcome == "win")
  )
  dt <- twig() +
    decisions(names = c(A, B)) +
    event(name = ev, options = c(yes, none),
          probs = c(pConst, leftover), transitions = c(win, lose)) +
    payoffs(names = c(val))

  res <- suppressMessages(run_twig(dt, params = list(z = 1),
                                   parallel = FALSE, progress_bar = FALSE))
  # P(win) = 0.5 for both decisions -> EV = 5.
  expect_equal(unname(res$mean_ev["A", "val"]), 5)
})

test_that("the exact reported model now fails with a clear message", {
  # The original report listed the same payoff twice, which silently produced
  # NA results. It must now raise an informative error.
  local_twig_funs(
    pStart    = function(state, prob_start) ifelse(state == "Never", prob_start, 0),
    pQuit     = function(state, prob_quit) ifelse(state == "Current", prob_quit, 0),
    residence = function() 0
  )
  mytwig <- suppressMessages(
    twig() +
      decisions(names = c(statusquo, statusquo1)) +
      states(names = c(Current, Former, Never), init_probs = c(0.1666, 0.0639, 0.7695)) +
      event(name = transition, options = c(start, quit, none),
            probs = c(pStart, pQuit, leftover), transitions = c(Current, Former, stay)) +
      payoffs(names = c(residence, residence))
  )
  expect_error(
    suppressMessages(run_twig(mytwig, params = list(prob_start = 0.1, prob_quit = 0.1),
                              n_cycles = 20, parallel = FALSE, progress_bar = FALSE)),
    "Duplicate payoff names"
  )
})
