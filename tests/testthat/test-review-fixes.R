# Regression tests for the deep-review findings.

test_that("H1: transition (event-dependent) payoffs use origin (start-of-cycle) weighting", {
  local_twig_funs(
    pDie       = function(state) 0.1 * (state == "Alive"),
    death_cost = function(death_event) 1000 * (death_event == "yes"),  # transition payoff
    alive_cost = function(state) 50 * (state == "Alive")               # occupancy payoff
  )
  mod <- twig() + decisions(names = c(A)) +
    states(names = c(Alive, Dead), init_probs = c(1, 0)) +
    event(name = death_event, options = c(yes, none),
          probs = c(pDie, leftover), transitions = c(Dead, stay)) +
    payoffs(names = c(death_cost, alive_cost))
  res <- suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 10,
                                   parallel = FALSE, progress_bar = FALSE))
  # Each death costs 1000, paid once: 1000 * P(ever die in 10 cycles) = 1000*(1-0.9^10).
  expect_equal(unname(res$mean_ev["A", "death_cost"]), 1000 * (1 - 0.9^10))
  # Occupancy payoff (end-of-cycle convention) is unchanged: 50 * sum_{c=1}^10 0.9^c.
  expect_equal(unname(res$mean_ev["A", "alive_cost"]), 50 * sum(0.9^(1:10)))
})

test_that("H2: a single-strategy decision tree with converging outcomes runs", {
  local_twig_funs(
    pSick      = function() 0.5,
    pDieIfSick = function() 0.8,
    pDieIfWell = function() 0.1,
    val        = function(outcome) 100 * (outcome == "Alive")
  )
  dt <- twig() + decisions(names = c(D)) +
    event(name = sick, options = c(yes, none),
          probs = c(pSick, leftover), transitions = c(die_sick, die_well)) +
    event(name = die_sick, options = c(yes, none),
          probs = c(pDieIfSick, leftover), transitions = c(Dead, Alive)) +
    event(name = die_well, options = c(yes, none),
          probs = c(pDieIfWell, leftover), transitions = c(Dead, Alive)) +
    payoffs(names = c(val))
  res <- suppressMessages(run_twig(dt, params = list(z = 1),
                                   parallel = FALSE, progress_bar = FALSE))
  # P(Alive) = 0.5*0.2 + 0.5*0.9 = 0.55 -> EV = 55.
  expect_equal(unname(res$mean_ev["D", "val"]), 55)
})

test_that("H3: dynamic initial probabilities must sum to 1 when there is no leftover", {
  local_twig_funs(
    initH = function(decision) 0.5, initD = function(decision) 0.3,   # sum 0.8
    pDie  = function(state) 0.1 * (state == "H"), cost = function() 1
  )
  bad <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, D), init_probs = c(initH, initD)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) + payoffs(names = c(cost)))
  expect_error(
    suppressMessages(run_twig(bad, params = list(z = 1), n_cycles = 3,
                              parallel = FALSE, progress_bar = FALSE)),
    "sum to 1")

  local_twig_funs(initH = function(decision) 0.6, initD = function(decision) 0.4)  # sum 1
  good <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, D), init_probs = c(initH, initD)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) + payoffs(names = c(cost)))
  expect_no_error(suppressMessages(run_twig(good, params = list(z = 1), n_cycles = 3,
                                            parallel = FALSE, progress_bar = FALSE)))
})

test_that("H4: a factor params column is rejected", {
  local_twig_funs(pDie = function(state, p) p * (state == "Alive"),
                  cost = function() 1)
  mod <- twig() + decisions(names = c(A)) +
    states(names = c(Alive, Dead), init_probs = c(1, 0)) +
    event(name = de, options = c(yes, none), probs = c(pDie, leftover),
          transitions = c(Dead, stay)) + payoffs(names = c(cost))
  pf <- data.frame(p = factor(c("0.1", "0.2")))
  expect_error(
    suppressMessages(run_twig(mod, params = pf, n_cycles = 3,
                              parallel = FALSE, progress_bar = FALSE)),
    "numeric")
})

test_that("M1: a state-independent Markov model runs", {
  local_twig_funs(pDie = function(cycle) 0.02 * cycle, cost = function() 100)
  mod <- twig() + decisions(names = c(A)) +
    states(names = c(Alive, Dead), init_probs = c(1, 0)) +
    event(name = de, options = c(yes, none), probs = c(pDie, leftover),
          transitions = c(Dead, stay)) + payoffs(names = c(cost))
  res <- suppressMessages(run_twig(mod, params = list(z = 1), n_cycles = 5,
                                   parallel = FALSE, progress_bar = FALSE))
  expect_false(anyNA(res$mean_ev))
})

test_that("M2: plot_ceac works for a single-strategy model", {
  sim_ev <- array(c(1000, 0.8, 1000, 0.8), dim = c(1, 2, 2),
                  dimnames = list("A", c("cost", "utility"), NULL))
  expect_s3_class(plot_ceac(sim_ev, wtp_range = seq(0, 1e5, 2e4)), "ggplot")
})

test_that("M3/L6/L7: run_twig validates n_cycles, offset_trace_cycle, and params rows", {
  local_twig_funs(pDie = function(state, p) p * (state == "Alive"), cost = function() 1)
  mk <- twig() + decisions(names = c(A)) +
    states(names = c(Alive, Dead), init_probs = c(1, 0)) +
    event(name = de, options = c(yes, none), probs = c(pDie, leftover),
          transitions = c(Dead, stay)) + payoffs(names = c(cost))
  expect_error(suppressMessages(run_twig(mk, params = list(p = 0.1), progress_bar = FALSE)),
               "n_cycles is required")
  expect_error(suppressMessages(run_twig(mk, params = list(p = 0.1), n_cycles = 2.5, progress_bar = FALSE)),
               "positive integer")
  expect_error(suppressMessages(run_twig(mk, params = list(p = 0.1), n_cycles = 5,
                                         offset_trace_cycle = 2, progress_bar = FALSE)),
               "offset_trace_cycle")
  expect_error(suppressMessages(run_twig(mk, params = data.frame(p = numeric(0)), n_cycles = 5,
                                         progress_bar = FALSE)),
               "no rows")
})

test_that("L8: draw_decision_tree keeps shared downstream events as distinct nodes", {
  # Both options of the first event lead to the same downstream event B.
  dt <- twig() + decisions(names = c(D)) +
    event(name = A, options = c(x, none), probs = c(pX, leftover),
          transitions = c(B, B)) +
    event(name = B, options = c(y, none), probs = c(pY, leftover),
          transitions = c(win, lose)) +
    payoffs(names = c(cost))
  dot <- draw_decision_tree(dt, output = "dot")
  # B must appear as two separate nodes (one per path), not collapse into one.
  n_B_nodes <- length(gregexpr('label="B"', dot, fixed = TRUE)[[1]])
  expect_equal(n_B_nodes, 2)
})
