# parallel = TRUE must produce the same results as parallel = FALSE. These tests
# run for real whenever the workers can load an installed copy of the package
# (CI and R CMD check); they skip only when that is impossible (for example
# under devtools::load_all()). A genuine parallel-backend failure therefore
# surfaces as a test failure, not a silent skip.

test_that("a parallel Markov run matches the sequential run", {
  skip_if_no_parallel_twig()
  local_twig_funs(
    pDie    = function(state, p) p * (state == "Alive"),
    cost    = function(decision) 10 + 0 * (decision == "A"),
    utility = function(state) 1 * (state == "Alive")
  )
  mod <- build_markov_model()
  psa <- data.frame(p = c(0, 0.05, 0.1, 0.2))

  seqv <- suppressMessages(run_twig(mod, params = psa, n_cycles = 8,
                                    parallel = FALSE, progress_bar = FALSE))
  parv <- suppressMessages(run_twig(mod, params = psa, n_cycles = 8,
                                    parallel = TRUE, ncore = 2, progress_bar = FALSE))
  expect_equal(parv$mean_ev, seqv$mean_ev)
})

test_that("a parallel decision-tree run matches the sequential run", {
  skip_if_no_parallel_twig()
  local_twig_funs(
    pYes = function(decision, base) base * (decision == "A") + 0.3 * (decision == "B"),
    val  = function(outcome) 100 * (outcome == "win")
  )
  dt <- twig() +
    decisions(names = c(A, B)) +
    event(name = ev, options = c(yes, none),
          probs = c(pYes, leftover), transitions = c(win, lose)) +
    payoffs(names = c(val))
  psa <- data.frame(base = c(0.4, 0.5, 0.6, 0.55))

  seqv <- suppressMessages(run_twig(dt, params = psa, parallel = FALSE, progress_bar = FALSE))
  parv <- suppressMessages(run_twig(dt, params = psa, parallel = TRUE, ncore = 2, progress_bar = FALSE))
  expect_equal(parv$mean_ev, seqv$mean_ev)
})
