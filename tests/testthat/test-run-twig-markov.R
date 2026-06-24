test_that("a deterministic Markov model returns exact expected values", {
  local_twig_funs(
    pDie    = function(state, p) p * (state == "Alive"),
    cost    = function(decision) 10 + 0 * (decision == "A"),
    utility = function(state) 1 * (state == "Alive")
  )
  mod <- build_markov_model()

  res <- suppressMessages(run_twig(mod, params = list(p = 0), n_cycles = 5,
                                   parallel = FALSE, progress_bar = FALSE))

  # With no deaths: cost = 10 per cycle, utility = 1 per cycle, over 5 cycles.
  expect_equal(unname(res$mean_ev["A", "cost"]), 50)
  expect_equal(unname(res$mean_ev["A", "utility"]), 5)
  expect_equal(unname(res$mean_ev["B", "cost"]), 50)
  expect_equal(unname(res$mean_ev["B", "utility"]), 5)
})

test_that("run_twig returns the documented result structure", {
  local_twig_funs(
    pDie    = function(state, p) p * (state == "Alive"),
    cost    = function(decision) 10 + 0 * (decision == "A"),
    utility = function(state) 1 * (state == "Alive")
  )
  res <- suppressMessages(run_twig(build_markov_model(), params = list(p = 0.1),
                                   n_cycles = 5, parallel = FALSE, progress_bar = FALSE))

  expect_named(res, c("mean_ev", "sim_ev"), ignore.order = TRUE)
  expect_equal(unname(dim(res$mean_ev)), c(2, 2))
  expect_equal(dimnames(res$mean_ev)[[1]], c("A", "B"))
  expect_equal(unname(dim(res$sim_ev)), c(2, 2, 1))
})

test_that("higher mortality lowers life-years (utility)", {
  local_twig_funs(
    pDie    = function(state, p) p * (state == "Alive"),
    cost    = function(decision) 10 + 0 * (decision == "A"),
    utility = function(state) 1 * (state == "Alive")
  )
  mod <- build_markov_model()
  lo <- suppressMessages(run_twig(mod, params = list(p = 0.05), n_cycles = 10,
                                  parallel = FALSE, progress_bar = FALSE))
  hi <- suppressMessages(run_twig(mod, params = list(p = 0.30), n_cycles = 10,
                                  parallel = FALSE, progress_bar = FALSE))
  expect_lt(hi$mean_ev["A", "utility"], lo$mean_ev["A", "utility"])
})

test_that("run_twig is deterministic for identical inputs", {
  local_twig_funs(
    pDie    = function(state, p) p * (state == "Alive"),
    cost    = function(decision) 10 + 0 * (decision == "A"),
    utility = function(state) 1 * (state == "Alive")
  )
  mod <- build_markov_model()
  r1 <- suppressMessages(run_twig(mod, params = list(p = 0.1), n_cycles = 8,
                                  parallel = FALSE, progress_bar = FALSE))
  r2 <- suppressMessages(run_twig(mod, params = list(p = 0.1), n_cycles = 8,
                                  parallel = FALSE, progress_bar = FALSE))
  expect_equal(r1$mean_ev, r2$mean_ev)
})
