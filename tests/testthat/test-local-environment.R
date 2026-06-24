# run_twig() must find user functions in the calling environment, not only in
# the global environment.

test_that("model functions are found in a local calling environment", {
  # Define everything as local variables whose names do not exist globally.
  locDie  <- function(state, p) p * (state == "Alive")
  locCost <- function(decision) 10 + 0 * (decision == "A")
  locUtil <- function(state) 1 * (state == "Alive")
  expect_false(exists("locDie", envir = globalenv()))

  mod <- suppressMessages(
    twig() + decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = de, options = c(yes, none), probs = c(locDie, leftover),
            transitions = c(Dead, stay)) +
      payoffs(names = c(locCost, locUtil))
  )
  res <- suppressMessages(run_twig(mod, params = list(p = 0), n_cycles = 5,
                                   parallel = FALSE, progress_bar = FALSE))
  expect_equal(unname(res$mean_ev["A", "locCost"]), 50)
  expect_equal(unname(res$mean_ev["A", "locUtil"]), 5)
})

test_that("validate_twig and run_twig agree on a local environment", {
  vpDie <- function(state, p) p * (state == "Alive")
  vUtil <- function(state) 1 * (state == "Alive")
  mod <- suppressMessages(
    twig() + decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = de, options = c(yes, none), probs = c(vpDie, leftover),
            transitions = c(Dead, stay)) +
      payoffs(names = c(vUtil))
  )
  expect_true(suppressMessages(validate_twig(mod, params = list(p = 0.1))))
  expect_no_error(suppressMessages(run_twig(mod, params = list(p = 0.1), n_cycles = 3,
                                            parallel = FALSE, progress_bar = FALSE)))
})
