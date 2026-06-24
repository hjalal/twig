test_that("validate_twig passes a consistent model", {
  pDie <- function(state, prob_die) prob_die * (state == "Alive")
  utility <- function(state) 1 * (state == "Alive")
  mod <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pDie, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(utility))
  )
  expect_true(suppressMessages(validate_twig(mod, params = list(prob_die = 0.1))))
})

test_that("validate_twig flags a missing function", {
  utility <- function(state) 1 * (state == "Alive")
  mod <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pMissing, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(utility))
  )
  expect_error(suppressMessages(validate_twig(mod, params = list(prob_die = 0.1))),
               "not found")
})

test_that("validate_twig flags an unexpected function argument", {
  pDie <- function(state, not_a_param) 0.1
  utility <- function(state) 1 * (state == "Alive")
  mod <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pDie, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(utility))
  )
  expect_error(suppressMessages(validate_twig(mod, params = list(z = 1))),
               "unexpected argument")
})

test_that("validate_twig accepts parameter-free functions", {
  pDie <- function(state, prob_die) prob_die * (state == "Alive")
  flatfee <- function() 1
  mod <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pDie, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(flatfee))
  )
  expect_true(suppressMessages(validate_twig(mod, params = list(prob_die = 0.1))))
})

test_that("validate_twig flags duplicate payoff names", {
  pDie <- function(state, prob_die) prob_die * (state == "Alive")
  residence <- function() 0
  mod <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pDie, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(residence, residence))
  )
  expect_error(suppressMessages(validate_twig(mod, params = list(prob_die = 0.1))),
               "Duplicate payoff")
})
