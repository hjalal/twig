# check_twig() is an internal validator; it is reachable directly from the
# package's own test environment.

test_that("a well-formed Markov twig passes validation", {
  obj <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pDie, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(cost, utility))
  )
  expect_no_error(suppressMessages(check_twig(obj)))
})

test_that("duplicate payoff names are rejected (issue #1)", {
  obj <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pDie, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(residence, residence))
  )
  expect_error(suppressMessages(check_twig(obj)), "Duplicate payoff names")
})

test_that("duplicate decision names are rejected", {
  obj <- suppressMessages(
    twig() +
      decisions(names = c(A, A)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0)) +
      event(name = death_event, options = c(yes, none),
            probs = c(pDie, leftover), transitions = c(Dead, stay)) +
      payoffs(names = c(cost))
  )
  expect_error(suppressMessages(check_twig(obj)), "Duplicate names")
})

test_that("an event without exactly one 'none' is rejected", {
  obj <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      event(name = ev, options = c(yes, maybe),
            probs = c(pYes, leftover), transitions = c(win, lose)) +
      payoffs(names = c(cost))
  )
  expect_error(suppressMessages(check_twig(obj)), "none")
})

test_that("more than one 'leftover' in an event is rejected", {
  obj <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      event(name = ev, options = c(yes, no, none),
            probs = c(pYes, leftover, leftover), transitions = c(win, draw, lose)) +
      payoffs(names = c(cost))
  )
  expect_error(suppressMessages(check_twig(obj)), "leftover")
})
