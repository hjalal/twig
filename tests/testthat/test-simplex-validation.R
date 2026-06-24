# Statically-checkable probability vectors (no function names) must be valid
# simplexes. Vectors that reference functions are left to run-time evaluation.

test_that("numeric initial probabilities without leftover must sum to 1", {
  obj <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, S, D), init_probs = c(0.7, 0.7, 0)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) +
      payoffs(names = c(cost))
  )
  expect_error(check_simplexes(obj), "must sum to 1")
})

test_that("negative initial probabilities are rejected", {
  obj <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, D), init_probs = c(-0.1, 1.1)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) +
      payoffs(names = c(cost))
  )
  expect_error(check_simplexes(obj), "cannot be negative")
})

test_that("a valid numeric initial distribution passes", {
  obj <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, D), init_probs = c(0.6, 0.4)) +
      event(name = ev, options = c(die, none), probs = c(pDie, leftover),
            transitions = c(D, stay)) +
      payoffs(names = c(cost))
  )
  expect_no_error(check_simplexes(obj))
})

test_that("numeric event probabilities summing above 1 are rejected", {
  obj <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, S, D), init_probs = c(1, 0, 0)) +
      event(name = ev, options = c(sick, die, none), probs = c(0.7, 0.7, leftover),
            transitions = c(S, D, stay)) +
      payoffs(names = c(cost))
  )
  expect_error(check_simplexes(obj), "more than 1")
})

test_that("event probabilities that reference functions are not statically rejected", {
  # These would sum above 1 at run time, but cannot be checked statically, so
  # check_simplexes() must not error on them.
  obj <- suppressMessages(
    twig() + decisions(names = c(A)) +
      states(names = c(H, S, D), init_probs = c(1, 0, 0)) +
      event(name = ev, options = c(sick, die, none), probs = c(pA, pB, leftover),
            transitions = c(S, D, stay)) +
      payoffs(names = c(cost))
  )
  expect_no_error(check_simplexes(obj))
})
