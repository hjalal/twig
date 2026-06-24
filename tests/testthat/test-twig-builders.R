test_that("twig() creates a decision_twig object", {
  obj <- twig()
  expect_s3_class(obj, "decision_twig")
  expect_s3_class(obj, "twig_class")
})

test_that("adding a states layer switches the model to a markov_twig", {
  obj <- suppressMessages(
    twig() +
      decisions(names = c(A, B)) +
      states(names = c(Alive, Dead), init_probs = c(1, 0))
  )
  expect_s3_class(obj, "markov_twig")
  expect_false(inherits(obj, "decision_twig"))
})

test_that("decisions() captures unquoted and quoted names identically", {
  expect_equal(decisions(names = c(A, B, C))$decisions, c("A", "B", "C"))
  expect_equal(decisions(names = c("A", "B", "C"))$decisions, c("A", "B", "C"))
})

test_that("event() stores options, probs and transitions", {
  ev <- event(name = death_event,
              options = c(yes, none),
              probs = c(pDie, leftover),
              transitions = c(Dead, stay))
  expect_equal(ev$type, "event")
  expect_equal(ev$event, "death_event")
  expect_equal(ev$options, c("yes", "none"))
  expect_equal(ev$probs, c("pDie", "leftover"))
  expect_equal(ev$transitions, c("Dead", "stay"))
})

test_that("payoffs() defaults discount rates to zero", {
  p <- payoffs(names = c(cost, utility))
  expect_equal(p$payoffs, c("cost", "utility"))
  expect_equal(unname(p$discount_rates), c(0, 0))

  p2 <- payoffs(names = c(cost, utility), discount_rates = c(0.03, 0.03))
  expect_equal(unname(p2$discount_rates), c(0.03, 0.03))
})

test_that("states() rejects 'stay' as a state name", {
  expect_error(states(names = c(Alive, stay), init_probs = c(1, 0)), "stay")
})

test_that("states() checks init_probs length", {
  expect_error(states(names = c(A, B, C), init_probs = c(1, 0)), "init_probs")
})
