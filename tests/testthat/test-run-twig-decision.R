test_that("a deterministic decision tree returns the exact expected value", {
  local_twig_funs(
    pYes = function() 0.5,
    val  = function(outcome) 100 * (outcome == "win") + 20 * (outcome == "lose")
  )
  dt <- twig() +
    decisions(names = c(A)) +
    event(name = ev, options = c(yes, none),
          probs = c(pYes, leftover), transitions = c(win, lose)) +
    payoffs(names = c(val))

  res <- suppressMessages(run_twig(dt, params = list(z = 1),
                                   parallel = FALSE, progress_bar = FALSE))
  # 0.5 * 100 + 0.5 * 20 = 60
  expect_equal(unname(res$mean_ev["A", "val"]), 60)
})

test_that("decision-tree probabilities and payoffs respond to the decision", {
  local_twig_funs(
    pYes = function(decision, base) base * (decision == "Treat") + 0.2 * (decision == "Watch"),
    val  = function(outcome) 100 * (outcome == "win")
  )
  dt <- twig() +
    decisions(names = c(Treat, Watch)) +
    event(name = ev, options = c(yes, none),
          probs = c(pYes, leftover), transitions = c(win, lose)) +
    payoffs(names = c(val))

  res <- suppressMessages(run_twig(dt, params = list(base = 0.6),
                                   parallel = FALSE, progress_bar = FALSE))
  # Treat: 0.6 * 100 = 60 ; Watch: 0.2 * 100 = 20
  expect_equal(unname(res$mean_ev["Treat", "val"]), 60)
  expect_equal(unname(res$mean_ev["Watch", "val"]), 20)
})
