make_sim_ev <- function() {
  set.seed(1)
  array(c(rnorm(20, 1000, 50), rnorm(20, 2000, 80),
          rnorm(20, 0.8, 0.02), rnorm(20, 0.9, 0.02)),
        dim = c(2, 2, 20),
        dimnames = list(c("A", "B"), c("cost", "utility"), NULL))
}

make_trace <- function() {
  array(c(1, 0.9, 0.8, 0, 0.1, 0.2,
          1, 0.8, 0.6, 0, 0.2, 0.4),
        dim = c(3, 2, 2),
        dimnames = list(cycle = c(0, 1, 2),
                        state = c("Alive", "Dead"),
                        decision = c("A", "B")))
}

test_that("plot_ce returns a ggplot object", {
  p <- plot_ce(make_sim_ev())
  expect_s3_class(p, "ggplot")
  expect_s3_class(plot_ce(make_sim_ev(), frontier = FALSE, show_points = FALSE), "ggplot")
})

test_that("plot_ce validates its input", {
  expect_error(plot_ce(matrix(1:4, 2, 2)), "3D array")
  expect_error(plot_ce(make_sim_ev(), col_names = c("cost", "qaly")), "must be among")
})

test_that("plot_ceac returns a ggplot object", {
  p <- plot_ceac(make_sim_ev(), wtp_range = seq(0, 100000, by = 5000))
  expect_s3_class(p, "ggplot")
})

test_that("plot_trace returns a ggplot object", {
  expect_s3_class(plot_trace(make_trace()), "ggplot")
  expect_s3_class(plot_trace(list(markov_trace = make_trace())), "ggplot")
})

test_that("plot_trace validates its input", {
  expect_error(plot_trace(matrix(1:4, 2, 2)), "markov_trace")
})

test_that("draw_decision_tree generates valid Graphviz DOT", {
  dt <- twig() +
    decisions(names = c(Treat, DoNotTreat)) +
    event(name = progress, options = c(yes, none),
          probs = c(pProgress, leftover), transitions = c(sick, healthy)) +
    payoffs(names = c(cost))
  dot <- draw_decision_tree(dt, output = "dot")
  expect_type(dot, "character")
  expect_match(dot, "digraph decision_tree")
  expect_match(dot, "Treat")
  expect_match(dot, "DoNotTreat")
  expect_match(dot, "sick")
  expect_match(dot, "healthy")
})

test_that("draw_transitions generates valid Graphviz DOT from an array", {
  tp <- array(c(0.9, 0, 0.1, 1, 0.8, 0, 0.2, 1), dim = c(2, 2, 2),
              dimnames = list(state = c("Alive", "Dead"),
                              dest = c("Alive", "Dead"),
                              decision = c("A", "B")))
  dot <- draw_transitions(tp, output = "dot")
  expect_type(dot, "character")
  expect_match(dot, "digraph transitions")
  expect_match(dot, '"Alive" -> "Dead"')
  # Selecting decision B uses its 0.2 transition probability.
  expect_match(draw_transitions(tp, decision = "B", output = "dot"), "0.2")
})

test_that("draw_transitions errors on an unknown decision", {
  tp <- array(c(0.9, 0, 0.1, 1), dim = c(2, 2, 1),
              dimnames = list(state = c("Alive", "Dead"),
                              dest = c("Alive", "Dead"),
                              decision = "A"))
  expect_error(draw_transitions(tp, decision = "Nope", output = "dot"), "Unknown decision")
})

test_that("the diagrams render when DiagrammeR is available", {
  skip_if_not_installed("DiagrammeR")
  dt <- twig() +
    decisions(names = c(Treat, DoNotTreat)) +
    event(name = progress, options = c(yes, none),
          probs = c(pProgress, leftover), transitions = c(sick, healthy)) +
    payoffs(names = c(cost))
  expect_s3_class(draw_decision_tree(dt), "htmlwidget")

  tp <- array(c(0.9, 0, 0.1, 1), dim = c(2, 2, 1),
              dimnames = list(state = c("Alive", "Dead"),
                              dest = c("Alive", "Dead"),
                              decision = "A"))
  expect_s3_class(draw_transitions(tp), "htmlwidget")
})
