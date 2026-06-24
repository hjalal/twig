test_that("calculate_icers computes the ICER for two non-dominated strategies", {
  m <- matrix(c(100, 200, 0.5, 0.7), ncol = 2,
              dimnames = list(c("A", "B"), c("cost", "utility")))
  icers <- calculate_icers(m)

  expect_s3_class(icers, "icers")
  expect_equal(nrow(icers), 2)
  expect_true(all(icers$status == "ND"))
  # B is more costly and more effective: ICER = (200-100)/(0.7-0.5) = 500
  expect_equal(icers$ICER[icers$decision == "B"], 500)
  expect_true(is.na(icers$ICER[icers$decision == "A"]))
})

test_that("calculate_icers handles a single strategy with the documented schema", {
  m <- matrix(c(100, 0.5), ncol = 2,
              dimnames = list("A", c("cost", "utility")))
  icers <- calculate_icers(m)
  expect_equal(nrow(icers), 1)
  expect_true(is.na(icers$ICER))
  # The single-strategy result must use the same schema and class as the
  # multi-strategy result.
  expect_s3_class(icers, "icers")
  expect_named(icers, c("decision", "cost", "utility", "inc_cost",
                        "inc_utility", "ICER", "status"))
  expect_equal(icers$status, "ND")
  expect_equal(icers$decision, "A")
})

test_that("calculate_icers single-strategy respects custom column names", {
  m <- matrix(c(100, 0.5), ncol = 2,
              dimnames = list("A", c("cost", "qaly")))
  icers <- calculate_icers(m, col_names = c("cost", "qaly"))
  expect_named(icers, c("decision", "cost", "qaly", "inc_cost",
                        "inc_qaly", "ICER", "status"))
})

test_that("calculate_icers flags a strongly dominated strategy", {
  # C costs more than B but is less effective -> dominated.
  m <- matrix(c(0, 100, 150,
                0.0, 0.7, 0.5), ncol = 2,
              dimnames = list(c("A", "B", "C"), c("cost", "utility")))
  icers <- calculate_icers(m)
  expect_equal(icers$status[icers$decision == "C"], "D")
})

test_that("calculate_icers does not produce NaN when two strategies share an effect", {
  m <- matrix(c(100, 200, 0.5, 0.5), ncol = 2,
              dimnames = list(c("X", "Y"), c("cost", "utility")))
  icers <- calculate_icers(m)
  expect_false(any(is.nan(icers$ICER)))
})

test_that("calculate_icers respects custom column names", {
  m <- matrix(c(100, 200, 0.5, 0.7), ncol = 2,
              dimnames = list(c("A", "B"), c("cost", "qaly")))
  icers <- calculate_icers(m, col_names = c("cost", "qaly"))
  expect_true("qaly" %in% names(icers))
})
