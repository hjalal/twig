test_that("rate2prob and prob2rate use the correct formulas", {
  expect_equal(rate2prob(0.1), 1 - exp(-0.1))
  expect_equal(prob2rate(0.1), -log(1 - 0.1))
  expect_equal(rate2prob(0), 0)
  expect_equal(prob2rate(0), 0)
})

test_that("rate2prob and prob2rate invert each other", {
  expect_equal(rate2prob(prob2rate(0.3)), 0.3)
  expect_equal(prob2rate(rate2prob(0.7)), 0.7)
})

test_that("rate2prob and prob2rate are vectorized", {
  expect_length(rate2prob(c(0.1, 0.2, 0.3)), 3)
  expect_length(prob2rate(c(0.1, 0.2)), 2)
})

test_that("rate2prob and prob2rate validate their inputs", {
  expect_true(is.infinite(prob2rate(1)))
  expect_error(rate2prob(-1), "non-negative")
  expect_error(prob2rate(1.5), "\\[0, 1\\]")
  expect_error(prob2rate(-0.1), "\\[0, 1\\]")
  expect_error(rate2prob("x"), "numeric")
  expect_error(prob2rate("x"), "numeric")
})
