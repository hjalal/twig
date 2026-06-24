test_that("%out% is the negation of %in%", {
  expect_equal(c("A", "B", "C") %out% c("B", "C", "D"), c(TRUE, FALSE, FALSE))
  expect_equal(1:3 %out% 2:5, c(TRUE, FALSE, FALSE))
  expect_equal(letters[1:3] %out% letters, c(FALSE, FALSE, FALSE))
  expect_identical("a" %out% character(0), TRUE)
})
