test_that("plots works", {
  # Test 1 - test factor independent variable and numeric dependent variable
  expect_s3_class(quick_plot(x = factor(c(0, 1, 1, 0)), y = c(123, 323, 232, 123)), "ggplot")

  # Test 2 - test numeric independent variable and numeric dependent variable, with third confounding/interaction term
  expect_s3_class(quick_plot(x = c(12, 42, 23, 25), y = c(123, 323, 232, 123), interaction_variable = factor(c(0, 1, 1, 0))), "ggplot")

  # Test 3 - test warning when axis label is not a character
  expect_warning(quick_plot(x = factor(c(0, 1, 1, 0)), y = c(123, 323, 232, 123), xlab = FALSE), "'xlab' should be of class 'character'!")

  # Test 4 - test error when dependent variable is a factor
  expect_error(quick_plot(x = factor(c(0, 1, 1, 0)), y = factor(c(0, 1, 1, 0))))
})
