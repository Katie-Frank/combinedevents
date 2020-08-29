context("error")

test_that("misuses of scores()", {
  expect_error(scores(factor(c(`100m` = 10.12, LJ = 7)), "male", "decathlon"),
               "`marks` must be a numeric or character vector", fixed = TRUE)
  expect_error(scores(c(`100m` = 10.12, LJ = -7), "male", "decathlon"),
               "Invalid entry for `marks`: negative mark(s) not allowed", fixed = TRUE)
  expect_error(scores(c(`1500m` = "5:20.38"), "male", seconds = "true"),
               "Invalid entry for `seconds`", fixed = TRUE)
  expect_error(scores(c(10.12, 7), "male"),
               "Every element of `marks` must be named if `combined_event` is unspecified",
               fixed = TRUE)
  expect_error(scores(c(`100m` = 10.12, lj = 7), "male"),
               "One or more invalid names for `marks`", fixed = TRUE)
})
