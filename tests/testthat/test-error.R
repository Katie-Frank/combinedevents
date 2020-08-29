context("error")

test_that("misuses of scores()", {
  expect_error(scores(factor(c(`100m` = 10.12, LJ = 7)), "male", "decathlon"),
               "`marks` must be a numeric or character vector", fixed = TRUE)
  expect_error(scores(c(`100m` = 10.12, LJ = -7), "male", "decathlon"),
               "Invalid entry for `marks`: negative mark(s) not allowed", fixed = TRUE)
})
