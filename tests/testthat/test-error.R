context("error")

test_that("misuses of scores()", {
  expect_error(scores(list(c(`100m` = 10.12, LJ = 7)), "male", "decathlon"),
               "`marks` must be a numeric or character vector", fixed = TRUE)
  expect_error(scores(c(`100m` = 10.12, LJ = -7), "male", "decathlon"),
               "Invalid entry for `marks`: negative mark(s) not allowed", fixed = TRUE)
  expect_error(scores(c(`1500m` = "5:20.38"), "male", seconds = "true"),
               "Invalid entry for `seconds`", fixed = TRUE)
  expect_error(scores(c(10.12, 7), "male"),
               "Elements of `marks` must be named if `combined_event` is unspecified",
               fixed = TRUE)
  expect_error(scores(c(`100m` = 10.12, lj = 7), "male"),
               "One or more invalid names for `marks`", fixed = TRUE)
  expect_error(scores(c(10.12, 7), "male", "Decathlon"),
               "Invalid entry for `combined_event`", fixed = TRUE)
  expect_error(scores(c(10.12, 7), "female"),
               "Elements of `marks` must be named if `combined_event` is unspecified",
               fixed = TRUE)
  expect_error(scores(c(`100m` = 12.2, 6.7), "female"),
               "One or more invalid names for `marks`", fixed = TRUE)
  expect_error(scores(c(10.12, 7), "female", "Heptathlon"),
               "Invalid entry for `combined_event`", fixed = TRUE)
})

test_that("misuses of marks()", {
  expect_error(marks(c(`100m` = "900", LJ = 850), "male", "decathlon"),
               "`scores` must be a numeric vector", fixed = TRUE)
  expect_error(marks(c(`100m` = -900, LJ = 850), "male", "decathlon"),
               "Invalid entry for `scores`: negative score(s) not allowed", fixed = TRUE)
  expect_error(marks(c(`100m` = 900, LJ = 850), "male", "decathlon", seconds = "true"),
               "Invalid entry for `seconds`", fixed = TRUE)
  expect_error(marks(c(900, 850), "male"),
               "Elements of `scores` must be named if `combined_event` is unspecified",
               fixed = TRUE)
  expect_error(marks(c(`100m` = 900, 850), "male"),
               "One or more invalid names for `scores`", fixed = TRUE)
  expect_error(marks(c(900, 850), "male", "DECATHLON"),
               "Invalid entry for `combined_event`", fixed = TRUE)
  expect_error(marks(c(900, 850), "female"),
               "Elements of `scores` must be named if `combined_event` is unspecified",
               fixed = TRUE)
  expect_error(marks(c(`110mH` = 900, LJ = 850), "female"),
               "One or more invalid names for `scores`", fixed = TRUE)
})
