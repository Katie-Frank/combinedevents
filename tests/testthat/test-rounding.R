context("rounding")

test_that("rounding of scores()", {
  expect_message(scores(c(`100m` = 10.822, LJ = 7.1), "male"),
                 "One or more entries of `marks` have been rounded to the second decimal place",
                 fixed = TRUE)
  expect_equal(scores(c(`100m` = 10.822, LJ = 7.1), "male")$marks, c(`100m` = 10.82, LJ = 7.1))
})

test_that("rounding of marks()", {
  expect_message(marks(c(`100m` = 851.4, LJ = 700), "male"),
                 "One or more entries of `scores` have been rounded to the nearest integer",
                 fixed = TRUE)
})
