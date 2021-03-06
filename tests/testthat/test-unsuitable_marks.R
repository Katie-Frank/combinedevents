context("possible marks for scores()")

test_that("print message for unsuitable marks", {
  expect_message(scores(c(`100m` = 18.9), "male"),
                 '100m time must be <= 17.83 seconds to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(LJ = 2.01), "male"),
                 'LJ distance must be >= 2.25m to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(SP = 1.5), "male"),
                 "SP distance must be >= 1.53m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(HJ = 0.61), "male"),
                 "HJ height must be >= 0.77m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`400m` = 90.00), "male"),
                 "400m time must be <= 81.21 seconds to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`110mH` = 30.89), "male"),
                 "110mH time must be <= 28.09 seconds to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(DT = 4), "male"),
                 "DT distance must be >= 4.10m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(PV = .99), "male"),
                 "PV height must be >= 1.03m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(JT = 7), "male"),
                 "JT distance must be >= 7.12m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`1500m` = 480), "male"),
                 "1500m time must be <= 7:54.11 minutes to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`200m` = 41.12), "male"),
                 '200m time must be <= 37.62 seconds to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(`60m` = 11.40), "male"),
                        '60m time must be <= 11.39 seconds to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(`60mH` = 16), "male"),
                 '60mH time must be <= 15.29 seconds to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(`1000m` = 360), "male"),
                 "1000m time must be <= 5:01.75 minutes to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`100mH` = 27), "female"),
                 "100mH time must be <= 26.40 seconds to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(HJ = .75), "female"),
                 "HJ height must be >= 0.76m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(SP = 1.49), "female"),
                 "SP distance must be >= 1.53m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`200m` = 43.01), "female"),
                 '200m time must be <= 42.08 seconds to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(LJ = 2.00), "female"),
                 'LJ distance must be >= 2.14m to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(JT = 3.80), "female"),
                 "JT distance must be >= 3.87m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`800m` = 301.10), "female"),
                 "800m time must be <= 4:10.79 minutes to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`100m` = 21), "female"),
                 '100m time must be <= 20.79 seconds to receive a positive score',
                 fixed = TRUE)
  expect_message(scores(c(DT = 3.02), "female"),
                 "DT distance must be >= 3.11m to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(PV = 1), "female"),
                 "PV height must be >= 1.02m to receive positive score",
                 fixed = TRUE)
  expect_message(scores(c(`400m` = 91), "female"),
                 "400m time must be <= 90.85 seconds to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`1500m` = 540), "female"),
                 "1500m time must be <= 8:48.40 minutes to receive a positive score",
                 fixed = TRUE)
  expect_message(scores(c(`60mH` = 17), "female"),
                 '60mH time must be <= 16.80 seconds to receive a positive score',
                 fixed = TRUE)
})

