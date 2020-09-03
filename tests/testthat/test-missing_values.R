context("Missing values when combined_event is not null")

test_that("missing marks return NA for scores() when combined_event is not null", {
  expect_equal(scores(c(10, rep(NA, 9)), "male", "decathlon")$scores,
               c(`100m` = 1096, LJ = NA, SP = NA, HJ = NA, `400m` = NA,
                 `110mH` = NA, DT = NA, PV = NA, JT = NA, `1500m` = NA))
  expect_equal(scores(c(SP = 14.01), "male", "decathlon")$scores,
               c(`100m` = NA, LJ = NA, SP = 729, HJ = NA, `400m` = NA,
                 `110mH` = NA, DT = NA, PV = NA, JT = NA, `1500m` = NA))
  expect_equal(scores(c(rep(NA, 4), `200m` = 24.56), "male", "outdoor pentathlon")$scores,
               c(LJ = NA, JT = NA, `200m` = 644, DT = NA, `1500m` = NA))
  expect_equal(scores(c(`1500m` = "4:30.01"), "male", "outdoor pentathlon")$scores,
               c(LJ = NA, JT = NA, `200m` = NA, DT = NA, `1500m` = 745))
  expect_equal(scores(c(`60mH` = 8.21, rep(NA, 6)), "male", "heptathlon")$scores,
               c(`60m` = NA, LJ = NA, SP = NA, HJ = NA, `60mH` = 930, PV = NA, `1000m`= NA))
  expect_equal(scores(c(HJ = 2), "male", "heptathlon")$scores,
               c(`60m` = NA, LJ = NA, SP = NA, HJ = 803, `60mH` = NA, PV = NA, `1000m`= NA))
  expect_equal(scores(c(`1000m` = "2:45.79", rep(NA, 4)), "male", "indoor pentathlon")$scores,
               c(`60mH` = NA, LJ = NA, SP = NA, HJ = NA, `1000m` = 810))
  expect_equal(scores(c(LJ = 5), "male", "indoor pentathlon")$scores,
               c(`60mH` = NA, LJ = 382, SP = NA, HJ = NA, `1000m` = NA))
  expect_equal(scores(c(JT = 43.93, rep(NA, 6)), "female", "heptathlon")$scores,
               c(`100mH` = NA, HJ = NA, SP = NA, `200m` = NA, LJ = NA, JT = 743,
                 `800m` = NA))
  expect_equal(scores(c(LJ = 6.77), "female", "heptathlon")$scores,
               c(`100mH` = NA, HJ = NA, SP = NA, `200m` = NA, LJ = 1095, JT = NA,
                 `800m` = NA))
  expect_equal(scores(c(SP = 13.86, rep(NA, 9)), "female", "decathlon")$scores,
               c(`100m` = NA, DT = NA, PV = NA, JT = NA, `400m` = NA, `100mH` = NA,
                 LJ = NA, SP = 785, HJ = NA, `1500m` = NA))
  expect_equal(scores(c(HJ = 1.95), "female", "decathlon")$scores,
               c(`100m` = NA, DT = NA, PV = NA, JT = NA, `400m` = NA, `100mH` = NA,
                 LJ = NA, SP = NA, HJ = 1171, `1500m` = NA))
  expect_equal(scores(c(`800m` = "2:07.26", rep(NA, 4)), "female", "pentathlon")$scores,
               c(`60mH` = NA, HJ = NA, SP = NA, LJ = NA, `800m` = 1005))
  expect_equal(scores(c(SP = 13.86), "female", "pentathlon")$scores,
               c(`60mH` = NA, HJ = NA, SP = 785, LJ = NA, `800m` = NA))
})

test_that("missing scores return NA for marks() when combined_event is not null", {
  expect_equal(marks(c(1096, rep(NA, 9)), "male", "decathlon")$scores,
               c(`100m` = 1096, LJ = NA, SP = NA, HJ = NA, `400m` = NA,
                 `110mH` = NA, DT = NA, PV = NA, JT = NA, `1500m` = NA))
  expect_equal(marks(c(SP = 729), "male", "decathlon")$scores,
               c(`100m` = NA, LJ = NA, SP = 729, HJ = NA, `400m` = NA,
                 `110mH` = NA, DT = NA, PV = NA, JT = NA, `1500m` = NA))
  expect_equal(marks(c(rep(NA, 4), `200m` = 644), "male", "outdoor pentathlon")$scores,
               c(LJ = NA, JT = NA, `200m` = 644, DT = NA, `1500m` = NA))
  expect_equal(marks(c(`1500m` = 745), "male", "outdoor pentathlon")$scores,
               c(LJ = NA, JT = NA, `200m` = NA, DT = NA, `1500m` = 745))
  expect_equal(marks(c(`60mH` = 930, rep(NA, 6)), "male", "heptathlon")$scores,
               c(`60m` = NA, LJ = NA, SP = NA, HJ = NA, `60mH` = 930, PV = NA, `1000m`= NA))
  expect_equal(marks(c(HJ = 803), "male", "heptathlon")$scores,
               c(`60m` = NA, LJ = NA, SP = NA, HJ = 803, `60mH` = NA, PV = NA, `1000m`= NA))
  expect_equal(marks(c(`1000m` = 810, rep(NA, 4)), "male", "indoor pentathlon")$scores,
               c(`60mH` = NA, LJ = NA, SP = NA, HJ = NA, `1000m` = 810))
  expect_equal(marks(c(LJ = 382), "male", "indoor pentathlon")$scores,
               c(`60mH` = NA, LJ = 382, SP = NA, HJ = NA, `1000m` = NA))
  expect_equal(marks(c(JT = 743, rep(NA, 6)), "female", "heptathlon")$scores,
               c(`100mH` = NA, HJ = NA, SP = NA, `200m` = NA, LJ = NA, JT = 743,
                 `800m` = NA))
  expect_equal(marks(c(LJ = 1095), "female", "heptathlon")$scores,
               c(`100mH` = NA, HJ = NA, SP = NA, `200m` = NA, LJ = 1095, JT = NA,
                 `800m` = NA))
  expect_equal(marks(c(SP = 785, rep(NA, 9)), "female", "decathlon")$scores,
               c(`100m` = NA, DT = NA, PV = NA, JT = NA, `400m` = NA, `100mH` = NA,
                 LJ = NA, SP = 785, HJ = NA, `1500m` = NA))
  expect_equal(marks(c(HJ = 1171), "female", "decathlon")$scores,
               c(`100m` = NA, DT = NA, PV = NA, JT = NA, `400m` = NA, `100mH` = NA,
                 LJ = NA, SP = NA, HJ = 1171, `1500m` = NA))
  expect_equal(marks(c(`800m` = 1005, rep(NA, 4)), "female", "pentathlon")$scores,
               c(`60mH` = NA, HJ = NA, SP = NA, LJ = NA, `800m` = 1005))
  expect_equal(marks(c(SP = 785), "female", "pentathlon")$scores,
               c(`60mH` = NA, HJ = NA, SP = 785, LJ = NA, `800m` = NA))
})



