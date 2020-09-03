context("expectations for marks()")

test_that("results of marks()", {
  expect_equal(marks(c(906, 838, 606, 850, 838, 911, 564,
              790, 442, 805), "male", "decathlon")$scores,
  c(`100m` = 906, LJ = 838, SP = 606, HJ = 850, `400m` = 838, `110mH` = 911, DT = 564,
    PV = 790, JT = 442, `1500m` = 805))
  expect_equal(marks(c(838, 442, 747, 564, 805), "male", "outdoor pentathlon")$scores,
               c(LJ = 838, JT = 442, `200m` = 747, DT = 564, `1500m` = 805))
  expect_equal(marks(c(886, 838, 606, 850, 898, 790, 887), "male", "heptathlon")$scores,
               c(`60m` = 886, LJ = 838, SP = 606, HJ = 850, `60mH` = 898, PV = 790,
                 `1000m` = 887))
  expect_equal(marks(c(898, 838, 606, 850, NA), "male", "indoor pentathlon")$scores,
               c(`60mH` = 898, LJ = 838, SP = 606, HJ = 850, `1000m` = NA))
  expect_equal(marks(c(`60m` = 850, LJ = 815, PV = 792), "male")$scores,
               c(`60m` = 851, LJ = 816, PV = 793))

  expect_equal(marks(c(1111, 1171, 785, 1071, 1095, 743, 1005), "female", "heptathlon")$scores,
               c(`100mH` = 1111, HJ = 1171, SP = 785, `200m` = 1071, LJ = 1095,
                 JT = 743, `800m` = 1005))
  expect_equal(marks(c(1157, 799, 221, 743, 638, 1111, 1095, 785, 1171, 826), "female",
                     "decathlon")$scores,
               c(`100m` = 1157, DT = 799, PV = 221, JT = 743, `400m` = 638, `100mH` = 1111,
                 LJ = 1095, SP = 785, HJ = 1171, `1500m` = 826))
  expect_equal(marks(c(1323, 1171, 785, 1095, 1005), "female", "pentathlon")$scores,
               c(`60mH` = 1323, HJ = 1171, SP = 785, LJ = 1095, `800m` = 1005))
  expect_equal(marks(c(HJ = 170, HJ = 1260, LJ = 1093), "female")$scores,
               c(HJ = 172, HJ = 1264, LJ = 1095))
})
