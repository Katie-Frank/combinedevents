context("expected scores")

test_that("results of scores()", {
  expect_equal(
    scores(c(10.8, 7.1, 12, 2.05, 49.5, 14.5, 35, 4.6, 40, "4:21"), "male", "decathlon")$scores,
    c(`100m` = 906, LJ = 838, SP = 606, HJ = 850, `400m` = 838, `110mH` = 911, DT = 564,
      PV = 790, JT = 442, `1500m` = 805))
  expect_equal(
    scores(c(10.8, 7.1, 12, 2.05, 49.5, 14.5, 35, 4.6, 40, "4:21"), "male", "decathlon")$score_total,
    7550)
  expect_equal(scores(c(7.1, 40, 23.41, 35, "4:21"), "male", "outdoor pentathlon")$scores,
               c(LJ = 838, JT = 442, `200m` = 747, DT = 564, `1500m` = 805))
  expect_equal(scores(c(7.1, 40, 23.41, 35, "4:21"), "male", "outdoor pentathlon")$score_total,
               3396)
  expect_equal(scores(c(6.99, 7.1, 12, 2.05, 8.34, 4.6, "2:38.77"), "male", "heptathlon")$scores,
               c(`60m` = 886, LJ = 838, SP = 606, HJ = 850, `60mH` = 898, PV = 790,
                 `1000m` = 887))
  expect_equal(scores(c(6.99, 7.1, 12, 2.05, 8.34, 4.6, "2:38.77"), "male", "heptathlon")$score_total,
               5755)
  expect_equal(scores(c(8.34, 7.1, 12, 2.05, "2:38.77"), "male", "indoor pentathlon")$scores,
               c(`60mH` = 898, LJ = 838, SP = 606, HJ = 850, `1000m` = 887))
  expect_equal(scores(c(8.34, 7.1, 12, 2.05, "2:38.77"), "male", "indoor pentathlon")$score_total,
               4079)
  expect_equal(scores(c(`60m` = 7.09, LJ = 7, LJ = 7.03), "male")$scores,
               c(`60m` = 851, LJ = 814, LJ = 821))
  expect_equal(scores(c(13.09, 1.95, 13.86, 23.08, 6.77, 43.93, "2:07.26"), "female",
                      "heptathlon")$scores,
               c(`100mH` = 1111, HJ = 1171, SP = 785, `200m` = 1071, LJ = 1095,
                 JT = 743, `800m` = 1005))
})
