context("founder_names")

test_that("founder_names returns the correct names for mpInterval and mpcross", {
  expect_equal(founder_names(small_mpcross), c("A", "B", "C", "D"))
  expect_equal(founder_names(small_mpint), c("A", "B", "C", "D"))
  expect_equal(founder_names(small_mpcross), founder_names(small_mpint))
})
