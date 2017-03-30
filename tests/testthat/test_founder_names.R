context("founder_names")

test_that("founder_names returns the correct names for mpInterval and mpcross", {
  expect_equal(founder_names(m4_cross_qtl), c("A", "B", "C", "D"))
  expect_equal(founder_names(m4_int_qtl), c("A", "B", "C", "D"))
  expect_equal(founder_names(m4_int_qtl), founder_names(m4_cross_qtl))
})
