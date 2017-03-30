context("founder_qtl_data")

obj_wrong_names <- m4_summary$summary[,1:6]
test_that("founder_qtl_data returns the correct data", {
  expect_equal(founder_qtl_data(m4_summary), founder_qtl_data(m4_summary$summary))
  expect_equal_to_reference(founder_qtl_data(m4_summary), "ref_data/founder_m4_summary.rds")
  expect_equal_to_reference(founder_qtl_data(m4_summary$summary), "ref_data/founder_m4_summary.rds")
})

test_that("founder_qtl_data returns error for incorrectly named obj", {
  expect_error(founder_qtl_data(obj_wrong_names))
})