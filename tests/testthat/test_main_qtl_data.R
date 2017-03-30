context("main_qtl_data")

obj_wrong_names <- m4_summary$summary[,1:6]
test_that("main_qtl_data returns the correct data", {
  expect_equal(main_qtl_data(m4_summary$summary), main_qtl_data(m4_summary))
  expect_equal_to_reference(main_qtl_data(m4_summary), "ref_data/main_m4_summary.rds")
  expect_equal_to_reference(main_qtl_data(m4_summary$summary), "ref_data/main_m4_summary.rds")
})

test_that("main_qtl_data returns error for incorrectly named obj", {
  expect_error(main_qtl_data(obj_wrong_names))
})