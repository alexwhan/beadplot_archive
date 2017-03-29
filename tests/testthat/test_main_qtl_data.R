context("main_qtl_data")

ref_dat <- structure(list(qtl = 1:2, lg = c(4, 5), qtl_centre = c(31.03, 77.66), 
                          qtl_prob = c(0.007, 0), qtl_perc_var = c(4.2, 9), 
                          qtl_logp = c(2.15, 4.9)), 
                     .Names = c("qtl", "lg", "qtl_centre", "qtl_prob", 
                                "qtl_perc_var", "qtl_logp"), 
                     row.names = c(NA, -2L), 
                     class = c("tbl_df", "tbl", "data.frame"))

obj_wrong_names <- m4_summary$summary[,1:6]
test_that("main_qtl_data returns the correct data", {
  expect_equal(main_qtl_data(m4_summary), ref_dat)
  expect_equal(main_qtl_data(m4_summary$summary), ref_dat)
})

test_that("main_qtl_data returns error for incorrectly named obj", {
  expect_error(main_qtl_data(obj_wrong_names))
})