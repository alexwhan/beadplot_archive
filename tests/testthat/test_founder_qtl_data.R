context("founder_qtl_data")

ref_dat <- structure(list(qtl = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L), 
               lg = c(4, 4, 4, 4, 5, 5, 5, 5), 
               qtl_centre = c(31.03, 31.03, 31.03, 31.03, 77.66, 77.66, 77.66, 77.66), 
               founder = c("A", "B", "C", "D", "A", "B", "C", "D"), 
               founder_cont = c(-0.038, 0.26, 0.036, -0.259, 0.285, -0.186, -0.351, 0.253), 
               founder_prob = c(0.399, 0.042, 0.408, 0.045, 0.071, 0.167, 0.034, 0.095),
               founder_logp = c(0.4, 1.37, 0.39, 1.35, 1.15, 0.78, 1.47, 1.02)), 
          row.names = c(NA, -8L), 
          class = c("tbl_df", "tbl", "data.frame"), 
          .Names = c("qtl", "lg", "qtl_centre", "founder", "founder_cont", "founder_prob", "founder_logp"))

obj_wrong_names <- m4_summary$summary[,1:6]
test_that("founder_qtl_data returns the correct data", {
  expect_equal(founder_qtl_data(m4_summary), ref_dat)
  expect_equal(founder_qtl_data(m4_summary$summary), ref_dat)
})

test_that("founder_qtl_data returns error for incorrectly named obj", {
  expect_error(founder_qtl_data(obj_wrong_names))
})