context("combo_plot")

test_that("combo_plot is returning gtable object", {
  expect_is(combo_plot(m4_cross_qtl, m4_summary), "gtable")
  expect_is(combo_plot(m4_int_qtl, m4_summary), "gtable")
})
