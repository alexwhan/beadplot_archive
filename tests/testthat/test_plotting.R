context("make_main_plot")

test_that("base plot is returning ggplot object", {
  expect_is(make_main_plot(m4_cross_qtl), "ggplot")
  expect_is(make_main_plot(genomap::bp_cross), "ggplot")
  expect_is(make_main_plot(genomap::bp_map), "ggplot")
})

test_that("founder plot is returning ggplot object", {
  expect_is(make_founder_plot(m4_cross_qtl), "ggplot")
  expect_is(make_founder_plot(genomap::bp_cross), "ggplot")
  expect_is(make_founder_plot(genomap::bp_map), "ggplot")
})