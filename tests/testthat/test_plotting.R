context("make_base_plot")

test_that("base plot is returning ggplot object", {
  expect_is(make_base_plot(m4_cross_qtl), "ggplot")
  expect_is(make_base_plot(genomap::bp_cross), "ggplot")
  expect_is(make_base_plot(genomap::bp_map), "ggplot")
})

test_that("founder plot is returning ggplot object", {
  expect_is(make_founder_plot(m4_cross_qtl), "ggplot")
  expect_is(make_founder_plot(genomap::bp_cross), "ggplot")
  expect_is(make_founder_plot(genomap::bp_map), "ggplot")
})