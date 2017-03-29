context("nfounders")

test_that("nfounders returns the correct integer value", {
  expect_equal(nfounders(m4_cross_qtl), 4)
  expect_equal(nfounders(genomap::bp_cross), 2)
  expect_equal(nfounders(genomap::bp_map), 2)
})

test_that("nfounders returns error for non map/cross objects", {
  expect_error(nfounders(mtcars))
  expect_error(nfounders(4))
})