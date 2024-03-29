

library(testthat)
context("Testing the plate-mapping content of the OPM package for consistency")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## PLATE_MAP
test_that("plate names agree", {
  mixed_names_and_values <- function(x) {
    diff <- setdiff(x, names(x))
    length(diff) >= length(x)
  }
  expect_false(mixed_names_and_values(PLATE_MAP))
  expect_equal(names(PLATE_MAP), colnames(WELL_MAP))
  # both should have been brought into the same order (usually alphabetical)
})

