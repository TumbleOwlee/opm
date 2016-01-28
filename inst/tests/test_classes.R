

library(testthat)
context("Testing the classes of the opm package and their conversion functions")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## as
test_that("the OPM/OPMA example data can be converted to a list and back", {

  # Converting to list and back
  opm.list <- as(OPM.1, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPM")
  expect_equal(OPM.1, opm.back)

  # Converting with metadata to list and back
  opm.list <- as(OPM.WITH.MD, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPM")
  expect_equal(OPM.WITH.MD, opm.back)

  # Converting aggregated stuff to list and back
  opm.list <- as(SMALL.AGG, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPMA")
  expect_equal(SMALL.AGG, opm.back)

  # With distortion of ordering and addition of nonsense
  change <- 1L:5L
  expect_true("aggregated" %in% names(opm.list))
  expect_true(all(vapply(opm.list$aggregated, is.list, NA)))
  opm.list$aggregated <- c(opm.list$aggregated[-change],
    opm.list$aggregated[change])
  opm.list$aggregated[change] <- lapply(opm.list$aggregated[change], rev)
  opm.list$aggregated[-change] <- lapply(X = opm.list$aggregated[-change],
    FUN = c, Z = 7)
  opm.back <- as(opm.list, "OPMA")
  expect_equal(SMALL.AGG, opm.back)

})

## as
test_that("some conversion are forbidden", {
  expect_error(as(OPM.1, "OPMA"))
  expect_error(as(OPM.1, "OPMD"))
  expect_error(as(THIN.AGG, "OPMD"))
})


## as
test_that("the OPMS example data can be converted to a list and back", {

  # Converting to list and back
  opms.list <- as(OPMS.INPUT, "list")
  expect_is(opms.list, "list")
  opms.back <- as(opms.list, "OPMS")
  expect_equal(OPMS.INPUT, opms.back)

  # Converting with aggregated data to list and back
  opms.list <- as(THIN.AGG, "list")
  expect_is(opms.list, "list")
  opms.back <- as(opms.list, "OPMS")
  expect_equal(THIN.AGG, opms.back)

})

## as
test_that("the OPM example data can be converted to a list and back", {
  # conversion of a list to an OPM object is tolerant against re-orderings
  # (but not against additions and omissions)
  x <- as(SMALL, "list")
  x$measurements <- c(rev(x$measurements[7:8]), rev(x$measurements[-7:-8]))
  x <- as(x, "OPM")
  expect_equal(measurements(x), measurements(SMALL))
})

## as
test_that("the OPMA example data can be converted to a list and back", {
  # conversion of a list to an OPMA object is tolerant against re-orderings
  # and additions (but not against omissions)
  x <- as(SMALL.AGG, "list")
  x$aggregated <- c(Answer = 42L, rev(x$aggregated), Text = LETTERS)
  x <- as(x, "OPMA")
  expect_equal(aggregated(x), aggregated(SMALL.AGG))
})

## as
test_that("OPMD objects can be converted to a list and back", {
  # conversion of a list to an OPMA object is tolerant against re-orderings
  # and additions (but not against omissions)
  d <- do_disc(SMALL.AGG, TRUE)
  x <- as(d, "list")
  x$discretized <- c(Answer = 42L, rev(x$discretized), Text = LETTERS)
  x <- as(x, "OPMD")
  expect_equal(discretized(x), discretized(d))
})


## as
test_that("MOPMX objects can be converted to database I/O objects and back", {
  expect_error(got <- as(MOPMX.1, "OPMA_DB"))
  expect_error(got <- as(MOPMX.1, "OPM_DB"))
  x <- MOPMX.1
  metadata(x[[1]]) <- list(organism = "Limulus polyphemus", run = 11)
  got <- as(x, "OPM_DB")
  expect_is(got, "OPM_DB")
  expect_true(setequal(plate_type(got), plate_type(MOPMX.1)))
  got.2 <- as(got, "MOPMX")
  expect_is(got.2, "MOPMX")
  expect_true(setequal(plate_type(got.2), plate_type(MOPMX.1)))
})


################################################################################

