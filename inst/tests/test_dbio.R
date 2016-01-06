

library(testthat)
context("Testing the database I/O methods of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## opm_dbput
## UNTESTED

## opm_dbclass
test_that("opm_dbclass finds the class", {
  expect_equal(opm_dbclass(OPM.1), "OPM_DB")
  expect_equal(opm_dbclass(THIN.AGG), "OPMA_DB")
  expect_equal(opm_dbclass(OPMS.INPUT), "OPM_DB")
  expect_equal(opm_dbclass(MOPMX.1), "OPM_DB")
  expect_equal(opm_dbclass(MOPMX.2), "OPMA_DB")
  x <- do_disc(MOPMX.2)
  expect_equal(opm_dbclass(x), "OPMD_DB")
})

## opm_dbfind
## UNTESTED

## opm_dbget
## UNTESTED

## opm_dbnext
## UNTESTED

## opm_dbclear
## UNTESTED

## opm_dbcheck
## UNTESTED

