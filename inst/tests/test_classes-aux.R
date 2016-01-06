

library(testthat)
context("Testing the internal helper functions of the classes of the package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## update_settings_list
## UNTESTED


## opm_problems (this actually only tests inheritance relationships)
test_that("WMDS has all methods of WMD, and vice versa", {
  m <- tryCatch(as.character(getGenerics("package:opm")),
    error = function(e) character())
  if (length(m)) {
    wmd.methods <- m[vapply(m, existsMethod, NA, "WMD")]
    wmds.methods <- m[vapply(m, existsMethod, NA, "WMDS")]
    expect_equal(character(), setdiff(wmd.methods, wmds.methods))
  }
})


## opm_problems (this actually only tests inheritance relationships)
test_that("OPMS has all methods of OPM/OPMA/OPMD and some of its own", {
  m <- tryCatch(as.character(getGenerics("package:opm")),
    error = function(e) character())
  if (length(m)) {
    opm.methods <- m[
      vapply(m, existsMethod, NA, "OPMD") |
      vapply(m, existsMethod, NA, "OPMA") |
      vapply(m, existsMethod, NA, "OPM") |
      vapply(m, existsMethod, NA, "WMD") |
      vapply(m, existsMethod, NA, "OPMX") |
      vapply(m, existsMethod, NA, "XOPMX")
    ]
    opms.methods <- m[
      vapply(m, existsMethod, NA, "OPMS") |
      vapply(m, existsMethod, NA, "WMDS") |
      vapply(m, existsMethod, NA, "XOPMX") |
      vapply(m, existsMethod, NA, "OPMX")
    ]
    expect_equal(character(), setdiff(opm.methods, opms.methods))
    expect_true(length(setdiff(opms.methods, opm.methods)) > 0L)
  } else {
    cat("NOTE: cannot count the OPM and OPMS methods\n")
  }
})


## attach_attr
## UNTESTED


## rename_wells
## UNTESTED


## opma_problems
## UNTESTED


## opmd_problems
## UNTESTED


## opms_problems
test_that("new() can be used to put plates together", {
  x <- list(a = OPM.1, b = OPM.2)
  x.opms <- new("OPMS", plates = x)
  expect_is(x.opms, "OPMS")
  expect_equal(length(x.opms), 2L)
  expect_equal(NULL, names(plates(x.opms)))
})


################################################################################


## initialize
test_that("the example objects have the correct classes", {
  expect_is(OPMS.INPUT, "OPMS")
  expect_is(THIN.AGG, "OPMS")
  expect_is(SMALL, "OPM")
  expect_false(is(SMALL, "OPMA"))
  expect_is(SMALL.AGG, "OPM")
  expect_is(SMALL.AGG, "OPMA")
})


## initialize
test_that("MOPMX objects are correctly created", {
  expect_error(x <- new("MOPMX", list(A = NULL, B = SMALL)))
  x <- new("MOPMX", list(B = SMALL, THIN.AGG))
  expect_equal(names(x), c("B", ""))
})



################################################################################
