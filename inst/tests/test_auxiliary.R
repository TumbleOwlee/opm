

library(testthat)
context("Testing the exported auxiliary functions of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################
################################################################################
#
# Miscellaneous helper functions
#


## separate
test_that("character vectors can be split regularly", {

  # a typical usage example: a bunch of filenames from which the settings
  # under which they have been created are to be extracted
  x <- c(
    "ibb_blastall.sim2_IS.log",
    "ibb_blastall.sim2_LS.log",
    "ibb_blastall.sim2_SS.log",
    "ibb_blat.sim2_IS.log",
    "ibb_blat.sim2_LS.log",
    "ibb_blat.sim2_SS.log",
    "ibb_megablast.sim2_IS.log",
    "ibb_megablast.sim2_LS.log",
    "ibb_megablast.sim2_SS.log"
  )

  got <- separate(x, c("#", "?", "%"))
  expect_is(got, "matrix")
  expect_equal(ncol(got), 1L)
  expect_equal(x, got[, 1L])

  got <- separate(x, NULL)
  expect_is(got, "matrix")
  expect_equal(ncol(got), 1L)
  expect_equal(x, got[, 1L])

  got <- separate(x, c(".", "_"))
  expect_is(got, "matrix")
  expect_equal(dim(got), c(length(x), 5L))
  expect_true(all(got[, 1L] == "ibb"))
  expect_true(all(got[, 3L] == "sim2"))
  expect_true(all(got[, 5L] == "log"))

  got.2 <- separate(x, c("_", "."))
  expect_equal(got.2, got)
  got.2 <- separate(x, "_.")
  expect_equal(got.2, got)
  got.2 <- separate(x, c("_-.", "#%&()"))
  expect_equal(got.2, got)

})

## separate
test_that("character vectors can be split regularly even if constant", {

  x <- paste("x", letters)
  got <- separate(x, keep.const = TRUE, split = " ")
  expect_is(got, "matrix")
  expect_equal(dim(got), c(26L, 2L))
  got <- separate(x, keep.const = FALSE, split = " ")
  expect_is(got, "matrix")
  expect_equal(dim(got), c(26L, 1L))
  got <- separate(x, keep.const = FALSE, split = " ", simplify = TRUE)
  expect_is(got, "character")
  expect_equal(got, letters)

  x <- paste("x", "y")
  got <- separate(x, keep.const = FALSE, split = " ", simplify = TRUE)
  expect_is(got, "character")
  expect_identical(got, "x")
  got <- separate(x, keep.const = FALSE, split = " ", simplify = FALSE)
  expect_is(got, "matrix")
  expect_equal(dim(got), c(1L, 0L))

})

## separate
test_that("character vectors can be split regularly with multiple separators", {
  x <- c(
    "ibb__blastall.sim2_IS.log",
    "ibb_blastall..sim2_LS.log",
    "ibb_blastall.sim2_SS.log"
    )
  got <- separate(x, keep.const = TRUE, split = "_.", simplify = TRUE)
  expect_is(got, "matrix")
  expect_equal(dim(got), c(3, 5))
})

## separate
test_that("character vectors can be split regularly in list-wise mode", {
  x <- c(
    "a, b",
    "a, c",
    NA_character_,
    "c, d, a"
    )
  got <- separate(x, keep.const = FALSE, split = ",", simplify = TRUE,
    list.wise = TRUE)
  expect_is(got, "matrix")
  expect_equal(dim(got), c(4, 3))
  expect_equal(colnames(got), c("b", "c", "d"))
  expect_true(all(is.na(got[3L, ])))
  expect_is(got, "matrix")
  got <- separate(x, keep.const = TRUE, split = ",", simplify = TRUE,
    list.wise = TRUE)
  expect_equal(dim(got), c(4, 4))
  expect_equal(colnames(got), c("a", "b", "c", "d"))
})

## separate
test_that("factors can be split regularly", {
  x <- as.factor(c(
    "ibb_blastall.sim2_IS.log",
    "ibb_blastall.sim2_LS.log",
    "ibb_blastall.sim2_SS.log",
    "ibb_blat.sim2_IS.log",
    "ibb_blat.sim2_LS.log",
    "ibb_blat.sim2_SS.log",
    "ibb_megablast.sim2_IS.log",
    "ibb_megablast.sim2_LS.log",
    "ibb_megablast.sim2_SS.log"
    ))
  got <- separate(x)
  expect_is(got, "data.frame")
  expect_equal(dim(got), c(9L, 5L))
  got <- separate(x, keep.const = FALSE)
  expect_is(got, "data.frame")
  expect_equal(dim(got), c(9L, 2L))
})

## separate
## UNTESTED (data-frame method)


################################################################################
################################################################################
#
# Global options
#

## opm_opt
## UNTESTED

## param_names
test_that("param_names() is consistent with other settings", {
  expect_true(OPM_OPTIONS$curve.param %in%
      c(param_names(), param_names("disc.name")))
})


