

library(testthat)
context("Testing the plotting functions of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## summary
test_that("a summary can be printed", {
  # OPM method
  s <- summary(OPM.1)
  expect_is(s, "OPM_Summary")
  expect_true(length(s) > 7L)
  capture.output(expect_equal(print(s), s))
  # OPMS method
  s <- summary(OPMS.INPUT)
  capture.output(expect_equal(print(s), s))
  expect_is(s, "OPMS_Summary")
  expect_equal(length(s), length(OPMS.INPUT))
  expect_true(all(vapply(s, inherits, logical(1L), "OPM_Summary")))
  # MOPMX method
  s <- summary(MOPMX.1)
  expect_is(s, "MOPMX_Summary")
  capture.output(expect_equal(print(s), s))
  expect_equal(nrow(s), length(MOPMX.1))
})

## show
## UNTESTED

## str
## UNTESTED

## xy_plot
test_that("the OPMS method xy_plot() works", {
  opms.input <- OPMS.INPUT[, 1L:10L]
  expect_is(opms.input, "OPMS")
  got <- xy_plot(opms.input)
  expect_is(got, "trellis")
  got <- xy_plot(opms.input, include = "organism")
  expect_is(got, "trellis")
  expect_error(got <- xy_plot(opms.input, include = "doesnotexist"))
})


## level_plot
test_that("the OPMS method level_plot() works", {
  opms.input <- OPMS.INPUT[, 1L:10L]
  expect_is(opms.input, "OPMS")
  got <- level_plot(opms.input)
  expect_is(got, "trellis")
})


## ci_plot
test_that("a tie-fighter (CI) plot can be drawn", {
  legend <- ci_plot(THIN.AGG[, , 1:6], as.labels = list("organism", "run"),
    subset = "A", na.action = "ignore")
  expect_equal(c("1: Bacillus simplex 3", "2: Bacillus simplex 4"), legend)
  legend <- ci_plot(THIN.AGG[, , 1:6], as.labels = list("organism"),
    subset = "A", na.action = "ignore", legend.field = NULL, bg = "lightgrey",
    x = "bottom")
  expect_equal(c("1: Bacillus simplex", "2: Bacillus simplex"), legend)
})


## heat_map
test_that("a heatmap can be drawn", {

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "A", as.groups = list("organism"))
  mat.2 <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "A", as.groups = list("organism"), dataframe = TRUE)

  hm <- heat_map(mat, margins = c(5, 5), use.fun = "stats")
  expect_is(hm, "list")
  expect_equal(NULL, hm$colColMap)
  expect_equal(names(hm$rowColMap), metadata(THIN.AGG, "organism"))

  # Data frame version
  hm.2 <- heat_map(mat.2, as.labels = c("organism", "run"),
    as.groups = "organism", margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)

  # Distance given as function or list
  hm.2 <- heat_map(mat, distfun = dist, margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)
  hm.2 <- heat_map(mat, distfun = list(method = "euclidean"),
    margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)

  # Clustering function given in distinct ways
  hm <- heat_map(mat, hclustfun = hclust, margins = c(5, 5), use.fun = "stats")
  expect_false(identical(hm.2, hm))
  hm.2 <- heat_map(mat, hclustfun = "complete", margins = c(5, 5),
    use.fun = "stats")
  expect_equal(hm, hm.2)
  hm.2 <- heat_map(mat, hclustfun = list(method = "complete"),
    margins = c(5, 5), use.fun = "stats")
  expect_equal(hm, hm.2)

  # Column groups
  group_fun <- function(x) substr(x, 1, 1)
  hm <- heat_map(mat, margins = c(5, 5), c.groups = group_fun)
  groups <- group_fun(colnames(mat))
  hm.2 <- heat_map(mat, margins = c(5, 5), c.groups = groups)
  expect_equal(hm, hm.2)
  expect_equivalent(groups, names(hm$colColMap))

})


## heat_map
test_that("a heat map can be drawn from a MOPMX object", {
  hm <- heat_map(MOPMX.2, ~ run, as.groups = ~ organism)
  expect_is(hm, "list")
  expect_equal(NULL, hm$colColMap)
  expect_true(setequal(names(hm$rowColMap), to_metadata(MOPMX.2)$organism))
})


################################################################################


## radial_plot
test_that("a radial plot can be drawn", {
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "A", as.groups = list("organism"))
  got <- radial_plot(mat)
  expected <- structure(select_colors()[1L:2L],
    names = c("Bacillus simplex 4", "Bacillus simplex 3"))
  expect_equal(got, expected)
})


################################################################################


## parallelplot
## UNTESTED

## parallel_plot
## UNTESTED

## cor.test
## UNTESTED

