

library(testthat)
context("Testing the plotting helper functions of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


rand_range <- function(n, minstart = 0, maxstart = 100, maxrange = 100) {
  start <- runif(1L, min = minstart, max = maxstart)
  width <- runif(1L, min = 0, max = maxrange)
  runif(n, start, start + width)
}


################################################################################


## print
## UNTESTED

## ranging
test_that("ranging can be applied", {

  x <- rand_range(10)
  got <- ranging(x)
  expect_equal(max(got), 1)
  expect_equal(min(got), 0)

  x <- rand_range(10)
  got <- ranging(x, fac = 3.5)
  expect_equal(max(got), 3.5)
  expect_equal(min(got), 0)

  x <- rand_range(10)
  got <- ranging(x, extended = FALSE)
  expect_equal(max(got), 1)
  expect_true(min(got) > 0)

  x <- rand_range(10)
  got <- ranging(x, extended = FALSE, fac = 6.1)
  expect_equal(max(got), 6.1)
  expect_true(min(got) > 0)

})


## guess_cex
test_that("cex can be guessed", {
  x <- 1:100
  got <- guess_cex(x)
  expect_equal(length(got), length(x))
  expect_equivalent(-1, cor.test(x, got, method = "spearman")$estimate)
  expect_warning(guess_cex(-1))
  expect_equal(Inf, guess_cex(0))
})


## best_layout
test_that("best layouts can be determined", {
  x <- 0:100
  got <- lapply(x, best_layout)
  prods <- vapply(X = got, FUN = Reduce, FUN.VALUE = 0, f = `*`)
  expect_true(all(prods >= x))
  expect_false(all(prods > x))
  expect_true(all(lengths(got, FALSE) == 2L))
  expect_true(all(vapply(got, function(a) a[1] >= a[2], NA)))
  expect_error(best_layout(-1))
})


## best_range
test_that("optimal ranges can be determined", {
  x <- 1:10
  expect_error(best_range(x, target = 8.9))
  expect_equal(c(0.5, 10.5), best_range(x, target = 10))
  expect_equal(c(0, 11), best_range(x, target = 10, offset = 0.5))
  expect_equal(c(1, 11), best_range(x, target = 10, align = "left"))
  expect_equal(c(0.5, 11.5), best_range(x, target = 10, align = "left",
    offset = 0.5))
  expect_equal(c(0, 10), best_range(x, target = 10, align = "right"))
  expect_equal(c(-0.5, 10.5), best_range(x, target = 10, align = "right",
    offset = 0.5))
  expect_equal(c(1, 10), best_range(x, target = NULL))
  expect_equal(c(0.5, 10.5), best_range(x, target = NULL, offset = 0.5))
})


## best_range
test_that("best ranges can be determined", {
  for (i in 1:100) {
    # real range
    real.range <- range(nums <- rand_range(10L))
    got.range <- range(got <- best_range(nums, NULL))
    expect_true(isTRUE(all.equal(real.range, got.range)))
    # larger range
    large.diff <- real.range[2L] - real.range[1L] + 1
    got.range <- range(got <- best_range(nums, large.diff))
    expect_true(real.range[1L] > got.range[1L])
    expect_true(real.range[2L] < got.range[2L])
    # with offset
    got.range <- range(got <- best_range(nums, NULL, offset = 1))
    expect_true(real.range[1L] > got.range[1L])
    expect_true(real.range[2L] < got.range[2L])
    # with proportional offset
    got.range <- range(got <- best_range(nums, NULL, prop.offset = 0.1))
    expect_true(real.range[1L] > got.range[1L])
    expect_true(real.range[2L] < got.range[2L])
  }
})


## improved_max
test_that("the improved maximum can be calculated", {
  for (i in 1:100) {
    m <- max(nums <- rand_range(10L))
    im.5 <- improved_max(nums, 5)
    im.10 <- improved_max(nums, 10)
    im.20 <- improved_max(nums, 20)
    if (m > 20)
      expect_true(im.20 > im.10)
    if (m > 10)
      expect_true(im.10 > im.5)
    expect_true(im.5 > m)
  }
})


## draw_ci
## UNTESTED


## main_title
test_that("the OPMX function main_title() can be applied to OPMS objects", {
  mt.got <- main_title(OPMS.INPUT, list())
  expect_is(mt.got, "character")
  expect_equal(length(mt.got), 1L)
})


## negative_control
test_that("
    the OPMX function negative_control() can be applied to OPMS objects", {
  nc.got <- negative_control(OPMS.INPUT, neg.ctrl = TRUE)
  expect_is(nc.got, "numeric")
  expect_equal(length(nc.got), 1L)
})


## try_select_colors
## UNTESTED


## default_color_regions
## UNTESTED


