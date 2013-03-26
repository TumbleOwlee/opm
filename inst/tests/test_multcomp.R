

library(testthat)
context("Testing the multiple-testing functions of the OPM package")


# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())


A.VALUES <- extract(c(THIN.AGG, THIN.AGG),
  as.labels = list("organism", "run"), subset = "A", dataframe = TRUE)


################################################################################


## opm_mcp
test_that("mcp without actually performing mcp", {
  # Without computation of multiple comparisons of means
  x <- opm_mcp(A.VALUES, as.labels = list("run", "organism"), do.mcp = FALSE)
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(384L, 6L))
})

## opm_mcp
test_that("error mcp.def", {
  # Without computation of multiple comparisons of means
  # error missing mcp.def
  expect_error(x <- opm_mcp(A.VALUES, as.labels = list("run"), m.type = "lm"))
})

## opm_mcp
test_that("error missing as.labels", {
  # Without computation of multiple comparisons of means
  # error as.labels missing
  expect_error(x <- opm_mcp(A.VALUES, m.type = "lm",
    mcp.def = mcp(run = "Dunnett")))
})

## opm_mcp
test_that("test on assert_all_factors_are_variable", {
  # Without computation of multiple comparisons of means
  # error
  expect_error(x <- opm_mcp(A.VALUES, m.type = "lm", as.labels = list("run",
    "organism"), mcp.def = mcp(organism = "Dunnett")))
})

## opm_mcp
test_that("mcp with specified m.type and with mcp.def", {
  # when 'model' is missing -> defaul-model is used
  x <- opm_mcp(A.VALUES, as.labels = list("run"),
    m.type = "lm", mcp.def = mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("mcp with specified m.type and with mcp.def, version 2", {
  # model is missing, op is stated
  # wrong 'as.labels' is given
  expect_error(x <- opm_mcp(A.VALUES,
    as.labels = list("run", "dummyColName"), op = "+", m.type = "lm",
    mcp.def = mcp(run = "Dunnett")))
})

## opm_mcp
test_that("mcp with specified m.type and with mcp.def, version 3", {
  x <- opm_mcp(A.VALUES, as.labels = list("run"),
    op = "+", m.type = "lm", mcp.def = mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("mcp with specified model", {
  # simple model statement
  x <- opm_mcp(A.VALUES, as.labels = list("run"),
    model = ~ run, mcp.def = mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("mcp with specified model as character-vector", {
  # no op
  x <- opm_mcp(A.VALUES, as.labels = list("run"),
    model = c("run", "Well"), mcp.def = mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("mcp with specified model as character-vector", {
  # m.type = aov and op
  x <- opm_mcp(A.VALUES, as.labels = list("run"), m.type = "aov", op = "+",
    model = c("run", "Well"), mcp.def = mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("stupid user", {
  # Error in mcp2matrix(model, linfct = linfct) :
  # Variable(s) 'run' have been specified in 'linfct'
  # but cannot be found in 'model'!
  expect_error(x <- opm_mcp(A.VALUES, as.labels = list("run"),
    model = Value ~ Well, mcp.def = mcp(run = "Dunnett")))
})


## opm_mcp
test_that("without model, mcp.def and glht.arg specified", {
  # very simple
  x <- opm_mcp(A.VALUES, as.labels = list("run"),
    mcp.def = mcp(run = "Dunnett"),
    glht.arg = list(alternative = "less"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("with model, mcp.def and glht.arg specified", {
  # number of performed comparisons exceeds 20
  expect_warning(x <- opm_mcp(A.VALUES, as.labels = list("run"), m.type = "lm",
    model = ~ Well, mcp.def = mcp(Well = "Dunnett"),
    glht.arg = list(alternative = "less")))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 95)
})

## opm_mcp
test_that("subset of wells with directly defined contrast matrix", {
  # only three comparisons. quite fast.
  rem <- -ncol(A.VALUES):-(ncol(A.VALUES) - 91L)
  x <- opm_mcp(A.VALUES[, rem], as.labels = list("run"), #sub.list = c(1:4),
    mcp.def = mcp(Well = "Dunnett"), model = Value ~ Well)
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 3)
})

## opm_mcp
test_that("mcp.def as predefined object", {
  # TODO Lea: what precisely does this test that isn't tested elsewhere?
  # that mcp.def is stored in an object is trivial
  # see above
  a <- mcp(Well = "Dunnett")
  rem <- -ncol(A.VALUES):-(ncol(A.VALUES) - 91L)
  x <- opm_mcp(A.VALUES[, rem], as.labels = list("run"), #sub.list = c(1:4),
    model = Value ~ Well, m.type = "lm", mcp.def = a)
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 3)
})


## opm_mcp
test_that("mcp.def as predefined matrix-object", {
  # see above
  contr <- rbind(
    "A01 (Negative Control) - A02 (Dextrin)" = c(1, -1, 0, 0),
    "A01 (Negative Control) - A03 (D-Maltose)" = c(-1, 0, 1, 0),
    "A01 (Negative Control) - A04 (D-Trehalose)" = c(-1, 0, 0, -1),
    "A03 (D-Maltose) - A04 (D-Trehalose)" = c(0, 0, 1, -1))
  rem <- -ncol(A.VALUES):-(ncol(A.VALUES) - 91L)
  x <- opm_mcp(A.VALUES[, rem], as.labels = list("run"), #sub.list = c(1:4),
    model = ~ Well, m.type = "lm", mcp.def = contr)
  expect_is(x, "glht")
  expect_equal(x$type, NULL)
  expect_true(is.list(x))
  expect_equal(length(x), 8)
  expect_equal(length(coef(x)), 4)
})

