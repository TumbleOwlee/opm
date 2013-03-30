

library(testthat)
context("Testing the multiple-testing functions of the OPM package")


# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())


EXPL.OPMS <- c(THIN.AGG, THIN.AGG)
EXPL.DF <- extract(EXPL.OPMS,
  as.labels = list("organism", "run"), subset = "A", dataframe = TRUE)


################################################################################


## opm_mcp
test_that("opm_mcp runs without actually performing mcp", {
  # Without computation of multiple comparisons of means
  x <- opm_mcp(EXPL.DF, model = list("organism", "run"), do.mcp = FALSE)
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(384L, 6L))
  y <- opm_mcp(EXPL.DF, model = ~ organism + run, do.mcp = FALSE)
  expect_equal(x, y)
  y <- opm_mcp(EXPL.OPMS, model = ~ organism + run, do.mcp = FALSE)
  expect_equal(x, y)
})


## opm_mcp
test_that("error mcp.def", {
  # Without computation of multiple comparisons of means
  # error missing mcp.def
  expect_error(x <- opm_mcp(EXPL.DF, model = list("run"), m.type = "lm"))
})

## opm_mcp
test_that("error missing model", {
  # Without computation of multiple comparisons of means
  # error model missing
  expect_error(x <- opm_mcp(EXPL.DF, m.type = "lm",
    mcp.def = mcp(run = "Dunnett")))
})

## opm_mcp
test_that("test on assert_all_factors_are_variable", {
  # Without computation of multiple comparisons of means
  # error
  expect_error(x <- opm_mcp(EXPL.DF, m.type = "lm", model = list("run",
    "organism"), mcp.def = mcp(organism = "Dunnett")))
})

## opm_mcp
test_that("mcp with specified m.type and with mcp.def", {
  # when 'model' is missing -> defaul-model is used
  x <- opm_mcp(EXPL.DF, model = list("run"),
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
  # wrong 'model' is given
  expect_error(x <- opm_mcp(EXPL.DF,
    model = list("run", "dummyColName"), ops = "+", m.type = "lm",
    mcp.def = mcp(run = "Dunnett")))
})

## opm_mcp
test_that("mcp with specified m.type and with mcp.def, version 3", {
  x <- opm_mcp(EXPL.DF, model = list("run"),
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
  x <- opm_mcp(EXPL.DF, model = list("run"), mcp.def = mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})


## opm_mcp
test_that("mcp with specified model as list #1", {
  # no op
  x <- opm_mcp(EXPL.DF, model = list("run", "Well"),
    mcp.def = mcp(run = "Dunnett"))
  expect_is(x, "glht")
  expect_equal(x$type, "Dunnett")
  expect_true(is.list(x))
  expect_equal(length(x), 9)
  expect_equal(length(coef(x)), 1)
})

## opm_mcp
test_that("mcp with specified model as list #2", {
  # m.type = aov and op
  x <- opm_mcp(EXPL.DF, model = list("run", "Well"), m.type = "aov", op = "+",
    mcp.def = mcp(run = "Dunnett"))
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
  expect_error(x <- opm_mcp(EXPL.DF, model = Value ~ Well,
    mcp.def = mcp(run = "Dunnett")))
})


## opm_mcp
test_that("without model, mcp.def and glht.arg specified", {
  # very simple
  x <- opm_mcp(EXPL.DF, model = list("run"),
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
  expect_warning(x <- opm_mcp(EXPL.DF, model = ~ Well, m.type = "lm",
    mcp.def = mcp(Well = "Dunnett"),
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
  rem <- -ncol(EXPL.DF):-(ncol(EXPL.DF) - 91L)
  x <- opm_mcp(EXPL.DF[, rem], model = Value ~ Well,
    mcp.def = mcp(Well = "Dunnett"))
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
  rem <- -ncol(EXPL.DF):-(ncol(EXPL.DF) - 91L)
  x <- opm_mcp(EXPL.DF[, rem],
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
  rem <- -ncol(EXPL.DF):-(ncol(EXPL.DF) - 91L)
  x <- opm_mcp(EXPL.DF[, rem],
    model = ~ Well, m.type = "lm", mcp.def = contr)
  expect_is(x, "glht")
  expect_equal(x$type, NULL)
  expect_true(is.list(x))
  expect_equal(length(x), 8)
  expect_equal(length(coef(x)), 4)
})

