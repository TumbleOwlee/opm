

library(testthat)
context("Testing the multiple-testing functions of the OPM package")

# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())
# vorbereiten der datenobjekte
EXPL.OPMS <- c(THIN.AGG, THIN.AGG)
EXPL.DF <- extract(EXPL.OPMS, as.labels = list("organism", "run"),
  subset = "A", dataframe = TRUE)

# dataframe erzeugen um die dimensionen der contrast-matrix zu kennen
EXPL. <- opm_mcp(EXPL.DF, model = ~ J(Well + run), linfct = c(Dunnett = 1),
  output = "data")

# funktion um contrastmatrix aufzuspannen:

con.mat <- function(object, model) {

  EXPL.1 <- opm_mcp(object, model = model, # hier muss markus was machen
    linfct = c(Dunnett = 1), output = "data")



  con <- matrix(c(1, -1), nrow = 1, ncol = 2, byrow = TRUE)
  con.mat <- kronecker(diag(1, 96), con) # TODO: adapt number of wells

  val.pos <- which(colnames(EXPL.1) == "Value")
  ex.com <- colnames(EXPL.[, val.pos : c(dim(EXPL.)[2])])[2]

  colnames(con.mat) <- levels(EXPL.[, ex.com])
  # schleife um die rownames zusammenzusetzen
  con.rn <- c()
  for (i in seq(1, length(EXPL.[, ex.com]) / 2, by = 2)) {
    con.rn[i] <- paste(levels(EXPL.[, ex.com])[i], " - ",
      levels(EXPL.[, ex.com])[i + 1], sep = "")
  }
  # rownames anfügen
  con.rn <- con.rn[seq(1, length(EXPL.[, ex.com]) / 2, by = 2)]
  rownames(con.mat) <- con.rn
  return(con.mat)
}

# erzeugen der kontrastmatrix
test.con <- con.mat(EXPL.DF, model = ~ J(Well + run))

# test rechnen
EXPL.CON.MAT <- opm_mcp(EXPL.DF, model = ~ J(Well + run),
  linfct = test.con)
# output ist ganz normales glht-object (mit 96 vergleichen)

################################################################################

# check, if names-attributes is existing

## annotation_vector
test_that("OPM-object", {
  # check class and dimension when testing with first plate from THIN.AGG
  x <- annotation_vector(THIN.AGG[1], "A")
  expect_is(x, "list")
  expect_equal(length(x), 1)
  expect_is(x[[1]], "numeric")
  expect_equal(length(x[[1]]), 96)
  # check, if names-attributes is existing
})

## annotation_vector
test_that("OPMS-object", {
  # check class and dimension when testing with THIN.AGG
  x <- annotation_vector(THIN.AGG, "A")
  expect_is(x, "list")
  expect_equal(length(x), 2)
  expect_is(x[[1]], "numeric")
  expect_is(x[[2]], "numeric")
  expect_equal(length(x[[1]]), 96)
  expect_equal(length(x[[2]]), 96)
})


## annotation_vector
test_that("glht-object", {
  # check class and dimension when testing with EXPL.CON.MAT
  x <- annotation_vector(EXPL.CON.MAT)
  expect_equal(length(x), 96)
  expect_is(x, "numeric")
})


#x <- annotation_vector(THIN.AGG[1], "A", output = "numeric")


#x <- annotation_vector(THIN.AGG[1], "A", output = "logical")



## web_query
## UNTESTED


## collect
## UNTESTED

