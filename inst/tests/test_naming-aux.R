

library(testthat)
context("Testing the opm naming helper functions")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## opm_string
## UNTESTED


################################################################################


## custom_plate_is
## UNTESTED


## custom_plate_proper
## UNTESTED


## custom_plate_prepend
## UNTESTED


## custom_plate_prepend_full
## UNTESTED


## custom_plate_normalize_proper
## UNTESTED


## custom_plate_normalize
## UNTESTED


## custom_plate_normalize_all
## UNTESTED


## custom_plate_exists
## UNTESTED


## custom_plate_get
## UNTESTED


## custom_plate_assert
## UNTESTED


## custom_plate_set
## UNTESTED


## custom_plate_set_full
## UNTESTED


## normalize_predefined_plate
## UNTESTED


################################################################################


## map_param_names
test_that("curve parameter names can be mapped", {
  x <- map_param_names()
  expect_true(all(CURVE_PARAMS %in% unlist(x)))
  y <- map_param_names(opm.fast = TRUE)
  expect_true(!any(names(y) %in% names(x)))
  expect_equivalent(x, y)
  x <- map_param_names(plain = TRUE)
  expect_true(setequal(unlist(x), param_names()))
})


## clean_coords
test_that("well names can be cleaned", {
  x <- c("  Z07\t", "D\n11\r", " A06  ", " B7")
  got <- clean_coords(x)
  expect_equal(got, c("Z07", "D11", "A06", "B07"))
})


## clean_plate_positions
test_that("plate positions can be cleaned", {
  x <- c("21-B", " 9-A", "1", "15", "8-B", NA)
  got <- clean_plate_positions(x)
  expect_true(all(grepl("^\\d{2}-[A-Z?]$", got[!is.na(x)], FALSE, TRUE)))
  expect_warning(got.2 <- clean_plate_positions(got))
  expect_equal(got, got.2)
})


## well_index
test_that("well indices given as formula can be mapped", {
  expect_equal(well_index(1:10), 1:10)
  expect_equal(well_index(), TRUE)
  got <- well_index(letters)
  expect_is(got, "character")
  expect_equal(length(got), 26L)
  expect_equal(toupper(got), got)
  expect_error(well_index(NULL ~ a:c))
  expect_equal(well_index(NULL ~ a:c, letters), 1:3)
  expect_equal(well_index(NULL ~ c(b, e:f), letters), c(2, 5, 6))
  expect_error(well_index(NULL ~ c(b, e:f), LETTERS))
  expect_equal(well_index(NULL ~ c(B, E:F), LETTERS), c(2, 5, 6))
})


## time_index
## UNTESTED


## map_well_names
## UNTESTED


## well_to_substrate
test_that("full substrate names can be back-translated using '@' annotation", {
  x <- c("A01@PM03", "B12@PM-M08", "A02@PM03", "C07@SF-N2")
  got <- well_to_substrate(x, NULL)
  expect_is(got, "character")
  expect_equal(length(got), length(x))
  expect_true(all(got != x))
})


## to_sentence
## UNTESTED


################################################################################


## create_listing
## UNTESTED


################################################################################


## is_cas
test_that("CAS numbers are recognized", {
  expect_true(all(is_cas(c("CAS 554-91-6", "554-94-9", "CAS 107-07-3"))))
  expect_true(all(!is_cas(c("CAS 554-91-7", "foo", "554-94-8", "bar"))))
  expect_true(all(is.na(is_cas(c(NA, NA, NA)))))
})


################################################################################


## web_query
## UNTESTED


## collect
## UNTESTED


################################################################################


