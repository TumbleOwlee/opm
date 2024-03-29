

library(testthat)
context("Testing the internal helper functions of the auxiliary methods")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################
################################################################################
#
# Miscellaneous helper functions
#


## get_and_remember
test_that("we can memoize queries", {
  query <- letters[1L:5L]
  prefix <- "TEST."
  expect_false(any(vapply(paste0(prefix, query), exists, NA, MEMOIZED)))
  qfun <- function(x) as.list(rep.int(42L, length(x)))
  result <- get_and_remember(query, prefix, NA, qfun)
  expect_equal(names(result), query)
  expect_true(all(vapply(paste0(prefix, query), exists, NA, MEMOIZED)))
  result.2 <- get_and_remember(query, prefix, NA, qfun)
  expect_equal(result.2, result)
  expect_true(all(vapply(paste0(prefix, query), exists, NA, MEMOIZED)))
  rm(list = paste0(prefix, query), envir = MEMOIZED)
})


################################################################################


## common_times
## UNTESTED


## select_by_disc
## UNTESTED


## do_select
## UNTESTED


################################################################################


## reduce_to_mode
## UNTESTED


## list2matrix
## UNTESTED


## close_index_gaps
## UNTESTED


## no_gaps
## UNTESTED


## metadata2factorlist
## UNTESTED


## sub_indexes
test_that("sub-indexes can be got and incremented", {
  x <- list(a = 1:2, b = 'a', c = c(TRUE, FALSE, FALSE))
  got <- sub_indexes(x)
  expect_equal(got, structure(list(a = 1:2, b = 3, c = 4:6), total = 6L))
})


## simplify_conditionally
## UNTESTED


## is_uniform
test_that("uniformity can be checked", {
  x <- list(a = 1:2, b = 1:2, a = 1:2)
  expect_true(isTRUE(is_uniform(x)))
  x <- c(x, list(c = 1:6))
  expect_false(isTRUE(isuni <- is_uniform(x)))
  expect_equal(isuni, x[c(1L, 4L)])
})


## reassign_duplicates
test_that("calculation are not done for duplicates again", {
  x <- c("d", "a", "a", "b", "b", "a", "d")
  got <- reassign_duplicates(x, function(x) paste0(x, x))
  expect_equal(got, paste0(x, x))
})


## is_constant
test_that("constantness can be checked", {

  x <- list(a = 1:2, b = 1:2, a = 1:2)
  expect_true(is_constant(x))

  x <- c(x, list(c = 1:6))
  expect_false(is_constant(x))
  # expect_true(is_constant(x, set.like = TRUE))

  y <- list(a = 1:2, b = 1, c = 2)
  expect_false(is_constant(y))
  # expect_false(is_constant(y, set.like = TRUE))

  x <- matrix(c(1, 2, 3, 2, 2, 2), ncol = 2)
  expect_equal(is_constant(x), c(FALSE, TRUE, FALSE))
  expect_equal(is_constant(x, 2), c(FALSE, TRUE))
  expect_equal(is_constant(x, 0), FALSE)
  expect_error(is_constant(x, 3))

})


## pick_from
test_that("rows can be picked", {

  x <- data.frame(a = 1:10, b = 11:20, c = letters[1:10],
    stringsAsFactors = FALSE)

  got <- pick_from(x, list(a = 4:5, b = 14))
  expect_equal(colnames(got), colnames(x))
  expect_equal(dim(got), c(1, 3))
  expect_equal(as.list(got[1, ]), list(a = 4, b = 14, c = "d"))

  got <- pick_from(x, list(a = 4:5, b = 15:14))
  expect_equal(colnames(got), colnames(x))
  expect_equal(dim(got), c(2, 3))
  expect_equal(as.list(got[2, ]), list(a = 5, b = 15, c = "e"))

  got <- pick_from(x, list(a = 4:5, b = 16:17))
  expect_equal(colnames(got), colnames(x))
  expect_equal(dim(got), c(0, 3))

  y <- x[1, , drop = FALSE]
  got <- pick_from(y, list(a = 4:5, b = 14))
  expect_equal(dim(got), c(0L, 3L))
  got <- pick_from(y, list(a = 1:2, b = 11:14))
  expect_equal(dim(got), c(1L, 3L))

  y <- rbind(x, c(NA, NA, NA, NA))
  got <- pick_from(y, list(a = 1, b = 11))
  expect_equal(dim(got), c(1L, 3L))

  expect_error(pick_from(y, list(a = 1, z = 11)))

})


## assert_splittable_matrix
## UNTESTED

## strip_whitespace
## UNTESTED

## vector2row
## UNTESTED


################################################################################
################################################################################
#
# String processing
#


## create_formula
## UNTESTED

## formula2infix
## UNTESTED

## metadata_key
test_that("we can convert formulas to formulas for use as metadata keys", {

  v <- c("A", "B")
  f <- ~ a $ b $ c + I(v) * ("d" + e) + c("f", "g", "h") | i$"j"
  got <- metadata_key(f, TRUE)
  expect_equal(got, ~ a.b.c + A.B * (d + e) + c(f, g, h) | i.j)

  f <- ~ a $ b $ c + I(v) * J("d" + e) + c("f", "g", "h") | i$"j"
  got <- metadata_key(f, TRUE)
  expect_equal(attr(got, "combine"), list(d.e = c("d", "e")))
  expect_equal(got, ~ a.b.c + A.B * d.e + c(f, g, h) | i.j)

  f <- ~ a $ b $ c + I(v) * J("d", e$r) + c("f", "g", "h") | i$"j"
  old <- opm_opt(comb.key.join = "#")
  got <- metadata_key(f, TRUE)
  expect_equal(attr(got, "combine"), list(`d#e.r` = c("d", "e.r")))
  expect_equal(got, ~ a.b.c + A.B * `d#e.r` + c(f, g, h) | i.j)
  opm_opt(comb.key.join = old$comb.key.join)

  f <- Value ~ k & foo.bar.baz
  got <- metadata_key(f, TRUE)
  expect_equal(got, f)
  got <- metadata_key(f, TRUE, syntactic = TRUE)
  expect_equal(got, f)
  f2 <- Value ~ k & `foo.bar?baz`
  got <- metadata_key(f2, TRUE)
  expect_equal(got, f2)
  got <- metadata_key(f2, TRUE, syntactic = TRUE)
  expect_equal(got, f)

})


## metadata_key
test_that("we can convert formulas to lists for use as metadata keys", {
  v <- c("A", "B")
  f <- ~ a $ b $ c + I(v) * ("d ?" + e) + c("f", "g", "h") | i$"j"

  got <- metadata_key(f, FALSE)
  wanted <- list(a.b.c = c("a", "b", "c"), A.B = c("A", "B"),
    `d ?` = "d ?", e = "e", f = "f", g = "g", h = "h", i.j = c("i", "j"))
  expect_equal(got, wanted)
  got <- metadata_key(f, FALSE, syntactic = TRUE)
  pos <- match("d ?", names(wanted))
  names(wanted)[pos] <- wanted[[pos]] <- "d.."
  expect_equal(got, wanted)

  got <- metadata_key(f, FALSE, remove = c("A.B", "i.j"))
  wanted <- list(a.b.c = c("a", "b", "c"),
    `d ?` = "d ?", e = "e", f = "f", g = "g", h = "h")
  expect_equal(got, wanted)
  got <- metadata_key(f, FALSE, syntactic = TRUE, remove = c("A.B", "i.j"))
  pos <- match("d ?", names(wanted))
  names(wanted)[pos] <- wanted[[pos]] <- "d.."
  expect_equal(got, wanted)

  f <- ~ a $ b $ c + I(v) * J("d" + e + E$F) + c("f", "g", "h") | i$"j"
  got <- metadata_key(f, FALSE)
  wanted <- list(a.b.c = c("a", "b", "c"), A.B = c("A", "B"),
    d = "d", e = "e", E.F = c("E", "F"), f = "f", g = "g", h = "h",
    i.j = c("i", "j"))
  attr(wanted, "combine") <- list(d.e.E.F = c("d", "e", "E.F"))
  expect_equal(got, wanted)

  f <- Value ~ Well
  got <- metadata_key(f, FALSE)
  wanted <- c(Well = "Well")
  expect_equal(got, wanted)
  got <- metadata_key(f, FALSE, remove = RESERVED_NAMES)
  expect_equal(got, NULL)

})


## metadata_key
test_that("we can convert lists for use as formulas", {

  x <- list(c("a", "b c"), list(K = "t", I = c("D", "E")))
  got <- metadata_key(x, TRUE)
  expect_equal(got, ~ `a.b c` + K + I)
  got <- metadata_key(x, TRUE, ops = c("+", "|"))
  expect_equal(got, ~ `a.b c` + K | I)
  got <- metadata_key(x, TRUE, remove = "K", ops = c("+", "|"))
  expect_equal(got, ~ `a.b c` + I)

  got <- metadata_key(x, TRUE, syntactic = TRUE)
  expect_equal(got, ~ a.b.c + K + I)

  x <- list("run")
  got <- metadata_key(x, TRUE)
  expect_equal(got, ~ run)

})


## metadata_key
test_that("some edge cases are correctly handled by metadata_key()", {
  x <- character()
  names(x) <- character()
  expect_error(metadata_key(x, TRUE))
  expect_equal(x, metadata_key(x, FALSE))
  expect_equal(NULL, metadata_key(NULL, FALSE))
  x <- numeric()
  got <- metadata_key(x, TRUE)
  expect_equal(metadata_key(got, TRUE), got)
  x <- ~ list(list(), list())
  expect_equal(x, metadata_key(x, TRUE))
})


## reassign_args_using
## UNTESTED


## parse_time
test_that("time strings can be parsed", {
  expect_warning(got <- parse_time(c("11.11.1911 11:11:11", "xxx")))
  expect_is(got, "POSIXlt")
  expect_equal(c(FALSE, TRUE), is.na(got))
})


## trim_string
test_that("strings can be trimmed", {
  x <- c("abcd", "a", "", "xy-", "zzz")
  got <- trim_string(x, 2)
  expect_equal(got, c("a.", "a", "", "x.", "z."))
  got.2 <- trim_string(x, 2, word.wise = TRUE)
  expect_equal(got, got.2)
})


## add_in_parens
test_that("annotations in parentheses can be added to a string", {
  x <- c("A07", "B11")
  y <- c("Sodium Bromide", "Calcium Nitrate")
  expect_equal("A07 (Sodium Bromide)", add_in_parens(x, y)[1L])
  expect_equal("A07\n(Sodium Bromide)",
    add_in_parens(x, y, paren.sep = "\n")[1L])
  expect_equal("ZZZ A07 (Sodium Bromide)",
    add_in_parens(x, y, prefix = "ZZZ")[1L])
  expect_equal("ZZZ\nA07\n(Sodium Bromide)",
    add_in_parens(x, y, paren.sep = "\n", prefix = "ZZZ")[1L])
  expect_equal("A07 [Sodium Bromide]", add_in_parens(x, y, brackets = TRUE)[1L])
  expect_equal("B11 (Calcium Nitrate)", add_in_parens(x, y)[2L])
  expect_equal("A07 (Sodium Bromide)", add_in_parens(x, y, 100L)[1L])
  expect_equal("B11 (Calcium Nitrate)", add_in_parens(x, y, 100L)[2L])
  expect_equal("A07 (Sod.)", add_in_parens(x, y, 10L)[1L])
  expect_equal("B11 (Cal.)", add_in_parens(x, y, 10L)[2L])
  expect_equal("A07 (S.)", add_in_parens(x, y, 8L)[1L])
  expect_equal("B11 (C.)", add_in_parens(x, y, 8L)[2L])
  expect_equal("A07", add_in_parens(x, y, 7L)[1L])
  expect_equal("B11", add_in_parens(x, y, 7L)[2L])
  expect_equal("A07 (.)", add_in_parens(x, y, 7L, clean = FALSE)[1L])
  expect_equal("B11 (.)", add_in_parens(x, y, 7L, clean = FALSE)[2L])
})

## add_in_parens
test_that("annotations can be added with word-wise abbreviation", {
  x <- c("A07", "B11")
  y <- c("Sodium Bromide", "Calcium Nitrate")
  got <- add_in_parens(x, y, word.wise = TRUE)
  expect_equal("A07 (Sodium Bromide)", got[1L])
  expect_equal("B11 (Calcium Nitrate)", got[2L])
  got <- add_in_parens(x, y, 10L, word.wise = TRUE)
  expect_equal("A07 (SdB.)", got[1L])
  expect_equal("B11 (ClN.)", got[2L])
  got <- add_in_parens(x, y, 8L, word.wise = TRUE)
  expect_equal("A07 (S.)", got[1L])
  expect_equal("B11 (C.)", got[2L])
  got <- add_in_parens(x, y, 7L, word.wise = TRUE)
  expect_equal("A07", got[1L])
  expect_equal("B11", got[2L])
  got <- add_in_parens(x, y, 7L, word.wise = TRUE, clean = FALSE)
  expect_equal("A07 (.)", got[1L])
  expect_equal("B11 (.)", got[2L])
})


## list2html
test_that("HTML can be recursively generated", {
  x <- list(a = 63, c = list(b = letters, structure(LETTERS, names = letters)))
  got <- list2html(x)
  expect_is(got, "character")
  expect_equal(length(got), 1L)
  got <- strsplit(got, "\\s*<[^>]+>\\s*", perl = TRUE)[[1]]
  expect_true(setequal(got[nzchar(got)],
    c(63, LETTERS, paste(letters, collapse = " "))))
})


## single_tag
## UNTESTED


## html_head
## UNTESTED


## tidy
## UNTESTED


################################################################################
################################################################################
#
# Mapping functions
#


## as
## UNTESTED


## prepare_class_names
## UNTESTED


################################################################################
################################################################################
#
# YAML reparation
#

## repair_na_strings
test_that("NAs in a character vectors can be repaired", {
  # old style
  x <- c("abc", " ", "NA", " NA", "           NA", "123", "NA ")
  got <- repair_na_strings(x)
  expect_equal(got, c("abc", " ", NA, NA, NA, "123", "NA "))
  # new style (YAML >= 2.1.7)
  x <- c("abc", " ", ".na.real", ".na.character", ".na", "123", ".na.integer")
  got <- repair_na_strings(x)
  expect_equal(got, c("abc", " ", NA, NA, NA, "123", NA))
})

## repair_na_strings
test_that("NAs in a list can be repaired", {

  x <- list(a = 99, b = list(xx = c("NA", "99.5", "1e+06")), c = 8,
    d = c(".na.real", "Z"))
  wanted <- list(a = 99, b = list(xx = c(NA_real_, 99.5, 1000000)), c = 8,
    d = c(NA, "Z"))

  got <- repair_na_strings(x)
  expect_equal(wanted, got)

  got <- repair_na_strings(x, "double")
  expect_equal(wanted, got)

  got <- repair_na_strings(x, "integer")
  wanted$b$xx <- c(NA_integer_, as.integer(x$b$xx[2L:3L]))
  expect_equal(wanted, got)

  got <- repair_na_strings(x, "complex")
  wanted$b$xx <- c(NA_complex_, as.complex(x$b$xx[2L:3L]))
  expect_equal(wanted, got)

})


## rescue_dots
test_that("rescue_dots re-inserts dots but only where necessary", {
  x <- 1:10
  expect_equal(rescue_dots(x), x)
  x <- c("A_B", "A.B", "_AB", "_A_B", "__AB_", NA, "")
  got <- rescue_dots(x)
  expect_equal(got, c("A_B", "A.B", "_AB", "A.B", ".AB.", NA, ""))
})


################################################################################
################################################################################
#
# Lists
#


## insert
test_that("a list can be inserted in a list", {
  x <- list(a = 9, b = 17, k = 88)
  y <- list(b = -17, k = 0)

  got <- insert(x, y, .force = FALSE)
  expect_equal(x, got)

  got <- insert(x, y, .force = TRUE)
  expect_equal(got, list(a = 9, b = -17, k = 0))

  z <- list(x = NULL, xx = "318")
  got <- insert(x, c(y, z), .force = FALSE)
  expect_equal(c(x, z), got)
})

## insert
test_that("anything and nothing can be inserted in a list", {
  x <- list(a = 9, b = 17:18, k = 88)
  y <- list(b = -17, k = 0)

  got <- insert(x, b = -17, k = 0:3, .force = FALSE)
  expect_equal(x, got)

  got <- insert(x, b = -17, k = 0:3, .force = TRUE)
  expect_equal(got, list(a = 9, b = -17, k = 0:3))

  z <- list(x = NULL, xx = "318")
  got <- insert(x, x = NULL, xx = "318", .force = FALSE)
  expect_equal(c(x, z), got)

  got <- insert(x)
  expect_equal(x, got)
})


################################################################################
################################################################################
#
# update()
#


## update
test_that("character-matrix objects can be updated by converting NAs", {

  data <- matrix(1:10, ncol = 2L)
  rownames(data) <- paste("taxon", 1L:5L, sep = "_")
  colnames(data) <- paste("char", 1L:2L, sep = "_")
  data <- discrete(data, TRUE, TRUE)
  data <- as(data, "CMAT")

  for (x in list(data, data + 1L, data * 2L)) {
    expect_true(anyNA(x))
    got <- update(x)
    expect_equal(length(unique(c(x))), length(unique(c(got))))
    expect_false(anyNA(got))

    expect_true(all(got[is.na(x)] > min(got, na.rm = TRUE)))
    expect_true(all(got[is.na(x)] < max(got, na.rm = TRUE)))

    wanted <- x == min(x, na.rm = TRUE)
    wanted <- wanted & !is.na(wanted)
    expect_true(all(x[wanted] == got[wanted]))

    wanted <- max(x, na.rm = TRUE)
    wanted <- x == wanted
    wanted <- wanted & !is.na(wanted)
    expect_true(all(x[wanted] <= got[wanted]))
  }

})

## update
test_that("character-matrix objects can be updated by deletion", {

  #x <- matrix(data = 1:10, ncol = 2L)
  x <- matrix(data = c(1L, 1L, 1L, 1L, NA, NA, NA, 2L, 2L, 2L), ncol = 2L)
  rownames(x) <- paste("taxon", 1L:5L, sep = "_")
  colnames(x) <- paste("char", 1L:2L, sep = "_")
  #x <- discrete(x, TRUE, TRUE)
  x <- as(x, "CMAT")

  got <- update(x, "delete.ambig")
  expect_equal(got, x) # no ambiguities
  got <- update(x, "delete.ambig")
  expect_equal(got, x, na.rm = FALSE) # na.rm no effect here

  got <- update(x, "delete.const")
  expect_equal(length(got), 0L)
  got <- update(x, "delete.const")
  expect_equal(length(got), 0L, na.rm = FALSE)

  got <- update(x, "delete.uninf")
  expect_equal(length(got), 0L)
  got <- update(x, "delete.uninf")
  expect_equal(length(got), 0L, na.rm = FALSE)

  y <- merge(x, c(1, 1, 2, 2, 2))

  got <- update(y, "delete.ambig")
  expect_equal(got, y)
  got <- update(y, "delete.ambig", na.rm = FALSE)
  expect_equal(dim(got), c(2, 1))

  got <- update(y, "delete.const")
  expect_equal(dim(got), c(2, 0))
  got <- update(y, "delete.const", na.rm = FALSE)
  expect_equal(got, y)

  got <- update(y, "delete.uninf")
  expect_equal(dim(got), c(2, 0))
  got <- update(y, "delete.uninf", na.rm = FALSE)
  expect_equal(dim(got), c(2, 1))

})


## remove_concentration
## UNTESTED


## get_partial_match
## UNTESTED

