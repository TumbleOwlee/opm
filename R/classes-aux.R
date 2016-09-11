setMethod("initialize", "OPM", function(.Object, ...) {
  .Object <- callNextMethod()
  plate.type <- CSV_NAMES[["PLATE_TYPE"]]
  .Object@csv_data[plate.type] <- plate_type(.Object@csv_data[plate.type])
  .Object
}, sealed = SEALED)

setMethod("initialize", "OPMS", function(.Object, ...) {
  .Object <- callNextMethod()
  names(.Object@plates) <- NULL
  .Object
}, sealed = SEALED)

setMethod("initialize", "CMAT", function(.Object, ...) {
  map2int <- function(x) match(toupper(x), CHARACTER_STATES)
  .Object <- callNextMethod()
  switch(EXPR = typeof(.Object),
    character = {
      .Object[] <- map2int(.Object)
      storage.mode(.Object) <- "integer"
    },
    list = {
      if (length(.Object) && typeof(.Object[[1L]]) == "character")
        .Object[] <- lapply(.Object, map2int)
      .Object[] <- lapply(.Object, sort.int, NULL, TRUE)
    },
    logical = .Object[] <- .Object + 1L
  )
  .Object
}, sealed = SEALED)

setGeneric("opm_problems",
  function(object, ...) standardGeneric("opm_problems"))

setMethod("opm_problems", "matrix", function(object) {
  errs <- character()
  # Check content
  if (anyNA(object))
    errs <- c(errs, "matrix contains NAs")
  if (!is.numeric(object))
    errs <- c(errs, "matrix is not numeric")
  # Check row names
  if (!is.null(rownames(object)))
    errs <- c(errs, "non-empty row names")
  # Check column names
  col.names <- colnames(object)
  # Pattern must be in sync with clean_coords() and is_coord()
  pattern <- sprintf("^([A-H][01]\\d|%s)$", HOUR)
  if (length(bad <- grep(pattern = pattern, x = col.names, invert = TRUE,
      value = TRUE)))
    errs <- c(errs, paste("invalid entry in header:", bad[1L]))
  if (bad <- anyDuplicated(col.names))
    errs <- c(errs, paste("duplicated entry in header:", col.names[bad]))
  if (col.names[1L] != HOUR)
    errs <- c(errs, paste("first entry in header must be", HOUR))
  if (is.unsorted(col.names[-1L]))
    errs <- c(errs, "names of wells must be sorted")
  if (!length(errs) && nrow(object) > 1L &&
      any(bad <- is_constant(object, 2L, FALSE)))
    warning("possible artefact(s): constant data in column(s) ",
      paste0(names(bad)[bad], collapse = ", "))
  errs
}, sealed = SEALED)

setMethod("opm_problems", "character", function(object) {
  errs <- character()
  wanted <- CSV_NAMES[c("FILE", "PLATE_TYPE", "POS", "SETUP")]
  missing <- !wanted %in% names(object)
  if (any(missing))
    errs <- c(errs, sprintf("need '%s' in CSV data", wanted[missing]))
  errs
}, sealed = SEALED)

setGeneric("opma_problems",
  function(object, ...) standardGeneric("opma_problems"))

setMethod("opma_problems", "matrix", function(object, orig.data, settings) {
  errs <- character()
  # Check content. In contrast to the raw measurements we have to allow NAs.
  if (!is.numeric(object))
    errs <- c(errs, "aggregated values are not numeric")
  # Compare column names with non-aggregated data
  cols <- colnames(object)
  if (length(bad <- cols[colnames(orig.data)[-1] != cols]))
    errs <- c(errs, paste("unknown column name in aggregated data:", bad))
  if (!nrow(object)) {
    errs <- c(errs, "no rows in aggregated data")
    return(errs) # further checks are impossible in that case
  }
  if (!is.null(settings) && settings[[SOFTWARE]] == opm_string()) {
    method <- settings[[METHOD]]
    if (method %in% KNOWN_METHODS$aggregation) {
      # Check row names
      got <- rownames(object)
      bad <- got[got != map_param_names()]
      if (length(bad))
        errs <- c(errs, paste("missing row name in aggregated data:", bad))
    } else
      errs <- c(errs, sprintf("unknown aggregation method '%s'", method))
  }
  errs
}, sealed = SEALED)

setMethod("opma_problems", "list", function(object) {
  check_string <- function(what) {
    if (length(x <- object[[what]]) == 1L && is.character(x) && !is.na(x))
      character()
    else
      sprintf("'%s' entry not a non-NA character scalar", what)
  }
  errs <- character()
  for (name in c(SOFTWARE, METHOD, VERSION))
    errs <- c(errs, check_string(name))
  if (!is.list(options <- object[[OPTIONS]]) || !length(options))
    errs <- c(errs, sprintf("non-empty list as '%s' entry needed", OPTIONS))
  else if (is.null(names(options)) || any(!nzchar(names(options))))
    errs <- c(errs, sprintf("all '%s' elements must be named", OPTIONS))
  bad <- setdiff(names(object), c(METHOD, OPTIONS, SOFTWARE, VERSION))
  if (length(bad))
    errs <- c(errs, paste("unknown settings key:", bad[1L]))
  errs
}, sealed = SEALED)

setGeneric("opmd_problems",
  function(object, ...) standardGeneric("opmd_problems"))

setMethod("opmd_problems", "list", function(object) {
  opma_problems(object)
}, sealed = SEALED)

setMethod("opmd_problems", "matrix", function(object, disc, param) {
  errs <- character()
  # uncomment this once numeric vectors are allowed, too:
  #if (!is.vector(disc) || !(is.numeric(disc) || is.logical(disc)))
  #  errs <- c(errs, "discretized data have wrong storage mode")
  if (!identical(names(disc), colnames(object)))
    errs <- c(errs, "discretized data have wrong names")
  if (length(param) != 1L || !param %in% rownames(object))
    errs <- c(errs, "missing name of discretized parameter")
  if (length(errs))
    return(errs) # further tests are impossible in these cases
  ok <- !is.na(disc)
  ok <- identical(order(disc[ok], object[param, ok]), order(object[param, ok]))
  if (!ok) {
    text <- sprintf("discretized data inconsistent with '%s' parameter", param)
    if (get("strict.OPMD", OPM_OPTIONS))
      errs <- c(errs, text)
    else
      warning(text, call. = FALSE)
  }
  errs
}, sealed = SEALED)

setGeneric("opms_problems",
  function(object, ...) standardGeneric("opms_problems"))

setMethod("opms_problems", "list", function(object) {
  errs <- character()
  if (length(object) < 2L) {
    errs <- c(errs, "less than two plates submitted")
    return(errs) # further checks are useless in that case
  }
  if (length(no.opm <- which(!vapply(object, is, NA, "OPM"))) > 0L) {
    bad.classes <- unlist(lapply(object[no.opm], class))
    errs <- c(errs, paste("wrong class:", bad.classes))
    return(errs) # further checks are impossible in that case
  }
  if (!isTRUE(isuni <- is_uniform(vapply(object, plate_type, ""))))
    errs <- c(errs, paste("plate types are not uniform:",
      paste0(isuni, collapse = " <=> ")))
  if (!isTRUE(is_uniform(lapply(object, wells))))
    errs <- c(errs, "wells are not uniform")
  if (!length(errs) &&
      !isTRUE(is_uniform(lapply(X = object, FUN = hours, what = "all"))))
    warning("running times are not uniform")
  errs
}, sealed = SEALED)

setGeneric("attach_attr", function(object, ...) standardGeneric("attach_attr"))

setMethod("attach_attr", "OPM", function(object, other) {
  for (name in setdiff(slotNames(object), "measurements"))
    attr(other, name) <- slot(object, name)
  other
}, sealed = SEALED)

setGeneric("update_settings_list",
  function(x, ...) standardGeneric("update_settings_list"))

setMethod("update_settings_list", "list", function(x) {
  if (is.null(names(x)))
    stop("expected named list 'x'")
  x <- map_names(x, rescue_dots)
  if (!length(software <- x[[SOFTWARE]])) {
    x[[SOFTWARE]] <- software <- opm_string()
    warning(sprintf("inserting '%s' as '%s' entry", software, SOFTWARE))
  }
  if (!length(version <- x[[VERSION]])) {
    x[[VERSION]] <- version <- if (software == opm_string())
      opm_string(version = TRUE)[2L]
    else
      UNKNOWN_VERSION
    warning(sprintf("inserting '%s' as '%s' entry", version, VERSION))
  }
  if (m <- match(PROGRAM, names(x), nomatch = 0L)) {
    names(x)[m] <- METHOD
    warning(sprintf("renaming '%s' to '%s'", PROGRAM, METHOD))
  }
  x
}, sealed = SEALED)

setGeneric("rename_wells",
  function(object, keys) standardGeneric("rename_wells"))

setMethod("rename_wells", c("OPM", "ANY"), function(object, keys) {
  colnames(object@measurements)[-1L] <- keys
  object
}, sealed = SEALED)

setMethod("rename_wells", c("OPMA", "ANY"), function(object, keys) {
  object <- callNextMethod()
  colnames(object@aggregated) <- keys
  object
}, sealed = SEALED)

setMethod("rename_wells", c("OPMS", "ANY"), function(object, keys) {
  object <- callNextMethod()
  names(object@discretized) <- keys
  object
}, sealed = SEALED)

