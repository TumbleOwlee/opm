to_opm_list <- function(object, ...) UseMethod("to_opm_list")

to_opm_list.list <- function(object, precomputed = TRUE, skip = FALSE,
    group = FALSE) {
  LL(precomputed, skip, group)
  opmd.slots <- setdiff(slotNames(OPMD), opma.slots <- slotNames(OPMA))
  opma.slots <- setdiff(opma.slots, opm.slots <- slotNames(OPM))
  convert_recursively <- function(item) {
    if (!is.list(item))
      if (skip)
        return(NULL)
    else
      stop("non-list element encountered")
    keys <- names(item)
    if (all(opm.slots %in% keys))
      as(item, if (all(opma.slots %in% keys))
        if (all(opmd.slots %in% keys))
          OPMD
        else
          OPMA
        else
          OPM)
    else
      lapply(item, FUN = convert_recursively)
  }
  get_plates <- function(item) {
    if (is(item, OPM))
      item
    else if (is(item, OPMS))
      plates(item)
    else if (skip)
      NULL
    else
      stop("need object derived from ", OPM, " or ", OPMS, ", got ",
        class(item)[1L])
  }
  result <- if (precomputed)
    rapply(object, f = get_plates, how = "unlist")
  else
    c(convert_recursively(object), recursive = TRUE)
  if (group)
    result <- split(result, vapply(result, plate_type, ""))
  result
}

try_opms <- function(object, ...) UseMethod("try_opms")

try_opms.list <- function(object, precomputed = TRUE, skip = FALSE) {
  tryCatch(
    new(OPMS, plates = to_opm_list.list(object, precomputed, skip, FALSE)),
    error = function(e) object)
}

setMethod("[<-", c(OPMS, "ANY", "missing", "NULL"), function(x, i, j, value) {
  x@plates[i] <- NULL
  case(length(x@plates), NULL, x@plates[[1L]], x) # no check necessary here
}, sealed = SEALED)

setMethod("[<-", c(OPMS, "ANY", "missing", OPM), function(x, i, j, value) {
  x@plates[i] <- value
  new(OPMS, plates = x@plates) # check needed
}, sealed = SEALED)

setMethod("[<-", c(OPMS, "ANY", "missing", OPMS), function(x, i, j, value) {
  x@plates[i] <- value@plates
  new(OPMS, plates = x@plates) # check needed
}, sealed = SEALED)

setMethod("c", OPMX, function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  try_opms.list(list(x, ...))
}, sealed = SEALED)

setMethod("+", c(OPM, OPM), function(e1, e2) {
  new(OPMS, plates = list(e1, e2))
}, sealed = SEALED)

setMethod("+", c(OPM, OPMS), function(e1, e2) {
  new(OPMS, plates = c(list(e1), plates(e2)))
}, sealed = SEALED)

setMethod("+", c(OPM, "list"), function(e1, e2) {
  new(OPMS, plates = c(list(e1), e2))
}, sealed = SEALED)

setMethod("+", c(OPMS, OPMS), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), plates(e2)))
}, sealed = SEALED)

setMethod("+", c(OPMS, OPM), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), e2))
}, sealed = SEALED)

setMethod("+", c(OPMS, "list"), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), e2))
}, sealed = SEALED)

opms <- function(..., precomputed = TRUE, skip = FALSE, group = FALSE) {
  opms_or_first_or_NULL <- function(x) {
    case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
  }
  if (is.character(group)) {
    wanted <- plate_type(group) # for normalization
    group <- TRUE
  } else {
    wanted <- NULL
    group <- as.logical(group)
  }
  # to_opm_list() checks the argument lengths
  result <- to_opm_list.list(list(...), precomputed, skip, group)
  if (is.null(wanted)) {
    if (group)
      lapply(result, opms_or_first_or_NULL)
    else
      opms_or_first_or_NULL(result)
  } else # group was TRUE in that case, and to_opm_list() split the list
    opms_or_first_or_NULL(result[[wanted]])
}

