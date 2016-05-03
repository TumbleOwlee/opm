setMethod("[<-", c("OPMS", "ANY", "missing", "NULL"), function(x, i, j,
    value) {
  x@plates[i] <- NULL
  case(length(x@plates), NULL, x@plates[[1L]], x) # no checks necessary here
}, sealed = SEALED)

setMethod("[<-", c("OPMS", "ANY", "missing", "OPM"), function(x, i, j, value) {
  x@plates[i] <- list(value) # checks and unnaming needed
  new(Class = "OPMS", plates = close_index_gaps(x@plates))
}, sealed = SEALED)

setMethod("[<-", c("OPMS", "ANY", "missing", "OPMS"), function(x, i, j, value) {
  x@plates[i] <- value@plates # checks and unnaming needed
  new(Class = "OPMS", plates = close_index_gaps(x@plates))
}, sealed = SEALED)

setMethod("[<-", c("OPMS", "ANY", "missing", "list"), function(x, i, j, value) {
  x@plates[i] <- value # checks and unnaming needed
  new(Class = "OPMS", plates = close_index_gaps(x@plates))
}, sealed = SEALED)

setMethod("[<-", c("MOPMX", "ANY", "missing", "OPMX"), function(x, i, ...,
    value) {
  x[i] <- list(value)
  x
})

setMethod("[<-", c("MOPMX", "ANY", "missing", "ANY"), function(x, i, ...,
    value) {
  x <- callNextMethod(x, i, ..., value)
  if (any(bad <- vapply(x, is.null, NA))) {
    warning("closing gaps in indexes", call. = FALSE)
    x <- x[!bad]
  }
  validObject(x)
  x
})

setMethod("[[<-", c("MOPMX", "ANY", "missing", "ANY"), function(x, i, ...,
    value) {
  x <- callNextMethod(x, i, ..., value)
  if (any(bad <- vapply(x, is.null, NA))) {
    warning("closing gaps in indexes", call. = FALSE)
    x <- x[!bad]
  }
  validObject(x)
  x
})

setMethod("$<-", c("MOPMX", "ANY"), function(x, name, value) {
  x[[name]] <- value
  x
})

setMethod("c", "OPMX", function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  try_opms.list(c(list(x), ..., recursive = recursive))
}, sealed = SEALED)

setMethod("c", "MOPMX", function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  y <- as.list(c(x@.Data, ..., recursive = recursive))
  tryCatch(expr = new(class(x), y), error = function(e) y)
}, sealed = SEALED)

setMethod("+", c("OPM", "OPM"), function(e1, e2) {
  new(Class = "OPMS", plates = list(e1, e2))
}, sealed = SEALED)

setMethod("+", c("OPM", "OPMS"), function(e1, e2) {
  e2@plates <- c(list(e1), e2@plates)
  validObject(e2)
  e2
}, sealed = SEALED)

setMethod("+", c("OPM", "MOPMX"), function(e1, e2) {
  e2@.Data <- c(list(e1), e2@.Data)
  e2
}, sealed = SEALED)

setMethod("+", c("OPM", "list"), function(e1, e2) {
  new(Class = "OPMS", plates = c(list(e1), e2))
}, sealed = SEALED)

setMethod("+", c("OPMS", "OPMS"), function(e1, e2) {
  e1@plates[seq_along(e2@plates) + length(e1@plates)] <- e2@plates
  validObject(e1)
  e1
}, sealed = SEALED)

setMethod("+", c("OPMS", "OPM"), function(e1, e2) {
  e1@plates <- c(e1@plates, list(e2))
  validObject(e1)
  e1
}, sealed = SEALED)

setMethod("+", c("OPMS", "MOPMX"), function(e1, e2) {
  e2@.Data <- c(list(e1), e2@.Data)
  e2
}, sealed = SEALED)

setMethod("+", c("OPMS", "list"), function(e1, e2) {
  new(Class = "OPMS", plates = c(e1@plates, e2)) # unnaming also needed
}, sealed = SEALED)

setMethod("+", c("MOPMX", "OPMX"), function(e1, e2) {
  e1@.Data <- c(e1@.Data, list(e2))
  e1
}, sealed = SEALED)

setMethod("+", c("MOPMX", "ANY"), function(e1, e2) {
  e1@.Data <- c(e1@.Data, as(e2, class(e1))@.Data)
  e1
}, sealed = SEALED)

setMethod("+", c("ANY", "MOPMX"), function(e1, e2) {
  e2@.Data <- c(as(e1, class(e2))@.Data, e2@.Data)
  e2
}, sealed = SEALED)

opms <- function(..., precomputed = TRUE, skip = FALSE, group = FALSE) {
  opms_or_first_or_NULL <- function(x) case(length(x), NULL, x[[1L]],
    new(Class = "OPMS", plates = x))
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
      new("MOPMX", lapply(result, opms_or_first_or_NULL))
    else
      opms_or_first_or_NULL(result)
  } else # group was TRUE in that case, and to_opm_list() has split the list
    opms_or_first_or_NULL(result[[wanted]])
}

