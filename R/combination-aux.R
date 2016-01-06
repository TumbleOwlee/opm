to_opm_list <- function(object, ...) UseMethod("to_opm_list")

to_opm_list.list <- function(object, precomputed = TRUE, skip = FALSE,
    group = FALSE) {
  LL(precomputed, skip, group)
  opmd.slots <- setdiff(slotNames("OPMD"), opma.slots <- slotNames("OPMA"))
  opma.slots <- setdiff(opma.slots, opm.slots <- slotNames("OPM"))
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
          "OPMD"
        else
          "OPMA"
        else
          "OPM")
    else
      lapply(item, FUN = convert_recursively)
  }
  get_plates <- function(item) {
    if (is(item, "OPM"))
      item
    else if (is(item, "OPMS"))
      plates(item)
    else if (skip)
      NULL
    else
      stop("need object derived from 'OPM' or 'OPMS', got ", class(item)[1L])
  }
  result <- if (precomputed)
    rapply(object, get_plates, "ANY", NULL, "unlist")
  else
    c(convert_recursively(object), recursive = TRUE)
  if (group)
    result <- if (is.null(result))
        list()
      else
        result <- split.default(result, vapply(result, plate_type, ""))
  result
}

try_opms <- function(object, ...) UseMethod("try_opms")

try_opms.list <- function(object, precomputed = TRUE, skip = FALSE) {
  tryCatch(
    new("OPMS", plates = to_opm_list.list(object, precomputed, skip, FALSE)),
    error = function(e) object)
}

