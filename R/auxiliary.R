setGeneric("separate", function(object, ...) standardGeneric("separate"))

setMethod("separate", "character", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, list.wise = FALSE,
    strip.white = list.wise) {

  strip_white <- function(x) sub("\\s+$", "", sub("^\\s+", "", x, FALSE, TRUE),
    FALSE, TRUE)

  p0 <- function(x) paste0(x, collapse = "")

  simple_if <- function(x, keep.const, simplify) {
    if (is.matrix(x)) {
      if (!keep.const) {
        if (all(const <- is_constant(x, 2L)) && simplify)
          x <- x[, 1L, drop = FALSE]
        else
          x <- x[, !const, drop = FALSE]
      }
      if (simplify && ncol(x) == 1L)
        x[, 1L]
      else
        x
    } else if (simplify)
      x
    else if (length(x))
      matrix(x)
    else
      matrix(NA_character_, 0L, 0L)
  }

  # create regexp for splitting
  char_group <- function(single, multiple) {
    if (length(single))
      if (length(multiple))
        sprintf("([%s]|[%s]+)", p0(single), p0(multiple))
      else
        sprintf("[%s]", p0(single))
    else if (length(multiple))
      sprintf("[%s]+", p0(multiple))
    else
      NA_character_ # does not split at all
  }

  # splitting at positions that contain whitespace in all strings
  split_fixed <- function(x) {
    ws <- c(" ", "\t", "\v", "\r", "\n", "\b", "\a", "\f")
    x <- strsplit(x, "", TRUE)
    max.len <- max(lengths(x, FALSE))
    x <- lapply(x, function(y) c(y, rep.int(" ", max.len - length(y))))
    x <- do.call(rbind, x)
    groups <- sections(apply(x, 2L, function(y) all(y %in% ws)))
    x <- apply(x, 1L, split.default, groups)
    x <- lapply(x, function(y) strip_white(vapply(y, p0, "")))
    do.call(rbind, x)
  }

  yields_constant <- function(char, x) {
    splits_constant <- function(char, x, ...)
      is_constant(lengths(strsplit(x, char, ...), FALSE))
    if (splits_constant(sprintf("[%s]+", char), x, FALSE, TRUE))
      2L
    else if (splits_constant(char, x, TRUE))
      1L
    else
      0L
  }

  # collect words after splitting and mark their occurrences
  word_occurrences <- function(x, split, strip.white) {
    x <- strsplit(x, sprintf("[%s]", p0(split)), FALSE, TRUE)
    if (strip.white)
      x <- lapply(x, strip_white)
    chars <- unlist(x, FALSE)
    chars <- unique.default(chars[!is.na(chars)])
    result <- matrix(FALSE, length(x), length(chars))
    colnames(result) <- sort.int(chars)
    rownames(result) <- names(x)
    for (i in seq_along(x))
      if (identical(x[[i]], NA_character_))
        result[i, ] <- NA
      else
        result[i, x[[i]]] <- TRUE
    result
  }

  LL(list.wise, strip.white, simplify, keep.const)

  # Fixed-width splitting mode
  if (identical(TRUE, split <- c(split)))
    return(simple_if(split_fixed(object), keep.const, simplify))
  split <- as.character(split)
  if (all(!nzchar(split <- split[!is.na(split)])))
    return(simple_if(split_fixed(object), keep.const, simplify))

  # Prepare split characters
  split <- unique.default(unlist(strsplit(split, "", TRUE), FALSE, FALSE))
  if (!length(split))
    return(simple_if(object, keep.const, simplify))
  split <- c(setdiff(split, "-"), intersect(split, "-"))

  # List-wise splitting
  if (list.wise)
    return(simple_if(word_occurrences(object, split, strip.white),
      keep.const, simplify))

  # Check and apply split characters
  yields.const <- vapply(split, yields_constant, 0L, object)
  split <- char_group(split[yields.const == 1L], split[yields.const == 2L])
  object <- do.call(rbind, strsplit(object, split, FALSE, TRUE))
  if (strip.white)
    object[] <- strip_white(object)
  simple_if(object, keep.const, simplify)

}, sealed = SEALED)

setMethod("separate", "factor", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, ...) {
  result <- separate(as.character(object), split = split,
    keep.const = keep.const, simplify = FALSE, ...)
  if (L(simplify) && ncol(result) == 1L)
    as.factor(result[, 1L])
  else
    as.data.frame(result, stringsAsFactors = TRUE, optional = TRUE)
}, sealed = SEALED)

setMethod("separate", "data.frame", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, coerce = TRUE, name.sep = ".", ...) {
  LL(coerce, name.sep, simplify)
  object <- do.call(cbind, mapply(function(x, name) {
    result <- if (is.character(x))
      as.data.frame(separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...), stringsAsFactors = FALSE, optional = TRUE)
    else if (coerce && is.factor(x))
      separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...)
    else
      as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    case(ncol(result),
      if (keep.const)
        result[, name] <- x,
      names(result) <- name,
      names(result) <- paste(name, seq_len(ncol(result)), sep = name.sep)
    )
    result
  }, object, names(object), SIMPLIFY = FALSE, USE.NAMES = FALSE))
  if (ncol(object) == 1L && simplify)
    object <- object[, 1L]
  object
}, sealed = SEALED)

setGeneric("opm_opt", function(x, ...) standardGeneric("opm_opt"))

setMethod("opm_opt", "list", function(x) {
  old <- mget(names(x), OPM_OPTIONS) # fails if names are missing
  for (i in seq_along(x)) {
    if (!length(value <- x[[i]]))
      stop("empty value provided for key '%s'", names(x)[i])
    if (!all(inherits(value, class(old[[i]]), TRUE)))
      stop(sprintf("new and old value have conflicting class(es) for key '%s'",
        names(x)[i]))
  }
  list2env(x, OPM_OPTIONS)
  invisible(old)
}, sealed = SEALED)

setMethod("opm_opt", "missing", function(x, ...) {
  if (nargs())
    opm_opt(list(...))
  else
    as.list(OPM_OPTIONS)
}, sealed = SEALED)

setMethod("opm_opt", "character", function(x) {
  get(x, , OPM_OPTIONS, "any", FALSE)
}, sealed = SEALED)

setGeneric("param_names", function(what) standardGeneric("param_names"))

setMethod("param_names", "missing", function(what) {
  CURVE_PARAMS
}, sealed = SEALED)

setMethod("param_names", "character", function(what) {
  switch(match.arg(what,
    c("param.names", "disc.name", "reserved.md.names", "split.at")),
    param.names = CURVE_PARAMS,
    disc.name = DISC_PARAM,
    reserved.md.names = unname(RESERVED_NAMES),
    split.at = RESERVED_NAMES[["parameter"]])
}, sealed = SEALED)

