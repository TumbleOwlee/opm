NULL

print.OPMD_Listing <- function(x, ...) {
  cat(formatDL(x = names(x), y = x, ...), sep = "\n")
  invisible(x)
}

print.OPMS_Listing <- function(x, ...) {
  for (name in rownames(x)) {
    cat(name, gsub(".", "-", name, FALSE, TRUE), sep = "\n")
    cat(formatDL(x = colnames(x), y = x[name, ], ...), sep = "\n")
    cat("\n")
  }
  invisible(x)
}

print.OPM_Summary <- function(x, ...) {
  lapply(X = formatDL(x = names(x), y = unlist(x), ...), FUN = cat, sep = "\n")
  invisible(x)
}

print.OPMS_Summary <- function(x, ...) {
  for (i in seq_along(x)) {
    cat(i, sep = "\n")
    print(x[[i]])
    cat("\n")
  }
  tmpl <- "=> %s object with %i plates (%i aggregated, %i discretized)"
  tmpl <- paste(tmpl, "of type '%s', %i well(s) and about %i time point(s).")
  y <- attr(x, "overall")
  cat(sprintf(tmpl, "OPMS", y$Dimensions[1L], y$Aggregated, y$Discretized,
    y$Plate.type, y$Dimensions[3L], y$Dimensions[2L]), sep = "\n")
  invisible(x)
}

print.MOPMX_Summary <- function(x, ...) {
  NextMethod()
  cat("", sprintf(
    "=> MOPMX object with %i element(s), details are shown above.", nrow(x)),
    " Access the elements with [[ or $ to apply specific methods.",
    sep = "\n")
  invisible(x)
}

print.print_easy <- function(x, ...) {
  to_map <- function(items) if (is.null(names(items)))
    items
  else
    as.list(items)
  cat(as.yaml(if (is.list(x))
    rapply(x, to_map, "ANY", NULL, "replace")
  else
    to_map(x)))
  invisible(x)
}

setGeneric("ranging", function(object, ...) standardGeneric("ranging"))

setMethod("ranging", "numeric", function(object, extended = !zscores,
    zscores = FALSE, na.rm = TRUE, fac = 1) {
  LL(extended, zscores, na.rm)
  result <- if (zscores) {
    if (extended) {
      center <- median(object, na.rm = na.rm)
      (object - center) / mad(object, center = center, na.rm = na.rm)
    } else {
      (object - mean(object, na.rm = na.rm)) / sd(object, na.rm = na.rm)
    }
  } else {
    if (extended) {
      min.object <- min(object, na.rm = na.rm)
      (object - min.object) / (max(object, na.rm = na.rm) - min.object)
    } else {
      object / max(abs(object), na.rm = na.rm)
    }
  }
  must(result * fac)
}, sealed = SEALED)

setMethod("ranging", "array", function(object, ...) {
  map_values(object = object, mapping = ranging, ...)
}, sealed = SEALED)

setMethod("ranging", "list", function(object, ...) {
  relist(ranging(unlist(as.relistable(object)), ...))
}, sealed = SEALED)

setGeneric("guess_cex", function(object, ...) standardGeneric("guess_cex"))

setMethod("guess_cex", "numeric", function(object) {
  0.5 * sqrt(96 / object)
}, sealed = SEALED)

setGeneric("best_layout",
  function(object, ...) standardGeneric("best_layout"))

setMethod("best_layout", "numeric", function(object, by = 0.75) {
  LL(object, by)
  if (object < 0)
    stop("a negative number of fields makes no sense")
  if (object < 2)
    return(c(object, object))
  large <- ceiling(sqrt((1 / by) * object)) # => error unless 'by' is numeric
  small <- ceiling(object / large)
  c(large, small)
}, sealed = SEALED)

setGeneric("best_range",
  function(object, ...) standardGeneric("best_range"))

setMethod("best_range", "numeric", function(object, target,
    align = c("center", "left", "right"),
    offset = 0, prop.offset = 0) {
  orig.range <- range(object)
  orig.diff <- orig.range[2L] - orig.range[1L]
  case(length(target), target <- orig.diff, LL(target))
  LL(offset, prop.offset)
  if (offset == 0)
    offset <- target * prop.offset
  total <- target + 2 * offset
  if (total < orig.diff) {
    fmt <- "target (%s) + 2 * offset (%s) smaller than original range (%s)"
    stop(sprintf(fmt, target, offset, orig.diff))
  }
  case(match.arg(align),
    center = {
      add <- total / 2
      mean(orig.range) + c(-add, add)
    },
    left = orig.range[1L] + c(-offset, target + offset),
    right = orig.range[2L] + c(-target - offset, offset)
  )
}, sealed = SEALED)

setGeneric("improved_max",
  function(object, ...) standardGeneric("improved_max"))

setMethod("improved_max", "numeric", function(object, by = 10) {
  LL(by)
  m <- max(object)
  while (by >= m)
    by <- by / 10
  ceiling(m / by) * by + by # => error unless 'by' is numeric
}, sealed = SEALED)

setMethod("improved_max", "OPMX", function(object, theor.max = TRUE, by = 10) {
  if (is.double(L(theor.max)))
    return(theor.max)
  if (theor.max)
    return(THEOR_RANGE[[2L]])
  improved_max(max(object), by)
}, sealed = SEALED)

setGeneric("draw_ci", function(object, ...) standardGeneric("draw_ci"))

setMethod("draw_ci", "numeric", function(object, col = "blue", cex = 1,
    na.action = c("warn", "error", "ignore")) {
  LL(object, .wanted = 4L)
  if (anyNA(c(left <- object[1L], right <- object[3L]))) {
    msg <- "cannot draw CI because left or right margin is 'NA'"
    case(match.arg(na.action),
      warn = warning(msg),
      error = stop(msg),
      ignore = NULL
    )
  }
  if (is.na(y <- object[4L]))
    stop("position on y axis must be provided")
  segments(x0 = left, y0 = y, x1 = right, y1 = y, lwd = cex, col = col)
  text(x = left, y = y, labels = "(", col = col, cex = cex)
  text(x = right, y = y, labels = ")", col = col, cex = cex)
  if (!is.na(point <- object[2L]))
    points(x = point, y = y, col = col, lwd = cex, pch = 19L, cex = cex)
  invisible(object)
}, sealed = SEALED)

setGeneric("negative_control",
  function(object, ...) standardGeneric("negative_control"))

setMethod("negative_control", "OPMX", function(object, neg.ctrl) {
  if (!length(neg.ctrl) || is.numeric(neg.ctrl))
    neg.ctrl
  else if (is.character(neg.ctrl)) {
    result <- vapply(neg.ctrl, function(x)
      tryCatch(expr = minmax(object, neg.ctrl), error = function(e) {
        warning("cannot get negative control from selected position ",
        "(deleted?); error was: ", conditionMessage(e), call. = FALSE)
        -1
      }), 0)
    result[result >= 0]
  } else if (is.logical(neg.ctrl)) {
    if (L(neg.ctrl))
      minmax(object)
    else
      NULL
  } else {
    stop("object 'neg.ctrl' must be either empty or a 'character', 'logical' ",
      "or 'numeric' vector")
  }
}, sealed = SEALED)

setGeneric("main_title", function(object, ...) standardGeneric("main_title"))

setMethod("main_title", "OPMX", function(object, settings) {
  if (is.character(settings) || is.expression(settings))
    settings <- list(predef = settings)
  else if (is.logical(settings))
    settings <- list(use = settings)
  else if (is.numeric(settings))
    settings <- list(max = settings)
  else
    settings <- as.list(settings)
  if (!is.null(settings$predef) && nzchar(settings$predef))
    return(settings$predef) # nzchar() works for expressions, too
  settings <- insert(settings, use = TRUE, full = TRUE, .force = FALSE)
  if (settings$use) {
    settings$use <- NULL
    do.call(plate_type, c(list(object = object), settings))
  } else {
    NULL
  }
}, sealed = SEALED)

try_select_colors <- function(set) {
  tryCatch(expr = select_colors(set), error = function(e) set)
}

default_color_regions <- function(colors, space, bias, n) {
  colorRampPalette(colors = unique(colors), space = space, bias = bias)(n)
}

