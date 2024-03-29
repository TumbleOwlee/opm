setGeneric("summary")

setMethod("summary", "OPM", function(object, ...) {
  result <- list(
    Class = class(object),
    `From file` = csv_data(object, what = "filename"),
    `Hours measured` = hours(object),
    `Number of wells` = length(wells(object)),
    `Plate type` = plate_type(object),
    Position = csv_data(object, what = "position"),
    `Setup time` = csv_data(object, what = "setup_time"),
    Metadata = sum(rapply(object@metadata, function(item) 1L)),
    Aggregated = has_aggr(object),
    Discretized = has_disc(object),
    ...
  )
  class(result) <- "OPM_Summary"
  result
}, sealed = SEALED)

setMethod("summary", "OPMS", function(object, ...) {
  result <- lapply(X = object@plates, FUN = summary, ...)
  attr(result, "overall") <- list(Dimensions = dim(object),
    Aggregated = sum(has_aggr(object)), Discretized = sum(has_disc(object)),
    Plate.type = plate_type(object))
  class(result) <- "OPMS_Summary"
  result
}, sealed = SEALED)

setMethod("summary", "MOPMX", function(object, ...) {
  select_parts <- function(x) if (is.null(y <- attr(x, "overall")))
      c(list(Length = 1L, Plate.type = x[["Plate type"]]),
        x[c("Aggregated", "Discretized")])
    else
      c(list(Length = y[["Dimensions"]][[1L]]),
        y[c("Plate.type", "Aggregated", "Discretized")])
  if (length(object)) {
    result <- lapply(lapply(X = object, FUN = summary, ...), select_parts)
    result <- do.call(rbind, lapply(result, as.data.frame))
    if (!is.null(n <- names(object)))
      rownames(result) <- make.names(n, TRUE)
  } else {
    result <- as.data.frame(matrix(NA, 0L, 4L, FALSE))
    colnames(result) <- c("Length", "Plate.type", "Aggregated", "Discretized")
  }
  class(result) <- c("MOPMX_Summary", oldClass(result))
  result
}, sealed = SEALED)

setMethod("show", "OPMX", function(object) {
  print(summary(object))
  invisible(NULL)
}, sealed = SEALED)

setMethod("show", "MOPMX", function(object) {
  print(summary(object))
  invisible(NULL)
}, sealed = SEALED)

setMethod("show", "CMAT", function(object) {
  if (typeof(object) == "list") {
    object[] <- lapply(X = object, FUN = paste0, collapse = "/")
    storage.mode(object) <- "character"
  }
  callNextMethod()
}, sealed = SEALED)

setGeneric("str")

setMethod("str", "WMDX", function(object, ...) {
  callNextMethod(object, ...)
  cat(STR_NOTE)
  invisible(NULL)
}, sealed = SEALED)

setMethod("str", "MOPMX", function(object, ...) {
  callNextMethod(object, ...)
  cat(STR_NOTE)
  invisible(NULL)
}, sealed = SEALED)

setMethod("str", "CMAT", function(object, ...) {
  callNextMethod(object, ...)
  cat(STR_NOTE)
  invisible(NULL)
}, sealed = SEALED)

setGeneric("xy_plot", function(x, ...) standardGeneric("xy_plot"))

setMethod("xy_plot", "MOPMX", function(x, ...) {
  sapply(X = x, FUN = xy_plot, ..., simplify = FALSE, USE.NAMES = TRUE)
}, sealed = SEALED)

setMethod("xy_plot", "OPM", function(x, col = "midnightblue", lwd = 1,
    neg.ctrl = "A01", base.col = "grey10", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE,
    strip.fmt = list(), striptext.fmt = list(), rcr = 0.75,
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPMS

  # Setup
  layout <- best_layout(dim(x)[2L], rcr)
  y.max <- improved_max(x, theor.max)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  ## END must be synchronized with xy_plot,OPMS

  # Plot
  xyplot(
    # Principally unchangeable arguments
    create_formula("`%s` ~ `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = flatten(x, ...), type = "l", layout = layout,
    as.table = TRUE,
    # Curve colour and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Main annotation
    main = main, xlab = xlab, ylab = ylab,
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # The panels
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    })

}, sealed = SEALED)

setMethod("xy_plot", "OPMS", function(x, col = opm_opt("colors"), lwd = 1,
    neg.ctrl = "A01", base.col = "black", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE, space = "top",
    strip.fmt = list(), striptext.fmt = list(), legend.labels = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE, rcr = 0.75,
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPM

  # Setup
  layout <- best_layout(dim(x)[3L], rcr)
  y.max <- improved_max(x, theor.max)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  ## END must be synchronized with xy_plot,OPM

  # OPMS-specific addition of defaults
  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  # Conversion
  data <- flatten(x, labels = legend.labels, ...)

  # Assignment of colours to plates
  param <- flattened_to_factor(object = data, sep = legend.sep)
  key.text <- levels(param)

  if (length(col) == 1) {
    if (!nzchar(col)) { # selection of a colour set, "" is special
      col <- length(key.text)
    } else if (!is_colorscheme_supported(col)) {
      col <- rep(col, length(key.text))
    }
  }
  col <- try_select_colors(col)

  if (length(col) < length(key.text))
    stop("colour should be by plate or metadata (", length(key.text),
      " variants), but there are too few colours (", length(col), ")")
  key.col <- col[seq_along(key.text)]
  col <- col[param]

  names(data)[match(RESERVED_NAMES[["plate"]], names(data))] <- "_GROUPING"

  # Plot
  xyplot(
    # Principally unchangeable arguments
    create_formula("`%s` ~ `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = data, type = "l", layout = layout,
    as.table = TRUE, groups = `_GROUPING`,
    # Curve colours and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    }
  )

}, sealed = SEALED)

setMethod("xy_plot", "data.frame", function(x, f, groups,
    col = opm_opt("colors"), lwd = 1, neg.ctrl = NULL, base.col = "black",
    base.lwd = lwd, main = groups, xlab = elem(f, 3L:2L), ylab = elem(f, 2L),
    draw.grid = TRUE, space = "top", strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE, rcr = 0.75,
    ...) {

  elem <- function(x, i) {
    pos <- 1L
    while (length(x) > 1L) {
      x <- x[[i[[pos]]]]
      if (pos < length(i))
        pos <- pos + 1L
    }
    as.character(x)
  }

  f <- as.formula(f)
  groups <- as.character(groups)

  # Layout
  xvar <- as.factor(x[, elem(f, 3L:3L)])
  layout <- best_layout(length(levels(xvar)), rcr)

  # Put grouping variable together
  pos <- match(groups, names(x))
  if (any(isna <- is.na(pos)))
    stop(sprintf("could not find '%s' in 'x'", groups[isna][1L]))
  x$`_GROUPING` <- do.call(paste, c(x[, pos, drop = FALSE], sep = legend.sep))
  x$`_GROUPING` <- as.factor(x$`_GROUPING`)

  # Assignment of colours
  key.text <- levels(x$`_GROUPING`)
  
  if (length(col) == 1) {
    if (!nzchar(col)) { # selection of a colour set, "" is special
      col <- length(key.text)
    } else if (!is_colorscheme_supported(col)) {
      col <- rep(col, length(key.text))
    }
  }
  col <- try_select_colors(col)
  
  if (length(key.text) > length(col))
    stop("number of colours (", length(col),
      ") must be at least as large as number of groups (",
      length(key.text), ")")
  key.col <- col[seq_along(key.text)]
  col <- key.col

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  xyplot(
    # Principally unchangeable arguments
    x = f, data = x, type = "l", layout = layout,
    as.table = TRUE, groups = `_GROUPING`,
    # Curve colours (panel height is omitted)
    col = col,
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    },
    ...
  )
}, sealed = SEALED)

setGeneric("level_plot", function(x, ...) standardGeneric("level_plot"))

setMethod("level_plot", "OPM", function(x, main = list(),
    colors = opm_opt("color.borders"), panel.headers = FALSE, cex = NULL,
    strip.fmt = list(), striptext.fmt = list(), legend.sep = " ",
    space = "Lab", bias = 0.5, num.colors = 200L, ...) {
  if (is.null(cex))
    cex <- guess_cex(dim(x)[2L])
  main <- main_title(x, main)
  levelplot(create_formula("`%s` ~ `%s` * `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = flatten(x, ...), main = main,
    col.regions = default_color_regions(colors, space, bias, num.colors),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)

setMethod("level_plot", "OPMS", function(x, main = list(),
    colors = opm_opt("color.borders"), panel.headers = TRUE, cex = NULL,
    strip.fmt = list(), striptext.fmt = list(), legend.sep = " ",
    space = "Lab", bias = 0.5, num.colors = 200L, ...) {
  dims <- dim(x)
  if (is.null(cex))
    cex <- guess_cex(dims[3L])
  data <- flatten(x, ...)
  if (is.null(panel.headers) || (is.logical(panel.headers) && !panel.headers)) {
    strip.fmt <- FALSE
  } else {
    if (is.logical(panel.headers))
      panel.headers <- flattened_to_factor(object = data, sep = legend.sep)
    if (!is.expression(panel.headers))
      panel.headers <- as.character(panel.headers)
    strip.fmt <- insert(as.list(strip.fmt), bg = "grey90",
      factor.levels = panel.headers)
    strip.fmt <- do.call(strip.custom, strip.fmt)
  }
  levelplot(create_formula("`%s` ~ `%s` * `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well", "plate")]),
    data = data, main = main_title(x, main),
    col.regions = default_color_regions(colors, space, bias, num.colors),
    strip = strip.fmt, as.table = TRUE, layout = c(dims[1L], 1L),
    par.strip.text = as.list(striptext.fmt),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)

setMethod("level_plot", "MOPMX", function(x, ...) {
  sapply(X = x, FUN = level_plot, ..., simplify = FALSE, USE.NAMES = TRUE)
}, sealed = SEALED)

setGeneric("ci_plot", function(object, ...) standardGeneric("ci_plot"))

setMethod("ci_plot", "data.frame", function(object, rowname.sep = " ",
    prop.offset = 0.04, align = "center", col = "blue", na.action = "warn",
    draw.legend = TRUE, legend.field = c(1, 1), x = "topleft", xpd = TRUE,
    vline = 0, split.at = param_names("split.at"), crr = 0.75, ...) {

  single_plot <- function(col.pos) {
    plot(x = NULL, y = NULL, xlim = ranges[, col.pos], ylim = ylim,
      main = colnames(object)[col.pos], yaxt = "n", xlab = "", ylab = "")
    if (length(vline))
      abline(v = vline, lty = 2L, col = "gray60")
    axis(2L, at = chunk.pos, labels = row.names)
    vapply(chunk.pos, function(pos) {
      pe <- object[pos, col.pos]
      left <- object[pos + 1L, col.pos]
      right <- object[pos + 2L, col.pos]
      draw_ci(c(left, pe, right, pos), col = col, na.action = na.action)
    }, numeric(4L))
  }

  # Determine the position used for splitting the data frame
  param.pos <- assert_splittable_matrix(object, split.at)

  # Check the triplet structure and determine all triplet start positions
  if (nrow(object) %% 3L != 0L)
    stop("need data frame with 3 * n rows")
  chunk.pos <- seq_len(nrow(object))
  chunk.pos <- chunk.pos[chunk.pos %% 3L == 1L]
  row.names <- as.character(seq_along(chunk.pos))

  # Reorder the matrix and construct the legend if necessary
  if (param.pos > 1L) {
    factor.pos <- seq.int(1L, param.pos - 1L)
    ordering <- do.call(order, as.list(object[, factor.pos, drop = FALSE]))
    object <- object[ordering, , drop = FALSE]
    legend <- as.matrix(object[chunk.pos, factor.pos, drop = FALSE])
    legend <- apply(X = legend, MARGIN = 1L, FUN = paste,
      collapse = rowname.sep)
    legend <- paste(row.names, legend, sep = ": ")
  } else {
    legend <- NULL
  }

  # Reduce to the numeric part of matrix
  object <- as.matrix(object[, seq.int(param.pos + 1L, ncol(object)),
    drop = FALSE])

  # Determine field range (which is set to be uniform)
  ranges <- apply(X = object, MARGIN = 2L, FUN = range, na.rm = TRUE)
  max.range <- max(apply(ranges, 2L, function(x) x[2L] - x[1L]))
  ranges <- apply(X = ranges, MARGIN = 2L, FUN = best_range,
    target = max.range, align = align, prop.offset = prop.offset)
  ylim <- best_range(object = chunk.pos, target = NULL,
    prop.offset = prop.offset)

  # Panel layout and plotting of individual panels
  old.par <- par(mfcol = best_layout(ncol(object), crr))
  on.exit(par(old.par))
  lapply(seq_len(ncol(object)), single_plot)

  # Legend
  if (draw.legend && !is.null(legend)) {
    if (length(legend.field) > 1L)
      old.par <- c(par(mfg = legend.field[1L:2L]), old.par)
    else
      old.par <- c(par(mfcol = c(1L, 1L)), old.par)
    legend(x = x, legend = legend, xpd = xpd, ...)
  }
  invisible(legend)

}, sealed = SEALED)

setMethod("ci_plot", "OPMS", function(object, as.labels,
    subset = opm_opt("curve.param"), ...) {
  ci_plot(extract(object, as.labels = as.labels, subset = subset,
    dataframe = TRUE, ci = TRUE), split.at = param_names("split.at"), ...)
}, sealed = SEALED)

setMethod("ci_plot", "MOPMX", function(object, ...) {
  sapply(X = object, FUN = ci_plot, ..., simplify = FALSE, USE.NAMES = TRUE)
}, sealed = SEALED)

setGeneric("heat_map", function(object, ...) standardGeneric("heat_map"))

setMethod("heat_map", "matrix", function(object,
    hclustfun = "ward.D2", distfun = "euclidean", scale = "none",
    r.groups = "row.groups", r.col = opm_opt("colors"),
    c.groups = "col.groups", c.col = opm_opt("colors"),
    magnif = 4, cexRow = magnif[1L] / sqrt(nrow(object)),
    cexCol = magnif[length(magnif)] / sqrt(ncol(object)),
    borders = c(0.55, 0.75),
    margins = if (use.fun[1L] == "gplots")
      c(borders[1L] * cexCol * max(nchar(colnames(object))),
      borders[length(borders)] * cexRow * max(nchar(rownames(object))))
    else
      c(5, 5),
    col = opm_opt("heatmap.colors"), asqr = FALSE, log1 = FALSE, lmap = 1L:3L,
    abbrev = c("none", "row", "column", "both"), plot.na = "plot.NA",
    reorderfun = function(d, w) reorder(d, w, mean), ...,
    use.fun = c("gplots", "stats")) {

  shorten <- function(x, n1 = 0L, n2 = 3L) {
    x <- gsub(sprintf("^\\b([A-Z][a-z]{%i})[a-z]{2,}\\b(?!\\.)", n1),
      "\\1.", x, FALSE, TRUE)
    gsub(sprintf("\\b([a-z]{%i})[a-z]{2,}\\b(?!\\.)", n2),
      "\\1.", x, FALSE, TRUE)
  }

  get_fun <- function(infun, usefun) {
    usefun <- match.fun(usefun)
    if (is.character(infun))
      function(x) usefun(x, method = infun)
    else if (is.list(infun))
      function(x) do.call(usefun, c(list(x), infun))
    else if (is.function(infun))
      infun
    else
      stop("expected character vector, list or function, got '", class(infun),
        "'")
  }

  get_side_colors <- function(groups, colors, for.rows) {
    if (is.null(groups))
      return(NULL)
    if (is.function(groups)) {
      groups <- if (for.rows)
        groups(rownames(object))
      else
        groups(colnames(object))
    } else if (is.character(groups)) {
      if (length(groups) == 1L) {
        groups <- attr(object, groups)
        if (is.null(groups))
          return(NULL)
      }
    } else {
      groups <- as.character(groups)
    }
    groups <- as.factor(groups)
  
    if (length(colors) == 1) {
      if (!nzchar(colors)) { # selection of a colour set, "" is special  
        colors <- length(levels(groups))
      } else if (!is_colorscheme_supported(colors)) {
        colors <- rep(colors, length(levels(groups)))
      }
    }
    colors <- try_select_colors(colors)
    
    if (length(colors) < length(levels(groups)))
      stop("more groups (", length(levels(groups)),
        ") than colours (", length(colors), ") given")
    structure(.Data = colors[groups], names = as.character(groups))
  }

  do_asqr <- function(x, percent) {
    if (percent) {
      if (any(x < 0, na.rm = TRUE) || any(x > 100, na.rm = TRUE))
        warning("in 'percent' mode, 'x' should be between 0 and 100")
      else if (all(x <= 1, na.rm = TRUE))
        warning("percentages expected, but everything < 1")
      return(100 * asin(sqrt(x / 100)))
    }
    asin(sqrt(x))
  }

  plot.na <- attr(object, plot.na)

  case(match.arg(abbrev),
    none = NULL,
    row = rownames(object) <- shorten(rownames(object)),
    column = colnames(object) <- shorten(colnames(object)),
    both = {
      rownames(object) <- shorten(rownames(object))
      colnames(object) <- shorten(colnames(object))
    }
  )

  clustfun <- get_fun(hclustfun, hclust)
  dfun <- get_fun(distfun, dist)
  arg.list <- list(scale = scale, cexRow = cexRow, cexCol = cexCol,
    hclustfun = clustfun, distfun = dfun, margins = margins, col = col,
    reorderfun = reorderfun, ...)

  row.side.colors <- get_side_colors(r.groups, r.col, for.rows = TRUE)
  if (!is.null(row.side.colors))
    arg.list$RowSideColors <- row.side.colors
  col.side.colors <- get_side_colors(c.groups, c.col, for.rows = FALSE)
  if (!is.null(col.side.colors))
    arg.list$ColSideColors <- col.side.colors

  case(match.arg(use.fun),
    gplots = {
      if (suppressMessages(suppressWarnings(require(package = gplots,
          quietly = TRUE, warn.conflicts = FALSE)))) {
        arg.list <- insert(arg.list, trace = "none", .force = FALSE)
        heatmap_fun <- gplots::heatmap.2
      } else {
        warning("package 'gplots' requested, but not available")
        heatmap_fun <- heatmap
      }
    },
    stats = heatmap_fun <- heatmap
  )

  if (typeof(object) == "logical")
    if (length(lmap))
      object[] <- map_values(c(object), lmap)
    else
      storage.mode(object) <- "integer"

  LL(asqr, log1)
  if (is.na(asqr) || asqr) {
    if (log1)
      stop("log and asrq transformation cannot both be chosen")
    object[] <- do_asqr(object, is.na(asqr))
  } else if (log1) {
    object[] <- log1p(object)
  }

  if (length(plot.na))
    object[is.na(object)] <- plot.na

  result <- do.call(heatmap_fun, c(list(x = object), arg.list))
  result$colColMap <- col.side.colors
  result$rowColMap <- row.side.colors
  invisible(result)

}, sealed = SEALED)

setMethod("heat_map", "data.frame", function(object, as.labels,
    as.groups = NULL, sep = " ", ...) {
  invisible(heat_map(extract_columns(object, what = "numeric", direct = FALSE,
    as.labels = as.labels, as.groups = as.groups, sep = sep), ...))
}, sealed = SEALED)

setMethod("heat_map", "OPMS", function(object, as.labels,
    subset = opm_opt("curve.param"), as.groups = NULL, sep = " ",
    extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = as.groups, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(heat_map(do.call(extract, extract.args), ...))
}, sealed = SEALED)

setMethod("heat_map", "MOPMX", function(object, as.labels,
    subset = opm_opt("curve.param"), as.groups = NULL, sep = " ",
    extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = as.groups, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(heat_map(do.call(extract, extract.args), ...))
}, sealed = SEALED)

setGeneric("radial_plot", function(object, ...) standardGeneric("radial_plot"))

setMethod("radial_plot", "matrix", function(object, as.labels = NULL,
    subset = TRUE, sep = " ", extract.args = list(), rp.type = "p",
    radlab = FALSE, show.centroid = TRUE, show.grid.labels = 1, lwd = 3,
    mar = c(2, 2, 2, 2), line.col = opm_opt("colors"), draw.legend = TRUE,
    x = "bottom", y = NULL, xpd = TRUE, pch = 15, legend.args = list(),
    group.col = FALSE, point.symbols = 15, point.col = opm_opt("colors"),
    poly.col = NA, main = paste0(as.labels, sep = sep), ...) {

  # insert a ready-made colour vector for line.col
  adapt_colors <- function(x, colors) {
    if (length(colors) < length(levels(f <- as.factor(x))))
      stop("not enough colours provided")
    structure(.Data = colors[f], names = x)
  }

  LL(radlab, show.centroid, show.grid.labels, draw.legend, xpd, pch, group.col)
  if (!nzchar(line.col))
    line.col <- "w3c"
  line.col <- try_select_colors(line.col)
  if (!nzchar(point.col))
    point.col <- "w3c"
  point.col <- try_select_colors(point.col)
  changed.par <- NULL
  on.exit(if (!is.null(changed.par))
    par(changed.par))

  if (group.col && !is.null(rn <- rownames(object))) {
    line.col <- adapt_colors(rn, line.col)
    point.col <- adapt_colors(rn, point.col)
  } else {
    line.col <- adapt_colors(seq_len(nrow(object)), line.col)
    point.col <- adapt_colors(seq_len(nrow(object)), point.col)
  }

  changed.par <- radial.plot(lengths = object[, subset, drop = FALSE],
    labels = colnames(object), rp.type = rp.type, radlab = radlab,
    show.centroid = show.centroid, lwd = lwd, mar = mar,
    show.grid.labels = show.grid.labels, line.col = line.col,
    point.symbols = point.symbols, point.col = point.col, poly.col = poly.col,
    main = main, ...)
  if (is.null(rn <- rownames(object))) {
    line.col <- NULL
  } else {
    if (group.col) {
      line.col <- line.col[!duplicated.default(line.col)]
      rn <- names(line.col)
    } else {
      names(line.col) <- rn
    }
    if (draw.legend) {
      legend.args <- insert(as.list(legend.args), x = x, y = y, col = line.col,
        legend = rn, pch = pch, .force = TRUE)
      do.call(legend, legend.args)
    }
  }
  invisible(line.col)
}, sealed = SEALED)

setMethod("radial_plot", "data.frame", function(object, as.labels,
    subset = "numeric", sep = " ", extract.args = list(), ...) {
  invisible(radial_plot(extract_columns(object, what = subset,
    direct = FALSE, as.labels = as.labels, sep = sep), ...))
}, sealed = SEALED)

setMethod("radial_plot", "OPMS", function(object, as.labels,
    subset = opm_opt("curve.param"), sep = " ", extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = NULL, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(radial_plot(do.call(extract, extract.args), ...))
}, sealed = SEALED)

setMethod("radial_plot", "MOPMX", function(object, ...) {
  sapply(X = object, FUN = radial_plot, ..., simplify = FALSE, USE.NAMES = TRUE)
}, sealed = SEALED)

setGeneric("parallel_plot",
  function(x, data, ...) standardGeneric("parallel_plot"))

setGeneric("parallelplot")

setMethod("parallelplot", c("missing", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, NULL, ...)
}, sealed = SEALED)

setMethod("parallel_plot", c("missing", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, NULL, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("NULL", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, x, ...)
}, sealed = SEALED)

setMethod("parallel_plot", c("NULL", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, x, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("vector", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, x, ...)
}, sealed = SEALED)

setMethod("parallel_plot", c("vector", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, x, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("MOPMX", "XOPMX"), function(x, data, ...) {
  stop("cannot use 'XOPMX' object as both 'x' and 'data' argument")
}, sealed = SEALED)

setMethod("parallel_plot", c("MOPMX", "XOPMX"), function(x, data, ...) {
  stop("cannot use 'XOPMX' object as both 'x' and 'data' argument")
}, sealed = SEALED)

setMethod("parallelplot", c("OPMX", "XOPMX"), function(x, data, ...) {
  stop("cannot use 'XOPMX' object as both 'x' and 'data' argument")
}, sealed = SEALED)

setMethod("parallel_plot", c("OPMX", "XOPMX"), function(x, data, ...) {
  stop("cannot use 'XOPMX' object as both 'x' and 'data' argument")
}, sealed = SEALED)

setMethod("parallelplot", c("formula", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, x, ...)
}, sealed = SEALED)

setMethod("parallel_plot", c("formula", "XOPMX"), function(x, data, ...) {
  parallel_plot(data, x, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("OPMX", "missing"), function(x, data, ...) {
  parallel_plot(x, NULL, ...)
}, sealed = SEALED)

setMethod("parallel_plot", c("OPMX", "missing"), function(x, data, ...) {
  parallel_plot(x, NULL, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("MOPMX", "missing"), function(x, data, ...) {
  parallel_plot(x, NULL, ...)
}, sealed = SEALED)

setMethod("parallel_plot", c("MOPMX", "missing"), function(x, data, ...) {
  parallel_plot(x, NULL, ...)
}, sealed = SEALED)

setMethod("parallelplot", c("MOPMX", "ANY"), function(x, data, ...) {
  sapply(X = x, FUN = parallel_plot, data = data, ..., simplify = FALSE,
    USE.NAMES = TRUE)
}, sealed = SEALED)

setMethod("parallel_plot", c("MOPMX", "ANY"), function(x, data, ...) {
  sapply(X = x, FUN = parallel_plot, data = data, ..., simplify = FALSE,
    USE.NAMES = TRUE)
}, sealed = SEALED)

setMethod("parallelplot", c("OPMX", "ANY"), function(x, data, ...) {
  parallel_plot(x = x, data = data, ...)
}, sealed = SEALED)

setMethod("parallel_plot", c("OPMX", "ANY"), function(x, data, groups = 1L,
  panel.var = NULL, pnames = param_names(), col = opm_opt("colors"),
  strip.fmt = list(), striptext.fmt = list(), legend.fmt = list(),
  legend.sep = " ", draw.legend = TRUE, space = "top", ...) {

  # Get the used column names from the 'data' argument
  final_dataframe_names <- function(x) {
    if (is.null(x))
      return(x)
    x <- metadata_key(x)
    combined <- attr(x, "combine") # a list mapping new name to old names
    x <- names(x)
    for (i in seq_along(combined)) { # does nothing if combined has zero length!
      j <- match(combined[[i]], x) # find position of old name and replace it
      x[j] <- c(names(combined)[i], rep.int(NA_character_, length(j) - 1L))
    }
    x[!is.na(x)]
  }
  # Convert left side of formula to character vector
  extract_left_side <- function(x) {
    if (!inherits(x, "formula") || length(x) < 3L)
      character()
    else
      all.vars(x[[2L]])
  }
  # Assign first element of 'n' to 'x' ('groups'/'panel.var'), if that
  # is numeric and 'n' is non-empty.
  fetch_from_md_names <- function(x, n) {
    if (!length(x))
      return(x)
    if (is.numeric(x) || is.logical(x))
      if (length(n))
        n[x]
      else
        NULL
    else
      x
  }

  x <- as.data.frame(x = x, include = data, sep = NULL, settings = FALSE)

  # Process the 'param' argument
  if (missing(pnames)) {
    if (length(tmp <- extract_left_side(data)))
      pnames <- tmp
    else
      pnames <- match.arg(pnames, several.ok = TRUE)
  } else {
    if (is.language(pnames))
      pnames <- all.vars(pnames)
    pnames <- match.arg(pnames, param_names(), TRUE)
  }
  if (length(pnames) < 2L)
    stop("'pnames' has to be at least of length 2")

  md.names <- final_dataframe_names(data)

  # Process 'groups'
  groups <- fetch_from_md_names(groups, md.names)
  if (!length(groups))
    groups <- make.names(CSV_NAMES[["PLATE_TYPE"]])
  else if (length(groups) > 1L)
    stop("'groups' argument must be of length 1")
  pos <- match(groups, names(x), 0L)
  if (!pos)
    stop("value of 'groups' not found in the column names of the data")
  # Renaming for lattice. Must be in sync with the processing of 'panel.var'.
  names(x)[pos] <- "_GROUPING"

  # Legend format
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt),
    cex = 1.5 / sqrt(9), lines = 1.25)
  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  # Legend text and colours
  key.text <- levels(x$`_GROUPING`)

  if (length(col) == 1) {
    if (!nzchar(col)) { # selection of a colour set, "" is special  
      col <- length(key.text)
    } else if (!is_colorscheme_supported(col)) {
      col <- rep(col, length(key.text))
    }
  }
  col <- try_select_colors(col)
  
  if (length(col) < length(key.text))
    stop("colour should be by plate or metadata (", length(key.text),
      " variants), but there are too few colours (", length(col), ")")
  col <- col[seq_along(key.text)]

  # Build basic formula and process 'panel.var'
  f <- paste0("~ x[", deparse(pnames), "]")
  panel.var <- fetch_from_md_names(panel.var, md.names)

  # Add content of 'panel.var' to formula if it is provided
  if (length(panel.var)) {
    panel.var[match(groups, panel.var)] <- "_GROUPING"
    f <- paste(f, "|", paste0(sprintf("`%s`", panel.var), collapse = " + "))
  }
  f <- formula(f)

  parallelplot(
    x = f, data = x, as.table = TRUE, groups = `_GROUPING`, col = col,
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    key = if (draw.legend)
        c(list(col = col, text = list(key.text)), legend.fmt)
      else
        NULL, ...
  )
}, sealed = SEALED)
