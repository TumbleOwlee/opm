setGeneric("discrete", function(x, ...) standardGeneric("discrete"))

setMethod("discrete", "numeric", function(x, range, gap = FALSE,
    output = c("character", "integer", "logical", "factor", "numeric"),
    middle.na = TRUE, states = 32L, ...) {

  convert_states <- function(states) {
    if (!length(states))
      CHARACTER_STATES
    else if (is.numeric(states))
      if (length(states) > 1L)
        CHARACTER_STATES[states]
      else
        CHARACTER_STATES[seq_len(states)]
    else if (is.character(states))
      if (length(states) == 1L) {
        if (!nzchar(states))
          stop("'states' cannot be the empty string")
        unlist(strsplit(states, "", TRUE), FALSE, FALSE)
      } else if (any(nchar(states) != 1L)) {
        stop("'states' cannot contain strings of length other than one")
      } else {
        states
      }
    else
      stop("'states' must be empty or character or numeric vector")
  }

  output <- match.arg(output)

  LL(gap, middle.na)

  if (isTRUE(range)) {
    range <- if (gap) # middle cluster represents the ambiguous values
        borders(run_kmeans(object = x, k = 3L, ...))[[1L]]
      else
        range(x) # lazy mode, check or conversion to other mode only
  } else if (identical(range, FALSE)) {
    if (gap)
      range <- borders(run_kmeans(object = x, k = 2L, ...))[[1L]]
    else
      stop("if 'range' is FALSE, 'gap' cannot be FALSE")
  } else {
    stopifnot(length(range) %in% c(1L, 2L))
    range <- sort.int(range)
  }

  if (gap) { # binary states with a potential ambiguity gap given by 'range'

    x.range <- range(x)
    if (range[1L] < x.range[1L] || range[length(range)] > x.range[2L])
      stop("in 'gap' mode, 'range' must be within the range of 'x'")
    if (output == "numeric")
      return(x)
    tol <- .Machine$double.eps ^ 0.5
    breaks <- c(x.range[1L], c(range[1L] + tol, range[length(range)] - tol),
      x.range[2L] + tol)
    ints <- cut(x = x, breaks = breaks, labels = FALSE, right = FALSE)
    map <- if (middle.na)
        case(output,
          character = c("0", MISSING_CHAR, "1"),
          integer = c(0L, NA_integer_, 1L),
          logical = c(FALSE, NA, TRUE),
          factor = ordered(c(0L, NA_integer_, 1L))
        )
      else
        case(output,
          character = c("0", "1", "2"),
          integer = c(0L, 1L, 2L),
          logical = stop("one cannot combine 'logical' and 'middle.na'"),
          factor = ordered(c(0L, 1L, 2L))
        )
    structure(map[ints], cutoffs = range, names = names(x))

  } else { # binary- to multiple-state mode without a gap

    if (any(x > range[2L] | x < range[1L]))
      stop("if not in 'gap' mode, all values must be between ", range[1L],
        " and ", range[2L])
    if (output == "numeric")
      return(x)
    states <- convert_states(states)
    ints <- if ((nstates <- length(states)) > 1L)
        cut(x = c(range[1L:2L], x), breaks = nstates, right = FALSE,
          labels = FALSE)[-1L:-2L]
      else
        rep.int(1L, length(x))
    structure(case(output,
      character = states[ints],
      integer = ints,
      logical = as.logical(ints - 1L),
      factor = ordered(ints)
    ), names = names(x))

  }
}, sealed = SEALED)

setMethod("discrete", "array", function(x, ...) {
  map_values(object = x, mapping = discrete, ...)
}, sealed = SEALED)

setMethod("discrete", "data.frame", function(x, ..., as.labels = NULL,
    sep = " ") {
  discrete(extract_columns(object = x, what = "numeric", as.labels = as.labels,
    sep = sep, direct = FALSE), ...)
}, sealed = SEALED)

setGeneric("best_cutoff", function(x, y, ...) standardGeneric("best_cutoff"))

setMethod("best_cutoff", c("matrix", "character"), function(x, y, ...) {
  best_cutoff(x, as.factor(y), ...)
}, sealed = SEALED)

setMethod("best_cutoff", c("matrix", "factor"), function(x, y,
    combined = TRUE, lower = min(x, na.rm = TRUE),
    upper = max(x, na.rm = TRUE), all = FALSE) {

  indexes <- function(x) {
    y <- as.character(x)
    sapply(levels(x), function(level) which(y == level), simplify = FALSE)
  }

  all_cutoffs <- function(x) {
    x <- sort.int(unique.default(as.vector(x)))
    x[-1L] - diff(x) / 2
  }

  freq_score <- function(x) max(tabulate(x, 2L))
  freq_scores <- function(x) apply(x, 2L, freq_score)

  mat_freq_score <- function(x) {
    sum(unlist(lapply(y, function(i) freq_scores(x[i, , drop = FALSE])))) /
      freq_score(x)
  }
  mat_freq_score_2 <- function(x) sum(freq_scores(x)) / freq_score(x)

  opt_fun <- function(threshold) mat_freq_score((x > threshold) + 1L)
  opt_fun_2 <- function(threshold, x) mat_freq_score_2((x > threshold) + 1L)

  LL(all, upper, lower, combined)
  if (!any(duplicated(na.fail(L(y, nrow(x))))))
    stop("'y' contains only singletons")

  y <- indexes(y)

  if (combined) {
    if (all)
      cbind(cutoff = cutoffs <- all_cutoffs(x),
        score = vapply(cutoffs, opt_fun, 1))
    else
      unlist(optimize(f = opt_fun, maximum = TRUE, lower = lower,
        upper = upper))
  } else if (all) {
    lapply(y, function(i) {
      cutoffs <- all_cutoffs(m <- x[i, , drop = FALSE])
      cbind(cutoff = cutoffs,
        score = vapply(cutoffs, opt_fun_2, 1, x = m))
    })
  } else {
    do.call(rbind, lapply(y, function(i) {
      unlist(optimize(f = opt_fun_2, x = x[i, , drop = FALSE],
        maximum = TRUE, lower = lower, upper = upper))
    }))
  }

}, sealed = SEALED)

setGeneric("do_disc", function(object, ...) standardGeneric("do_disc"))

setMethod("do_disc", "OPMA", function(object, cutoff, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), unify = FALSE) {
  if (!length(cutoff))
    stop(sprintf("'cutoff' must be non-empty vector if applied to '%s' objects",
      class(object)))
  x <- aggregated(object = object, ci = FALSE,
    subset = unlist(map_param_names(subset, FALSE), FALSE, FALSE))[1L, ]
  x <- discrete(x = x, range = cutoff, gap = TRUE, output = "logical")
  settings <- disc_settings(object = cutoff, cutoffs = attr(x, "cutoffs"),
    datasets = 1L, parameter = subset, unified = -1)
  if (L(plain))
    return(structure(.Data = c(x), settings = settings))
  new("OPMD", measurements = measurements(object),
    metadata = metadata(object), csv_data = csv_data(object),
    aggregated = aggregated(object), aggr_settings = aggr_settings(object),
    discretized = c(x), disc_settings = settings)
}, sealed = SEALED)

setMethod("do_disc", "OPMS", function(object, cutoff = TRUE, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), unify = !length(cutoff),
    ...) {

  run_best_cutoff <- function(x, grp, combined) {
    bc <- best_cutoff(x, grp <- as.factor(grp), combined)
    if (combined) # convert returned vector into appropriate matrix
      matrix(bc, length(levels(grp)), length(bc), TRUE,
        list(levels(grp), names(bc)))
    else
      bc
  }

  create_opmd <- function(x, discretized, settings) {
    new("OPMD", measurements = measurements(x),
      metadata = metadata(x), csv_data = csv_data(x),
      aggregated = aggregated(x), aggr_settings = aggr_settings(x),
      discretized = discretized, disc_settings = settings)
  }

  prepare_unify <- function(unify) {
    if (is.logical(L(unify))) {
      if (is.na(unify))
        1L
      else if (unify)
        get("min.mode", OPM_OPTIONS)
      else
        -1L
    } else if (is.numeric(unify)) {
      if (is.na(unify))
        stop("a numeric NA 'unify' value makes no sense")
      if (unify > 0) {
        if (unify > 1)
          stop("a numeric 'unify' value > 1 makes no sense")
        unify
      } else {
        -1L
      }
    } else {
      stop("'unify' must be a logical or numeric scalar")
    }
  }

  if (!all(has_aggr(object)))
    stop("all plates must contain aggregated data to run this function")

  unify <- prepare_unify(unify)

  if (is.logical(groups)) {
    combined <- !L(groups)
    groups <- NULL
  } else {
    combined <- !length(groups)
  }

  # extra step necessary here because extract() allows 'disc'
  subset <- unname(match.arg(subset, unlist(map_param_names(plain = TRUE))))

  x <- extract(object = object, as.labels = groups, subset = subset,
    ci = FALSE, full = FALSE, dataframe = FALSE, dups = "ignore", ...)

  if (use.best <- is.null(cutoff)) {
    if (combined) {
      grp <- rep.int(".", nrow(x))
    } else {
      if (!length(groups))
        stop("if 'cutoff' is NULL, 'groups' must not be empty")
      grp <- rownames(x)
    }
  } else if (combined) {
    grp <- NULL
  } else if (length(groups)) {
    grp <- rownames(x)
  } else {
    grp <- seq_len(nrow(x))
  }

  settings <- disc_settings(cutoff)

  if (length(grp)) { # unless empty, 'grp' now holds the group names

    settings <- rep.int(list(settings), length(object))

    if (use.best) { # using best_cutoff() instead of discrete()

      bc <- run_best_cutoff(x, grp, combined)
      for (idx in split(seq_along(grp), grp)) {
        group <- grp[idx[1L]]
        options <- list(cutoffs = bc[group, "maximum"], datasets = length(idx),
          score = bc[group, "objective"], group = group, parameter = subset,
          unified = unify)
        if (combined)
          options$group <- NULL
        x[idx, ] <- x[idx, , drop = FALSE] > options$cutoffs
        for (i in idx)
          settings[[i]][[OPTIONS]] <- options
      }

    } else { # discrete() partitioning separately per group

      for (idx in split(seq_along(grp), grp)) {
        x[idx, ] <- y <- discrete(x[idx, , drop = FALSE], range = cutoff,
          gap = TRUE, output = "integer")
        options <- list(cutoffs = attr(y, "cutoffs"), datasets = length(idx),
          group = as.character(grp[idx[1L]]), parameter = subset,
          unified = unify)
        for (i in idx)
          settings[[i]][[OPTIONS]] <- options
      }

    }

    mode(x) <- "logical"

    if (unify > 0)
      for (idx in split(seq_along(grp), grp)) {
        y <- reduce_to_mode.matrix(x[idx, , drop = FALSE], unify, TRUE)
        for (i in idx)
          x[i, ] <- y
      }

  } else if (combined) { # discrete() partitioning with entire dataset at once

    x <- discrete(x = x, range = cutoff, gap = TRUE, output = "logical")

    settings[[OPTIONS]] <- list(cutoffs = attr(x, "cutoffs"),
      datasets = length(object), parameter = subset, unified = unify)
    settings <- rep.int(list(settings), length(object))

    if (unify > 0) {
      y <- reduce_to_mode.matrix(x, unify, TRUE)
      for (i in seq_len(nrow(x)))
        x[i, ] <- y
    }

  } else { # this should be an impossible combination of settings

    stop(BUG_MSG)

  }

  if (L(plain))
    return(structure(.Data = c(x), settings = settings))
  for (i in seq_along(object@plates))
    object@plates[[i]] <- create_opmd(object@plates[[i]], x[i, ], settings[[i]])
  object
}, sealed = SEALED)

setMethod("do_disc", "MOPMX", function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = do_disc, ...)
  object
}, sealed = SEALED)

