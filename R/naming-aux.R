custom_plate_is <- function(x) grepl("^Custom:", x, TRUE, TRUE)

custom_plate_proper <- function(x) substring(x, 8L, nchar(x))

custom_plate_prepend <- function(x) sprintf("CUSTOM:%s", x)

custom_plate_prepend_full <- function(x) sprintf("CUSTOM_FULL_NAME:%s", x)

custom_plate_normalize_proper <- function(x) {
  x <- sub("\\W+$", "", sub("^\\W+", "", x, FALSE, TRUE), FALSE, TRUE)
  toupper(gsub("\\W+", "-", x, FALSE, TRUE))
}

custom_plate_normalize <- function(x) {
  custom_plate_prepend(custom_plate_normalize_proper(custom_plate_proper(x)))
}

custom_plate_normalize_all <- function(x) {
  x <- ifelse(custom_plate_is(x), custom_plate_proper(x), x)
  custom_plate_prepend(custom_plate_normalize_proper(x))
}

custom_plate_exists <- function(x) {
  exists(x, MEMOIZED)
}

custom_plate_get <- function(x) {
  get(x, MEMOIZED)
}

custom_plate_assert <- function(x, coords) {
  if (custom_plate_exists(x)) {
    if (any(bad <- !coords %in% names(custom_plate_get(x))))
      stop("well coordinate missing from plate type '", x, "': ",
        coords[bad][1L])
  } else
    stop("unknown user-defined plate type: ", x)
  TRUE
}

custom_plate_set <- function(x, value) {
  if (exists(x, MEMOIZED))
    warning("overwriting well map for plate type ", x)
  MEMOIZED[[x]] <- value
  value
}

custom_plate_set_full <- function(x, value) {
  key <- custom_plate_prepend_full(custom_plate_proper(x))
  names(value) <- NULL
  if (exists(key, MEMOIZED) && !identical(value, get(key, MEMOIZED)))
    warning("overwriting full name for plate type ", x)
  MEMOIZED[[key]] <- value
  value
}

normalize_predefined_plate <- function(object, subtype = FALSE) {
  normalize_pm <- function(x, subtype) {
    x <- sub("^PMM", "PM-M", x, FALSE, TRUE)
    x <- sub("^PM-MTOX", "PM-M TOX", x, FALSE, TRUE)
    x <- sub("([A-Z]+)$", if (subtype)
      "-\\1"
    else
      "", x, FALSE, TRUE)
    sub("([^\\d])(\\d)([^\\d]|$)", "\\10\\2\\3", x, FALSE, TRUE)
  }
  normalize_sf <- function(x, subtype) {
    x <- if (subtype)
      sub("-$", "", sub(SP_PATTERN, "\\1-\\2", x, FALSE, TRUE), FALSE, TRUE)
    else
      sub(SP_PATTERN, "\\1", x, FALSE, TRUE)
    x <- sub("^(G|SF)([NP])", "SF-\\2", x, FALSE, TRUE)
    sub("^GENIII", "Gen III", x, FALSE, TRUE)
  }
  result <- toupper(gsub("\\W", "", object, FALSE, TRUE))
  pm <- grepl("^PM(M(TOX)?)?\\d+[A-Z]*$", result, FALSE, TRUE)
  result[pm] <- normalize_pm(result[pm], subtype)
  sf[sf] <- grepl(SP_PATTERN, result[sf <- !pm], FALSE, TRUE)
  result[sf] <- normalize_sf(result[sf], subtype)
  result[bad] <- object[bad <- !(pm | sf)]
  result
}

opm_string <- function(version = FALSE) {
  x <- "opm"
  if (!version)
    return(x)
  if (exists("opm.version", MEMOIZED))
    y <- MEMOIZED$opm.version
  else
    MEMOIZED$opm.version <- y <- tryCatch(
      expr = as.character(packageVersion(x)), error = function(e) {
        warning(sprintf("cannot find %s version", x))
        UNKNOWN_VERSION
      })
  c(x, y)
}

is_cas <- function(x) {
  ms <- function(x, m, i) { # get the substring from the chosen capture
    start <- attr(m, "capture.start")[, i]
    substr(x, start, start + attr(m, "capture.length")[, i] - 1L)
  }
  cmp <- function(digits, check) { # compare check digits
    sum_up <- function(x) 0.1 * sum(seq.int(length(x), 1L) * as.numeric(x))
    s <- vapply(strsplit(digits, "", TRUE), sum_up, 0)
    abs(s - floor(s) - 0.1 * as.numeric(check)) < .Machine$double.eps ^ 0.5
  }
  m <- regexpr("^(?:CAS\\s+)?(\\d{2,7})-(\\d{2})-(\\d)$", x, TRUE, TRUE)
  f <- attr(m, "match.length") > 0L
  ok <- f & !is.na(x) # NA values in 'x' should yield NA values in 'f'
  f[ok] <- cmp(paste0(ms(x, m, 1L)[ok], ms(x, m, 2L)[ok]), ms(x, m, 3L)[ok])
  names(f) <- x
  f
}

map_param_names <- function(subset = NULL, ci = TRUE, plain = FALSE,
    opm.fast = FALSE, disc = FALSE) {
  part.1 <- as.list(CURVE_PARAMS)
  names(part.1) <- if (opm.fast)
    c("mu", "lambda", "A", "AUC")
  else
    c("mu", "lambda", "A", "integral")
  if (disc)
    part.1$disc <- DISC_PARAM
  if (plain)
    return(part.1)
  if (length(subset) > 0L) {
    subset <- match.arg(subset, part.1, several.ok = TRUE)
    part.1 <- part.1[part.1 %in% subset]
  }
  if (ci) {
    part.2 <- paste(part.1, "CI95 low")
    part.3 <- paste(part.1, "CI95 high")
    if (opm.fast) {
      names(part.2) <- sprintf("%s.ci.low", names(part.1))
      names(part.3) <- sprintf("%s.ci.high", names(part.1))
    } else {
      names(part.2) <- sprintf("ci95.%s.bt.lo", names(part.1))
      names(part.3) <- sprintf("ci95.%s.bt.up", names(part.1))
    }
  } else {
    part.2 <- NULL
    part.3 <- NULL
  }
  if (opm.fast)
    names(part.1) <- sprintf("%s.point.est", names(part.1))
  else
    names(part.1) <- sprintf("%s.spline", names(part.1))
  c(part.1, part.2, part.3)
}

well_index <- function(x, names) {
  if (missing(x))
    TRUE
  else if (is.character(x))
    clean_coords(x)
  else if (inherits(x, "formula"))
    eval(x[[length(x)]],
      structure(.Data = as.list(seq_along(names)), names = names))
  else
    x
}

time_index <- function(x, times) {
  if (missing(x))
    TRUE
  else if (inherits(x, "formula") && is.atomic(x <- x[[length(x)]]))
    if (anyNA(x))
      !duplicated.default(times)
    else
      times <= x
  else
    x
}

clean_coords <- function(x) {
  do_clean <- function(x) {
    x <- sub("\\s+$", "", sub("^\\s+", "", x, FALSE, TRUE), FALSE, TRUE)
    sprintf("%s%02i", toupper(substr(x, 1L, 1L)),
      as.integer(sub("^[A-Za-z]+", "", x, FALSE, TRUE)))
  }
  if (any(bad <- !grepl("^[A-Z]\\d{2,2}$", x, FALSE, TRUE)))
    x[bad] <- do_clean(x[bad])
  x
}

clean_plate_positions <- function(x) {
  x <- lapply(strsplit(x, "\\W+", FALSE, TRUE), function(s) s[nzchar(s)])
  n <- as.integer(vapply(x, `[[`, "", 1L))
  x <- toupper(substr(vapply(x, `[`, "", 2L), 1L, 1L))
  x[is.na(x)] <- "?" # Microstation positions are only integers
  sprintf("%02i-%s", n, x)
}

map_well_names <- function(wells, plate, in.parens = FALSE, brackets = FALSE,
    paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    max = opm_opt("max.chars"), ...) {
  if ((L(paren.sep) == "@"))
    return(sprintf("%s@%s", wells, plate))
  if (custom_plate_is(plate)) {
    if (custom_plate_exists(plate))
      res <- custom_plate_get(plate)[wells]
    else
      res <- NULL
  } else {
    if (is.na(pos <- match(plate, colnames(WELL_MAP))))
      res <- NULL
    else
      res <- WELL_MAP[wells, pos, "name"]
  }
  if (is.null(res)) {
    warning("cannot find plate type ", plate)
    return(trim_string(str = wells, max = max, ...))
  }
  if (rm.num)
    res <- remove_concentration(res)
  if (downcase)
    res <- substrate_info(res, "downcase")
  if (in.parens)
    add_in_parens(str.1 = wells, str.2 = res, brackets = brackets,
      paren.sep = paren.sep, max = max, ...)
  else
    trim_string(str = res, max = max, ...)
}

well_to_substrate <- function(x, plate) {
  get_name <- function(x, plate) wells(x, TRUE, FALSE, plate = plate)[, 1L]
  if (length(plate)) {
    if (all(grepl(SUBSTRATE_PATTERN[["any"]], x, FALSE, TRUE)))
      get_name(substr(x, 1L, 3L), plate)
    else
      x # assume plain substrate names without wells as prefix
  } else if (all(grepl("^[A-Z][0-9]{2}@", x, FALSE, TRUE))) {
    plate <- as.factor(substr(x, 5L, nchar(x)))
    pos <- split.default(seq_along(x), plate)
    x <- split.default(substr(x, 1L, 3L), plate)
    x <- mapply(FUN = get_name, x = x, plate = names(x), SIMPLIFY = FALSE)
    result <- character(length(plate))
    for (i in seq_along(x))
      result[pos[[i]]] <- x[[i]]
    result
  } else {
    for (p in SUBSTRATE_PATTERN[c("paren", "bracket")]) {
      m <- regexpr(p, x, FALSE, TRUE)
      if (all(attr(m, "match.length") > 0L))
        return(get_partial_match(1L, m, x))
    }
    x
  }
}

to_sentence <- function(x, ...) UseMethod("to_sentence")

to_sentence.logical <- function(x, html, ...) {
  sentence <- function(x, what) {
    if (length(x)) {
      if (html)
        x <- substrate_info(x, "html")
      sprintf("%s for %s.", what, listing(x, style = "sentence"))
    } else
      ""
  }
  LL(html)
  isna <- is.na(x)
  n <- c("Positive", "Negative", "Ambiguous")
  result <- c(sentence(names(x)[x & !isna], n[1L]),
    sentence(names(x)[!x & !isna], n[2L]), sentence(names(x)[isna], n[3L]))
  if (html)
    result <- sprintf("<div>%s</div>", result)
  names(result) <- n
  result
}

web_query <- function(ids, what = c("kegg", "drug")) {
  get_kegg <- function(x, prepend) {
    compound_object <- function(x) {
      pos <- match(c("EXACT_MASS", "MOL_WEIGHT"), names(x), 0L)
      for (p in pos[pos > 0L])
        x[[p]] <- as.numeric(x[[p]])
      class(x) <- c("kegg_compound", "print_easy")
      x
    }
    chunks <- function(x, n) split.default(x,
      rep(seq_len(ceiling(length(x) / n)), each = n)[seq_along(x)])
    run_keggrest <- function(x, prepend) {
      result <- lapply(chunks(paste0(prepend, x), 10), KEGGREST::keggGet)
      result <- lapply(unlist(result, FALSE), compound_object)
      names(result) <- vapply(result, `[[`, "", "ENTRY")
      found <- match(names(result), x, 0L)
      if (!all(found > 0L))
        stop("KEGG request yielded entries that do not match the query")
      structure(.Data = result[found], names = x)
    }
    prepend <- paste0(match.arg(prepend, c("cpd", "drug")), ":")
    got <- get_and_remember(x = x, prefix = "KEGG.", getfun = run_keggrest,
      default = compound_object(list()), prepend = prepend)
    structure(.Data = got, names = names(x),
      class = c("kegg_compounds", "print_easy"))
  }
  case(match.arg(what),
    kegg = get_kegg(ids, "cpd"),
    drug = get_kegg(ids, "drug")
  )
}

NULL

collect.kegg_compounds <- function(x,
    what = c("pathway", "brite", "activity", "exact_mass"),
    missing.na = TRUE, ...) {
  partial_matrix <- function(name, x) {
    convert <- list(
      ACTIVITY = function(x) {
        # notes in brackets make entries more specific; we use both variants
        unique.default(c(x, sub("\\s+\\[.*", "", x, FALSE, TRUE)))
      },
      BRITE = function(x) {
        if (!length(x))
          return(character())
        # remove the starting points of the classifications (which are just
        # their names) and the end points (the substrates themselves)
        m <- attr(regexpr("^\\s+", x, FALSE, TRUE), "match.length")
        x <- x[!(m < 0L | c(m[-1L] < m[-length(m)], TRUE))]
        gsub("\\s+", " ", sub("^\\s+", "", x, FALSE, TRUE), FALSE, TRUE)
      },
      PATHWAY = names,
      EXACT_MASS = function(x) if (is.null(x))
        NA_real_
      else
        x
    )
    result <- lapply(lapply(x, `[[`, name), convert[[name]])
    if (name == "EXACT_MASS")
      matrix(unlist(result), ncol = 1L, dimnames = list(NULL, tolower(name)))
    else
      collect(result, "occurrences")
  }
  what <- toupper(match.arg(what, several.ok = TRUE))
  result <- do.call(cbind, lapply(what, partial_matrix, x))
  if (L(missing.na))
    result[!lengths(x, FALSE), ] <- as(NA, typeof(result))
  result
}

create_listing <- function(x, as.groups,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) {
  add_stuff <- function(x, html, cutoff) {
    class(x) <- "OPMS_Listing"
    attr(x, "html") <- html
    attr(x, "cutoff") <- cutoff
    x
  }
  LL(cutoff, sep)
  if (!length(as.groups)) {
    res <- do.call(rbind, lapply(X = plates(x), FUN = listing, html = html,
      downcase = downcase, full = full, in.parens = in.parens,
      as.groups = NULL, ...))
    rownames(res) <- seq_len(nrow(res))
    return(add_stuff(res, html, cutoff))
  }
  res <- extract(object = x, subset = DISC_PARAM, as.groups = as.groups,
    sep = sep, exact = exact, strict = strict, downcase = downcase,
    full = full, in.parens = in.parens, dataframe = FALSE, as.labels = NULL,
    ...)
  res <- vapply(split.default(seq_len(nrow(res)), attr(res, "row.groups")),
    function(idx) to_sentence(reduce_to_mode.matrix(res[idx, , drop = FALSE],
      cutoff, TRUE), html), character(3L))
  add_stuff(t(res), html, cutoff)
}

