opm_files <- function(what = c("scripts", "testdata", "auxiliary", "demo",
    "doc", "css", "sql", "omnilog", "single", "multiple", "growth")) {
  what <- match.arg(what)
  switch(EXPR = what,
    css = grep("\\.css$", pkg_files(opm_string(), "auxiliary"),
      TRUE, TRUE, TRUE),
    growth = grep("\\.txt(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    multiple = grep("Multiple\\.csv(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    omnilog = grep("(Example(_Old_Style)?_\\d+\\.csv|\\.exl)(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    single = grep("Multiple\\.csv(\\.[^.]+)?$", pkg_files(opm_string(),
      "testdata"), TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
    sql = grep("\\.sql$", pkg_files(opm_string(), "auxiliary"),
      TRUE, TRUE, TRUE),
    pkg_files(opm_string(), what)
  )
}

select_colors <- function(
    set = c("w3c", "w3c.i", "w3c.r", "nora", "nora.i", "nora.r", "brewer",
      "brewer.i", "brewer.r", "roseo", "roseo.i", "roseo.r", "ssnot",
      "ssnot.i", "ssnot.r", "phrogz", "phrogz.i", "phrogz.r", "groups",
      "groups.i", "groups.r", "jp01", "jp01.i", "jp01.r", "jp02", "jp02.i",
      "jp02.r", "jp03", "jp03.i", "jp03.r", "jp04", "jp04.i", "jp04.r",
      "jp05", "jp05.i", "jp05.r", "jp06", "jp06.i", "jp06.r", "jp07",
      "jp07.i", "jp07.r", "jp08", "jp08.i", "jp08.r", "jp09", "jp09.i",
      "jp09.r", "jp10", "jp10.i", "jp10.r", "jp11", "jp11.i", "jp11.r",
      "jp12", "jp12.i", "jp12.r", "jp13", "jp13.i", "jp13.r", "jp14",
      "jp14.i", "jp14.r", "jp15", "jp15.i", "jp15.r", "jp16", "jp16.i",
      "jp16.r", "jp17", "jp17.i", "jp17.r", "jp18", "jp18.i", "jp18.r",
      "jp19", "jp19.i", "jp19.r", "jp20", "jp20.i", "jp20.r", "jp21",
      "jp21.i", "jp21.r", "jp22", "jp22.i", "jp22.r", "jp23", "jp23.i",
      "jp23.r", "jp24", "jp24.i", "jp24.r", "jp25", "jp25.i", "jp25.r",
      "jp26", "jp26.i", "jp26.r", "jp27", "jp27.i", "jp27.r", "jp28",
      "jp28.i", "jp28.r", "jp29", "jp29.i", "jp29.r", "jp30", "jp30.i",
      "jp30.r", "jp31", "jp31.i", "jp31.r", "jp32", "jp32.i", "jp32.r",
      "jp33", "jp33.i", "jp33.r", "jp34", "jp34.i", "jp34.r", "jp35",
      "jp35.i", "jp35.r", "jp36", "jp36.i", "jp36.r", "jp37", "jp37.i",
      "jp37.r", "jp38", "jp38.i", "jp38.r", "jp39", "jp39.i", "jp39.r",
      "jp40", "jp40.i", "jp40.r")) {
  fetch <- function(x) get(x, NULL, COLORS, "character", FALSE)
  if (!missing(set) && is.numeric(set))
    set <- sprintf("jp%02.0f", set)
  set <- match.arg(set)
  m <- regexpr(".", set, FALSE, FALSE, TRUE)
  if (m > 0L) {
    suffix <- substr(set, m + 1L, nchar(set))
    set <- substr(set, 1L, m - 1L)
  } else {
    suffix <- "n"
  }
  result <- fetch(toupper(set))
  switch(EXPR = suffix, n = result, i = rev.default(result), r = sample(result))
}

setGeneric("plate_type", function(object, ...) standardGeneric("plate_type"))

setMethod("plate_type", "OPM", function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  plate_type(object = object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]], ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", "MOPMX", function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  vapply(X = object@.Data, FUN = plate_type, FUN.VALUE = "", ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", "OPM_DB", function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  plate_type(object = object@plates[, "plate_type"], ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", "character", function(object, full = FALSE,
    in.parens = TRUE, max = opm_opt("max.chars"), clean = TRUE,
    brackets = FALSE, word.wise = FALSE, paren.sep = " ", downcase = FALSE,
    normalize = TRUE, subtype = FALSE) {
  do_normalize <- function(object, subtype) {
    is.custom <- custom_plate_is(object)
    object[!is.custom] <- normalize_predefined_plate(object[!is.custom],
      subtype)
    object[is.custom] <- custom_plate_normalize(object[is.custom])
    object
  }
  orig_and_full <- function(orig, full.name) {
    if (downcase)
      full.name <- substrate_info(full.name, "downcase")
    if (in.parens)
      add_in_parens(str.1 = orig, str.2 = full.name, max = max,
        clean = clean, brackets = brackets, word.wise = word.wise,
        paren.sep = paren.sep)
    else
      trim_string(str = full.name, max = max, clean = clean,
        word.wise = word.wise)
  }
  expand_predefined <- function(x) {
    pos <- match(x, names(PLATE_MAP))
    ok <- !is.na(pos)
    for (name in x[!ok])
      warning("cannot find full name of plate ", name)
    x[ok] <- orig_and_full(x[ok], PLATE_MAP[pos[ok]])
    x
  }
  expand_custom <- function(x) {
    if (!length(x))
      return(x)
    n <- custom_plate_prepend_full(custom_plate_proper(x))
    ok <- vapply(n, exists, NA, MEMOIZED)
    for (name in x[!ok])
      warning("cannot find full name of plate ", name)
    x[ok] <- orig_and_full(x[ok], unlist(mget(n[ok], MEMOIZED), FALSE, FALSE))
    x
  }
  LL(full, downcase, in.parens, normalize, subtype)
  result <- if (normalize)
    do_normalize(object, subtype)
  else
    object
  if (!full)
    return(result)
  is.custom <- custom_plate_is(result)
  result[!is.custom] <- expand_predefined(result[!is.custom])
  result[is.custom] <- expand_custom(result[is.custom])
  result
}, sealed = SEALED)

setMethod("plate_type", "factor", function(object, ...) {
  map_values(object = object, mapping = plate_type, ...)
}, sealed = SEALED)

setMethod("plate_type", "missing", function(object, ...) {
  x <- ls(MEMOIZED)
  plate_type(c(names(PLATE_MAP), x[custom_plate_is(x)]), ...)
}, sealed = SEALED)

setMethod("plate_type", "logical", function(object, ...) {
  if (is.na(L(object))) {
    x <- ls(MEMOIZED)
    x <- c(names(PLATE_MAP), x[custom_plate_is(x)])
  } else if (object) {
    x <- ls(MEMOIZED)
    x <- x[custom_plate_is(x)]
  } else {
    x <- names(PLATE_MAP)
  }
  plate_type(x, ...)
}, sealed = SEALED)

setGeneric("gen_iii", function(object, ...) standardGeneric("gen_iii"))

setMethod("gen_iii", "OPM", function(object, to = "gen.iii", force = FALSE) {
  get <- function(name, map) map[[match.arg(tolower(name), names(map))]]
  if (custom_plate_is(L(to))) {
    to <- custom_plate_normalize(to)
    custom_plate_assert(to, colnames(object@measurements)[-1L])
  } else {
    to <- get(to, if (L(force))
      structure(.Data = names(PLATE_MAP),
        names = make.names(tolower(names(PLATE_MAP))))
    else
      SPECIAL_PLATES)
  }
  object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]] <- to
  object
}, sealed = SEALED)

setMethod("gen_iii", "OPMS", function(object, ...) {
  object@plates <- lapply(X = object@plates, FUN = gen_iii, ...)
  object
}, sealed = SEALED)

setMethod("gen_iii", "MOPMX", function(object, ...) {
  object@.Data <- mapply(FUN = gen_iii, object = object@.Data, ...,
    MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  object
}, sealed = SEALED)

setGeneric("register_plate",
  function(object, ...) standardGeneric("register_plate"))

setMethod("register_plate", "character", function(object, ...) {
  x <- do.call(c, lapply(object, function(file)
    tryCatch(expr = yaml.load_file(file), error = function(e) readRDS(file))))
  x <- mapply(FUN = function(d, n) {
      names(d) <- rep.int(n, length(d))
      d
    }, d = x, n = names(x), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  register_plate(do.call(c, x), ...)
}, sealed = SEALED)

setMethod("register_plate", "missing", function(object, ...) {
  register_plate(list(...))
}, sealed = SEALED)

setMethod("register_plate", "list", function(object, ...) {
  valid_names <- function(n) length(n) && !anyNA(n) && all(nzchar(n))
  prepare_names <- function(n) {
    if (!valid_names(n))
      stop("all arguments must be validly named")
    n <- ifelse(custom_plate_is(n), custom_plate_proper(n), n)
    custom_plate_normalize_proper(n)
  }
  convert_rectangular_coords <- function(x) {
    if (!length(j <- as.integer(colnames(x))))
      j <- seq_len(ncol(x))
    if (!length(i <- rownames(x)))
      i <- rep(LETTERS, length.out = nrow(x))
    n <- vapply(i, sprintf, character(length(j)), fmt = "%s%02i", j)
    structure(.Data = c(t(x)), names = c(n))
  }
  prepare_well_map <- function(x) {
    if (inherits(x, "well_coords_map")) {
      case(ncol(x), stop("'well_coords_map' object has zero columns"),
        x <- x[, 1L], {
          warning("discarding additional columns in 'well_coords_map' object")
          x <- x[, 1L]
        })
    } else if (is.data.frame(x)) {
      for (i in which(vapply(x, is.factor, NA)))
        x[, i] <- as.character(x[, i])
      x <- convert_rectangular_coords(as.matrix(x))
    } else if (is.matrix(x)) {
      x <- convert_rectangular_coords(x)
    } else {
      names(x) <- clean_coords(names(x))
    }
    storage.mode(x) <- "character"
    if (dup <- anyDuplicated(names(x)))
      stop("duplicate well coordinate provided: ", names(x)[dup])
    x
  }
  insert_plate_types <- function(x) {
    named <- vapply(lapply(x, names), valid_names, NA) |
      vapply(x, is.data.frame, NA) | vapply(x, is.matrix, NA)
    if (any(lengths(x, FALSE) > 1L & !named))
      stop("element unnamed but not of length 1")
    x[named] <- lapply(x[named], prepare_well_map)
    names(x) <- ifelse(named, custom_plate_prepend(names(x)),
      custom_plate_prepend_full(names(x)))
    list2env(x, MEMOIZED)
  }
  remove_plate_types <- function(x) {
    x <- c(custom_plate_prepend(x), custom_plate_prepend_full(x))
    suppressWarnings(rm(list = x, envir = MEMOIZED))
  }
  if (!missing(...))
    warning("arguments other than 'object' are ignored")
  names(object) <- prepare_names(names(object))
  nonempty <- lengths(object, FALSE) > 0L
  insert_plate_types(object[nonempty])
  remove_plate_types(names(object)[!nonempty])
  structure(.Data = nonempty, names = custom_plate_prepend(names(object)))
}, sealed = SEALED)

setGeneric("wells", function(object, ...) standardGeneric("wells"))

setMethod("wells", "OPM", function(object, full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), brackets = FALSE, clean = TRUE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    plate = plate_type(object), prefix = FALSE, simplify = TRUE) {
  LL(full, simplify, plate)
  x <- colnames(object@measurements)[-1L]
  if (!missing(plate))
    plate <- if (custom_plate_is(plate))
      custom_plate_normalize(plate)
    else
      normalize_predefined_plate(plate)
  if (full)
    x <- structure(.Data = map_well_names(wells = x, plate = plate,
      in.parens = in.parens, max = max, brackets = brackets, clean = clean,
      word.wise = word.wise, paren.sep = paren.sep, downcase = downcase,
      rm.num = rm.num, prefix = prefix), names = x)
  if (simplify)
    return(x)
  x <- matrix(x, length(x), 1L, FALSE, list(names(x), plate))
  class(x) <- "well_coords_map"
  x
}, sealed = SEALED)

setMethod("wells", "ANY", function(object, full = TRUE, in.parens = FALSE,
    max = opm_opt("max.chars"), brackets = FALSE, clean = TRUE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    plate = "PM01", prefix = FALSE, simplify = FALSE) {
  LL(full, simplify)
  x <- well_index(object, rownames(WELL_MAP))
  if (!is.character(x))
    x <- rownames(WELL_MAP)[x]
  ok <- is.custom <- custom_plate_is(plate)
  x <- matrix(x, length(x), length(plate), FALSE, list(x, ifelse(is.custom,
    custom_plate_normalize(plate), normalize_predefined_plate(plate))))
  ok[is.custom] <- vapply(colnames(x)[is.custom], custom_plate_exists, NA)
  ok[!is.custom] <- match(colnames(x)[!is.custom], colnames(WELL_MAP), 0L) > 0L
  x[, !ok] <- NA_character_
  if (full)
    for (i in which(ok))
      x[, i] <- map_well_names(wells = x[, i], plate = colnames(x)[i],
        in.parens = in.parens, max = max, brackets = brackets, clean = clean,
        word.wise = word.wise, paren.sep = paren.sep, downcase = downcase,
        rm.num = rm.num, prefix = prefix)
  if (simplify && ncol(x) == 1L)
    return(x[, 1L])
  class(x) <- "well_coords_map"
  x
}, sealed = SEALED)

setMethod("wells", "missing", function(object, ...) {
  wells(object = TRUE, ...)
}, sealed = SEALED)

setOldClass("well_coords_map")

setClass("well_coords_listing", contains = "print_easy")

setMethod("listing", "well_coords_map", function(x) {
  x <- x[!apply(is.na(x), 1L, all), , drop = FALSE]
  result <- structure(.Data = vector("list", ncol(x)),
    names = plate <- colnames(x))
  full <- ifelse(custom_plate_is(plate),
    mget(custom_plate_prepend_full(custom_plate_proper(plate)), MEMOIZED,
    "character", rep.int(list(NA_character_), length(plate))), PLATE_MAP[plate])
  for (i in seq_along(result))
    result[[i]] <- list(full[[i]], as.list(x[, i]))
  class(result) <- c("well_coords_listing", "print_easy")
  result
}, sealed = SEALED)

setMethod("listing", "OPMD", function(x, as.groups,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) {
  res <- to_sentence(discretized(object = x, full = full,
    in.parens = in.parens, downcase = downcase, ...), html)
  if (length(as.groups)) {
    res <- matrix(res, 1L, length(res), FALSE, list(NULL, names(res)))
    rownames(res) <- paste0(metadata(x, as.groups, exact, strict),
      collapse = L(sep))
    attr(res, "cutoff") <- L(cutoff)
    class(res) <- "OPMS_Listing"
  } else
    class(res) <- "OPMD_Listing"
  attr(res, "html") <- html
  res
}, sealed = SEALED)

setMethod("listing", "MOPMX", function(x, as.groups,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) {
  create_listing(x = x, as.groups = as.groups, cutoff = cutoff,
    downcase = downcase, full = full, in.parens = in.parens, html = html,
    sep = sep, ..., exact = exact, strict = strict)
}, sealed = SEALED)

setMethod("listing", "OPMX", function(x, as.groups,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) {
  create_listing(x = x, as.groups = as.groups, cutoff = cutoff,
    downcase = downcase, full = full, in.parens = in.parens, html = html,
    sep = sep, ..., exact = exact, strict = strict)
}, sealed = SEALED)

setGeneric("find_substrate",
  function(object, ...) standardGeneric("find_substrate"))

setMethod("find_substrate", "character", function(object,
    search = c("exact", "glob", "approx", "regex", "pmatch"), max.dev = 0.2) {
  su <- function(x) lapply(lapply(x, unique.default), sort.int)
  find_name <- function(patterns, ...) {
    su(lapply(X = patterns, FUN = grep, x = WELL_MAP[, , "name"], value = TRUE,
      useBytes = TRUE, ...))
  }
  find_approx <- function(pattern, ...) {
    su(lapply(X = pattern, FUN = agrep, x = WELL_MAP[, , "name"], value = TRUE,
      ignore.case = TRUE, useBytes = TRUE, ...))
  }
  find_partial <- function(pattern) {
    # next step necessary because multiple <partial> matches are never allowed
    table <- unique.default(WELL_MAP[, , "name"])
    found <- table[pmatch(pattern, table, NA_integer_, TRUE)]
    names(found) <- pattern
    lapply(lapply(as.list(found), na.exclude), sort.int)
  }
  result <- case(match.arg(search),
    exact = find_name(object, fixed = TRUE),
    glob = find_name(structure(.Data = glob_to_regex(object), names = object),
      ignore.case = TRUE, perl = TRUE),
    regex = find_name(object, ignore.case = TRUE, perl = TRUE),
    approx = find_approx(object, max.distance = max.dev),
    pmatch = find_partial(object)
  )
  names(result) <- object
  class(result) <- c("substrate_match", "print_easy")
  result
}, sealed = SEALED)

setOldClass("substrate_match")

setGeneric("find_positions",
  function(object, ...) standardGeneric("find_positions"))

setMethod("find_positions", "character", function(object, type = NULL, ...) {
  if (length(type) && !identical(type, FALSE)) {
    x <- WELL_MAP[, plate_type(type)[1L], "name"]
    return(structure(.Data = names(x)[match(object, x)], names = object))
  }
  plates <- colnames(WELL_MAP)
  sapply(X = object, FUN = function(name) {
    result <- which(WELL_MAP[, , "name"] == name, arr.ind = TRUE)
    matrix(c(plates[result[, 2L]], rownames(result)), ncol = 2L,
      dimnames = list(NULL, RESERVED_NAMES[c("plate", "well")]))
  }, simplify = FALSE)
}, sealed = SEALED)

setMethod("find_positions", "substrate_match", function(object, ...) {
  rapply(object = object, f = find_positions, "character", how = "list", ...)
}, sealed = SEALED)

setMethod("find_positions", "list", function(object, ...) {
  rapply(object = object, f = find_positions,
    classes = c("character", "factor"), how = "list", ...)
}, sealed = SEALED)

setMethod("find_positions", "OPM", function(object, type = NULL, ...) {
  object <- wells(object, full = TRUE, in.parens = FALSE)
  if (isTRUE(type))
    structure(.Data = names(object), names = object)
  else
    find_positions(object, ...)
}, sealed = SEALED)

setGeneric("substrate_info",
  function(object, ...) standardGeneric("substrate_info"))

setMethod("substrate_info", "character", function(object,
    what = c("cas", "kegg", "drug", "metacyc", "chebi", "mesh", "seed",
      "downcase", "greek", "concentration", "html", "peptide", "peptide2",
      "all"),
    browse = 0L, download = FALSE, ...) {

  find_substrate_id <- function(x) {
    result <- WELL_MAP[, , "substrate_id"][match(x, WELL_MAP[, , "name"])]
    structure(.Data = as.integer(result), names = x)
  }

  create_url <- function(x, how) {
    url_base <- c(
      kegg = "http://www.genome.jp/dbget-bin/www_bget?cpd:",
      drug = "http://www.genome.jp/dbget-bin/www_bget?dr:",
      chebi = "http://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI:",
      metacyc = "http://biocyc.org/META/NEW-IMAGE?type=COMPOUND&object=",
      cas = "http://chem.sis.nlm.nih.gov/chemidplus/direct.jsp?regno=",
      mesh = "http://www.ncbi.nlm.nih.gov/mesh/",
      seed = paste0("http://seed-viewer.theseed.org/seedviewer.cgi?",
        "page=CompoundViewer&compound=")
    )
    base <- url_base[[match.arg(how, names(url_base))]]
    x <- sub("^(CAS\\s+|CHEBI:)", "", x, TRUE, TRUE)
    ifelse(is.na(x), NA_character_, paste0(base, vapply(x, URLencode, "")))
  }

  map_words <- function(x, fun, ...) {
    y <- strsplit(x, "\\w+", FALSE, TRUE)
    x <- strsplit(x, "\\W+", FALSE, TRUE)
    bad <- !vapply(x, function(value) nzchar(value[1L]), NA)
    x[bad] <- lapply(x[bad], `[`, i = -1L)
    bad <- lengths(x, FALSE) < lengths(y, FALSE)
    x[bad] <- lapply(x[bad], function(value) c(value, ""))
    x <- lapply(X = x, FUN = fun, ...) # fun() must keep the length!
    mapply(FUN = paste0, x = y, y = x, MoreArgs = list(collapse = ""))
  }

  expand_greek_letters <- function(x) {
    map_words(x = x, fun = map_values, mapping = GREEK_LETTERS)
  }

  compound_name_to_html <- function(x) {
    x <- gsub("'", "&prime;", safe_labels(x, "html"), FALSE, FALSE, TRUE)
    map_words(x = x, fun = map_values, mapping = COMPOUND_NAME_HTML_MAP)
  }

  safe_downcase <- function(x) {
    good_case <- function(x) {
      bad <- nchar(x) > 1L # avoid changing acronyms and chemical elements
      bad[bad] <- !grepl("^(pH|[a-z]?[A-Z][A-Z]+|([A-Z][a-z]?\\d*)+)$", x[bad],
        FALSE, TRUE)
      x[bad] <- tolower(x[bad])
      x
    }
    map_words(x, function(y) map_values(good_case(y), GREEK_LETTERS))
  }

  extract_concentration <- function(x) {
    in.parens <- grepl(SUBSTRATE_PATTERN[["either"]], x, FALSE, TRUE)
    x <- ifelse(in.parens, substr(x, 1L, nchar(x) - 1L), x)
    m <- regexpr("(?<=#)\\s*\\d+\\s*$", x, FALSE, TRUE)
    ## The following code is currently not in use because the only plate to
    ## which it is applicable (PM09) does not show regularity anyway. Conversion
    ## to integer would also be problematic because contractions such as 5.5 or
    ## 6.5 are present.
    #if (all(m < 0L)) {
    #  x <- ifelse(in.parens, substr(x, 6L, nchar(x)), x)
    #  m <- regexpr("^(?:\\d+(?:\\.\\d+)?)(?=%|mM)", x, FALSE, TRUE)
    #}
    as.integer(substr(x, m, m + attr(m, "match.length") - 1L))
  }

  parse_peptide <- function(x, remove.L) {
    recognize_full_names <- function(x) {
      m <- regexpr("^(?:[A-Za-z][,-])*[A-Za-z]-", x, FALSE, TRUE)
      result <- AMINO_ACIDS[substr(x, m + attr(m, "match.length"), nchar(x))]
      ok <- !is.na(result)
      prefix <- m > 0L & ok
      m <- substr(x, m, m + attr(m, "match.length") - 1L)
      result[prefix] <- paste0(m[prefix], result[prefix])
      result <- as.list(result)
      result[!ok] <- list(character())
      result
    }
    result <- structure(.Data = vector("list", length(x)), names = x)
    x <- remove_concentration(x)
    pat <- "(([A-Za-z][,-])*[A-Za-z]-)?[A-Z][a-z]{2}"
    pat <- sprintf("^%s(-%s)*$", pat, pat)
    ok <- grepl(pat, x, FALSE, TRUE)
    result[ok] <- strsplit(x[ok], "(?<!\\b\\w)-", FALSE, TRUE)
    result[!ok] <- recognize_full_names(x[!ok])
    if (remove.L)
      result <- lapply(X = result, FUN = sub, pattern = "^L-",
        replacement = "", ignore.case = FALSE, perl = TRUE)
    result
  }

  all_information <- function(x) {
    result <- SUBSTRATE_INFO[find_substrate_id(x), , drop = FALSE]
    colnames(result) <- map_values(colnames(result),
      c(METACYC = "MetaCyc", MESH = "MeSH", CHEBI = "ChEBI",
        KEGG = "KEGG compound", DRUG = "KEGG drug"))
    result <- split.data.frame(result, seq_len(nrow(result)))
    result <- lapply(result, function(y) y[, !is.na(y), drop = TRUE])
    class(result) <- c("substrate_data", "print_easy")
    result
  }

  result <- case(what <- match.arg(what),
    all = all_information(object),
    chebi =, drug =, kegg =, metacyc =, mesh =, seed =,
    cas = SUBSTRATE_INFO[find_substrate_id(object), toupper(what)],
    concentration = extract_concentration(object),
    downcase = safe_downcase(object),
    greek = expand_greek_letters(object),
    html = compound_name_to_html(object),
    peptide = parse_peptide(object, TRUE),
    peptide2 = parse_peptide(object, FALSE)
  )
  browse <- must(as.integer(L(browse)))
  if (browse != 0L) {
    result <- create_url(result, what)
    if (browse > 0L)
      lapply(head(result[!is.na(result)], browse), browseURL)
  }
  names(result) <- object
  if (L(download))
    result <- web_query(result, what)
  result
}, sealed = SEALED)

setMethod("substrate_info", "substrate_match", function(object, ...) {
  rapply(object = object, f = substrate_info, classes = "character",
    how = "replace", ...)
}, sealed = SEALED)

setMethod("substrate_info", "list", function(object, ...) {
  rapply(object = object, f = substrate_info, how = "replace",
    classes = c("character", "factor"), ...)
}, sealed = SEALED)

setMethod("substrate_info", "OPM", function(object, ...) {
  substrate_info(wells(object = object, full = TRUE, in.parens = FALSE), ...)
}, sealed = SEALED)

lapply(c(
    #+
    find_substrate,
    find_positions,
    substrate_info
    #-
  ), FUN = function(func_) {
  setMethod(func_, "factor", function(object, ...) {
    func_(as.character(object), ...)
  }, sealed = SEALED)
})

lapply(c(
    #+
    find_positions,
    substrate_info,
    wells,
    plate_type
    #-
  ), FUN = function(func_) {
  setMethod(func_, "OPMS", function(object, ...) {
    func_(object@plates[[1L]], ...)
  }, sealed = SEALED)
})

lapply(c(
    #+
    find_positions,
    substrate_info,
    wells
    #-
  ), FUN = function(func_) {
  setMethod(func_, "MOPMX", function(object, ...) {
    lapply(X = object@.Data, FUN = func_, ...)
  }, sealed = SEALED)
})

