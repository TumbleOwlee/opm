explode_dir <- function(names,
    include = NULL, exclude = NULL, ignore.case = TRUE, wildcard = TRUE,
    recursive = TRUE, missing.error = TRUE, remove.dups = TRUE) {
  extended_file_pattern <- function(arg, wildcard) {
    if (is.list(arg))
      return(do.call(file_pattern, arg))
    result <- as.character(arg)
    if (wildcard)
      result <- glob_to_regex(result)
    result
  }
  explode_names <- function(names, recursive) {
    is.dir <- file.info(names)$isdir
    if (any(no.info <- is.na(is.dir))) {
      msg <- sprintf("File or directory not found: '%s'",
        paste0(names[no.info], collapse = " "))
      if (missing.error)
        stop(msg)
      else
        warning(msg)
    }
    is.dir <- is.dir[!no.info]
    names <- as.list(names[!no.info]) # use of a list ensures input order
    names[is.dir] <- lapply(X = names[is.dir], FUN = list.files,
      full.names = TRUE, recursive = recursive)
    unlist(names)
  }
  select_files <- function(data, pattern, invert) {
    if (is.null(pattern))
      return(data)
    pattern <- extended_file_pattern(pattern, wildcard)
    grep(pattern, data, ignore.case = ignore.case, value = TRUE,
      invert = invert)
  }
  names <- as.character(names)
  if (remove.dups)
    names <- unique(names)
  result <- explode_names(names, recursive = recursive)
  result <- select_files(result, include, invert = FALSE)
  select_files(result, exclude, invert = TRUE)
}

batch_collect <- function(names, fun, fun.args = list(), proc = 1L, ...,
    use.names = TRUE, simplify = FALSE, demo = FALSE) {
  names <- explode_dir(names, ...)
  if (demo) {
    message(paste0(names, collapse = "\n"))
    return(invisible(names))
  }
  fun.args <- as.list(fun.args)
  mcmapply(MoreArgs = as.list(fun.args), names, # this arg should not be named
    FUN = fun, SIMPLIFY = simplify, USE.NAMES = use.names, mc.cores = proc)
}

batch_process <- function(names, out.ext, io.fun, fun.args = list(), proc = 1L,
    outdir = NULL, overwrite = c("yes", "older", "no"), in.ext = "any",
    compressed = TRUE, literally = inherits(in.ext, "AsIs"), ...,
    verbose = TRUE, demo = FALSE) {
  create_outfile_names <- function(infiles, outdir, out.ext) {
    if (length(outdir) == 0L || all(!nzchar(outdir)))
      outdir <- dirname(infiles)
    result <- sub(in.ext, "", basename(infiles), TRUE, TRUE)
    result <- paste(result, sub("^\\.+", "", out.ext), sep = ".")
    file.path(outdir, result)
  }
  LL(demo, verbose, compressed)
  in.ext <- file_pattern(in.ext, compressed, literally)
  overwrite <- match.arg(overwrite)
  infiles <- explode_dir(names, ...)
  outfiles <- create_outfile_names(infiles, outdir, out.ext)
  if (demo) {
    message(paste(infiles, outfiles, sep = "\n  => ", collapse = "\n"))
    return(invisible(cbind(infiles, outfiles)))
  }
  fun.args <- as.list(fun.args)
  data <- mapply(FUN = c, infiles, outfiles, # do not name these two arguments
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  result <- mclapply(X = data, FUN = process_io, mc.cores = proc,
    io.fun = io.fun, fun.args = fun.args, overwrite = overwrite,
    verbose = verbose)
  invisible(do.call(rbind, result))
}

file_pattern <- function(
    type = c("both", "csv", "yaml", "json", "yorj", "lims", "nolims", "any",
      "empty"),
    compressed = TRUE, literally = inherits(type, "AsIs")) {
  make_pat <- function(x, compressed, enclose = "\\.%s$") {
    if (compressed)
      x <- sprintf("%s(\\.(bz2|gz|lzma|xz))?", x)
    sprintf(enclose, x)
  }
  LL(literally, compressed)
  result <- if (literally) {
    x <- make_pat("([^.]+)", compressed, "^.*?\\.%s$")
    x <- sub(x, "\\1", type, FALSE, TRUE)
    if (all(same <- x == basename(type))) { # assuming extensions
      type <- x
      bad <- "^\\w+$"
    } else { # assuming file names
      type <- x[!same]
      bad <- "^\\w+(\\.\\w+)?$"
    }
    if (any(bad <- !grepl(bad, type <- unique.default(type), FALSE, TRUE)))
      stop("'type' must contain word characters (only): ", type[bad][1L])
    case(length(type), stop("'type' must be non-empty"), type,
      sprintf("(%s)", paste0(type, collapse = "|")))
  } else
    case(match.arg(type), both = "(csv|exl|ya?ml|json)", csv = "(csv|exl)",
      yaml = "ya?ml", json = "json", yorj = "(ya?ml|json)", lims = "exl",
      nolims = "csv", any = "[^.]+", empty = "")
  make_pat(result, compressed)
}

split_files <- function(files, pattern, outdir = "", demo = FALSE,
    single = TRUE, wildcard = FALSE, invert = FALSE, include = TRUE,
    format = opm_opt("file.split.tmpl"), compressed = TRUE, ...) {

  create_outnames <- function(files, compressed, outdir) {
    file.pat <- file_pattern("any", compressed = compressed, literally = FALSE)
    out.base <- sub(file.pat, "", files, TRUE, TRUE)
    out.ext <- substr(files, nchar(out.base) + 2L, nchar(files))
    if (compressed)
      out.ext <- sub("\\.[^.]+$", "", out.ext, FALSE, TRUE)
    if (length(outdir) && all(nzchar(outdir)))
      out.base <- file.path(outdir, basename(out.base))
    list(base = out.base, ext = out.ext)
  }

  LL(pattern, outdir, demo, single, wildcard, invert, include,
    format, compressed)
  files <- unique(as.character(files))
  out <- create_outnames(files, compressed = compressed, outdir = outdir)
  if (wildcard)
    pattern <- glob_to_regex(pattern)

  invisible(mapply(function(infile, out.base, out.ext) {
    con <- file(description = infile, encoding = opm_opt("file.encoding"))
    on.exit(close(con))
    data <- readLines(con = con, warn = FALSE)
    data <- sections(x = data, pattern = pattern, invert = invert,
      include = include, ...)
    if ((len <- length(data)) == 0L || (!single && len == 1L))
      return(character())
    outnames <- sprintf(format, out.base, seq_along(data), out.ext)
    if (demo)
      message(listing(structure(.Data = outnames, names = seq_along(outnames)),
        header = infile))
    else
      mapply(FUN = write, x = data, file = outnames, USE.NAMES = FALSE,
        SIMPLIFY = FALSE)
    outnames
  }, files, out$base, out$ext, SIMPLIFY = FALSE))
}

glob_to_regex <- function(object) UseMethod("glob_to_regex")

glob_to_regex.character <- function(object) {
  # TODO: one should perhaps also check for '|'
  x <- glob2rx(gsub("([+^$])", "\\\\\\1", object, FALSE, TRUE))
  attributes(x) <- attributes(object)
  x
}

glob_to_regex.factor <- function(object) {
  levels(object) <- glob_to_regex(levels(object))
  object
}

read_opm <- function(names = getwd(),
    convert = c("grp", "try", "no", "yes", "sep"), gen.iii = opm_opt("gen.iii"),
    include = list(), ..., force = FALSE, demo = FALSE) {
  do_split <- function(x) split(x, vapply(x, plate_type, ""))
  do_opms <- function(x) case(length(x), , x[[1L]],
    new(Class = "OPMS", plates = x))
  convert <- match.arg(convert)
  LL(gen.iii, demo)
  names <- explode_dir(names = names, include = include, ...)
  if (demo) {
    message(paste0(names, collapse = "\n"))
    return(invisible(names))
  }
  # The c() call is necessary to flatten lists from YAML/JSON input.
  result <- c(lapply(names, read_single_opm), recursive = TRUE)
  switch(EXPR = mode(gen.iii),
    logical = if (gen.iii)
      result <- lapply(result, gen_iii),
    character = if (nzchar(gen.iii))
      result <- lapply(X = result, FUN = gen_iii, to = gen.iii, force = force),
    stop("'gen.iii' must either be logical or character scalar")
  )
  case(length(result),
    case(convert,
      no =,
      grp = new("MOPMX"),
      sep = list(),
      yes =,
      try = NULL
    ),
    case(convert,
      no = new("MOPMX", result),
      grp = new("MOPMX", structure(.Data = result,
        names = plate_type(result[[1L]]))),
      sep = structure(.Data = list(new("MOPMX", result)),
        names = plate_type(result[[1L]])),
      yes =,
      try = result[[1L]]
    ),
    case(convert,
      no = new("MOPMX", result),
      yes = new(Class = "OPMS", plates = result),
      grp = new("MOPMX", lapply(do_split(result), do_opms)),
      sep = lapply(X = do_split(result), FUN = new, Class = "MOPMX"),
      try = tryCatch(expr = new(Class = "OPMS", plates = result),
        error = function(e) {
          warning("the data from distinct files could not be converted to a ",
            "single OPMS object and will be returned as a list")
          new("MOPMX", result)
        })
    )
  )
}

FILE_NOT_CSV <- file_pattern(type = "yorj", compressed = TRUE)

FILE_LIMS <- file_pattern(type = "lims", compressed = TRUE)

read_single_opm <- function(filename) {
  if (!file.exists(filename <- as.character(L(filename))))
    stop(sprintf("file '%s' does not exist", filename))
  routines <- list(`New CSV` = read_new_opm, `Old CSV` = read_old_opm,
    `MicroStation CSV` = read_microstation_opm)
  routines <- if (grepl(FILE_NOT_CSV, filename, TRUE, TRUE))
      c(YAML = read_opm_yaml, routines, `LIMS CSV` = read_lims_opm)
    else if (grepl(FILE_LIMS, filename, TRUE, TRUE))
      c(`LIMS CSV` = read_lims_opm, routines, YAML = read_opm_yaml)
    else
      c(routines[get("input.try.order", OPM_OPTIONS)],
        `LIMS CSV` = read_lims_opm, YAML = read_opm_yaml)
  errs <- character(length(routines))
  for (i in seq_along(routines)) {
    result <- tryCatch(expr = routines[[i]](filename), error = conditionMessage)
    if (!is.character(result))
      return(result)
    errs[i] <- result
  }
  names(errs) <- paste(names(routines), "error")
  stop(listing(c(errs, Filename = filename), header = "Unknown file format:"))
}

setGeneric("collect_template",
  function(object, ...) standardGeneric("collect_template"))

setMethod("collect_template", "character", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = -1L,
    instrument = NULL, include = list(), ..., demo = FALSE) {
  do_collect <- function(infile)
    if (is.list(x <- read_single_opm(infile))) # possible in case of YAML input
      do.call(rbind, lapply(X = x, FUN = collect_template,
        selection = selection, normalize = normalize, add.cols = add.cols,
        instrument = instrument, outfile = NULL, previous = NULL, sep = sep,
        md.args = md.args))
    else
      collect_template(object = x, selection = selection, normalize = normalize,
        add.cols = add.cols, instrument = instrument, outfile = NULL,
        previous = NULL, sep = sep, md.args = md.args)
  result <- batch_collect(names = object, fun = do_collect, include = include,
    ..., simplify = FALSE, demo = demo)
  if (!demo)
    result <- do.call(rbind, result)
  rownames(result) <- NULL # if 'previous' was given, row names lacked anyway
  finish_template(result, outfile, sep, previous, md.args, demo)
}, sealed = SEALED)

setMethod("collect_template", "OPM", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = -1L,
    instrument = NULL, ..., demo = FALSE) {
  result <- csv_data(object = object, keys = selection, normalize = normalize)
  result <- as.list(result)
  if (length(instrument)) {
    if (!is.logical(L(instrument)))
      result[[INSTRUMENT]] <- must(as.integer(instrument))
    else if (instrument)
      result[[INSTRUMENT]] <- L(get("machine.id", OPM_OPTIONS))
  }
  result <- as.data.frame(x = result, stringsAsFactors = FALSE, optional = TRUE)
  if (length(add.cols)) {
    add.cols <- matrix(NA_character_, nrow(result), length(add.cols), FALSE,
      list(NULL, add.cols))
    result <- cbind(result, add.cols, stringsAsFactors = FALSE)
  }
  finish_template(result, outfile, sep, previous, md.args, demo)
}, sealed = SEALED)

setMethod("collect_template", "OPMS", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = -1L,
    instrument = NULL, ..., demo = FALSE) {
  result <- lapply(X = object@plates, FUN = collect_template, md.args = md.args,
    add.cols = add.cols, sep = sep, selection = selection, previous = NULL,
    normalize = normalize, instrument = instrument, outfile = NULL, ...)
  finish_template(do.call(rbind, result), outfile, sep, previous, md.args, demo)
}, sealed = SEALED)

setMethod("collect_template", "MOPMX", function(object,
    outfile = NULL, sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL,
    normalize = -1L, instrument = NULL, ..., demo = FALSE) {
  result <- lapply(X = object, FUN = collect_template, selection = selection,
    add.cols = add.cols, normalize = normalize, instrument = instrument,
    outfile = NULL, previous = NULL, sep = sep, md.args = md.args, ...)
  finish_template(do.call(rbind, result), outfile, sep, previous, md.args, demo)
}, sealed = SEALED)

setGeneric("to_metadata",
  function(object, ...) standardGeneric("to_metadata"))

setMethod("to_metadata", "character", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = c("\t", ",", ";"), strip.white = NA, ...) {
  if (length(object) > 1L && !is.null(names(object))) {
    if (is.na(L(strip.white)))
      strip.white <- FALSE
    return(to_metadata(object = vector2row(object), strip.white = strip.white,
      sep = sep, stringsAsFactors = stringsAsFactors, optional = optional, ...))
  }
  if (!length(sep))
    stop("empty 'sep' argument")
  if (is.na(L(strip.white)))
    strip.white <- TRUE
  for (char in sep) {
    x <- read.delim(file = object, sep = char, check.names = !optional,
      strip.white = strip.white, stringsAsFactors = stringsAsFactors, ...)
    if (ncol(x) > 1L)
      break
  }
  x
}, sealed = SEALED)

setMethod("to_metadata", "ANY", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  x <- as.data.frame(x = object, stringsAsFactors = stringsAsFactors,
    optional = optional, ...)
  if (L(strip.white))
    x <- strip_whitespace(x)
  x
}, sealed = SEALED)

setMethod("to_metadata", "WMD", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  x <- collect(x = list(object@metadata), what = "values",
    optional = optional, stringsAsFactors = stringsAsFactors,
    dataframe = TRUE, keep.unnamed = NA, ...)
  if (L(strip.white))
    x <- strip_whitespace(x)
  x
}, sealed = SEALED)

setMethod("to_metadata", "WMDS", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  x <- collect(x = metadata(object), what = "values",
    optional = optional, stringsAsFactors = stringsAsFactors,
    dataframe = TRUE, keep.unnamed = NA, ...)
  if (L(strip.white))
    x <- strip_whitespace(x)
  x
}, sealed = SEALED)

setMethod("to_metadata", "MOPMX", function(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) {
  collect(x = lapply(X = object, FUN = to_metadata, optional = optional,
    sep = sep, stringsAsFactors = FALSE, strip.white = FALSE, ...),
    what = "rows", dataframe = TRUE, stringsAsFactors = stringsAsFactors)
}, sealed = SEALED)

batch_opm <- function(names, md.args = NULL, aggr.args = NULL,
    force.aggr = FALSE, disc.args = NULL, force.disc = FALSE,
    gen.iii = opm_opt("gen.iii"), force.plate = FALSE, device = "mypdf",
    dev.args = NULL, plot.args = NULL, csv.args = NULL,
    table.args = list(sep = "\t", row.names = FALSE),
    ..., proc = 1L, outdir = "", overwrite = "no",
    output = c("yaml", "json", "csv", "xyplot", "levelplot", "split", "clean"),
    combine.into = NULL, verbose = TRUE, demo = FALSE) {

  csv2md <- function(x, spec) {
    if (!is.matrix(x)) # OPM objects yield only a character vector
      x <- t(as.matrix(x))
    x <- to_metadata(x)
    spec <- flatten(list(spec))
    spec <- rapply(spec, as.character, "factor", NULL, "replace")
    if (any(!vapply(spec, inherits, NA, c("character", "function"))))
      stop("can only apply character vector, factor or function to CSV data")
    for (approach in spec)
      if (is.character(approach)) {
        for (name in approach[!approach %in% colnames(x)])
          x[, name] <- seq_len(nrow(x))
        x <- x[, approach, drop = FALSE]
        if (!is.null(names(approach)))
          colnames(x) <- names(approach)
      } else {
        x <- approach(x)
        if (!is.data.frame(x)) # wrong no. of rows should yield error later on
          stop("function applied to CSV data must yield data frame")
      }
    x
  }

  convert_dataset <- function(data) {
    switch(EXPR = mode(gen.iii),
      logical = if (gen.iii) {
        if (verbose)
          message("conversion: changing to 'Generation III'...")
        data <- gen_iii(data, force = force.plate)
      },
      character = if (nzchar(gen.iii)) {
        if (verbose)
          message(sprintf("conversion: changing to '%s'...", gen.iii))
        data <- gen_iii(data, to = gen.iii, force = force.plate)
      },
      stop("'gen.iii' must either be a logical or a character scalar")
    )
    if (length(csv.args)) {
      if (verbose)
        message("conversion: using CSV data as metadata...")
      metadata(data, 1L) <- csv2md(csv_data(data), csv.args)
    }
    if (length(md.args)) {
      if (verbose)
        message("conversion: including metadata...")
      data <- do.call(include_metadata, c(object = data, md.args))
    }
    if (length(aggr.args)) {
      if (force.aggr || !has_aggr(data)) {
        if (verbose)
          message("conversion: aggregating data...")
        data <- do.call(do_aggr, c(list(object = data), aggr.args))
      } else if (verbose)
        message("conversion: previously aggregated data present, ",
          "skipping that step")
    }
    if (length(disc.args)) {
      if (force.aggr || !has_disc(data)) {
        if (verbose)
          message("conversion: discretizing data...")
        data <- do.call(do_disc, c(list(data), disc.args))
      } else if (verbose)
        message("conversion: previously discretized data present, ",
          "skipping that step")
    }
    data
  }

  read_file <- function(infile) {
    data <- read_single_opm(infile)
    if (is.list(data)) # YAML input can result in lists of several OPM objects
      lapply(data, convert_dataset)
    else
      convert_dataset(data)
  }

  create_yaml <- function(x, outfile) {
    if (is.list(x)) # would be more elegant if to_yaml() could handle that
      x <- lapply(x, as, "list")
    write(to_yaml(x, json = json), outfile)
  }
  convert_to_yaml <- function(infile, outfile) {
    create_yaml(read_file(infile), outfile)
  }

  create_csv <- function(x, outfile) {
    if (is.list(x))
      x <- do.call(rbind, lapply(x, as.data.frame))
    else
      x <- as.data.frame(x)
    do.call(write.table, c(list(x = x, file = outfile), as.list(table.args)))
  }
  convert_to_csv <- function(infile, outfile) {
    create_csv(read_file(infile), outfile)
  }

  create_plot <- function(x, outfile) {
    do.call(device, c(list(file = outfile), dev.args))
    print(do.call(plot.type, c(list(x = x), plot.args)))
    dev.off()
  }
  create_plot_from_file <- function(infile, outfile) {
    data <- read_file(infile)
    if (is.list(data))
      data <- opms(data, group = TRUE)
    create_plot(data, outfile)
  }

  convert_to_single_file <- function(names, outfile.template, out.ext, demo,
      verbose, ..., proc) {
    x <- read_opm(names = names, convert = "grp", ..., demo = demo)
    if (demo) {
      if (verbose)
        message(paste0(x, collapse = "\n"))
      return(invisible(x))
    }
    out.names <- gsub(" ", "-", names(x), FALSE, FALSE, TRUE)
    out.names <- paste(sprintf(outfile.template, out.names), out.ext, sep = ".")
    x <- mclapply(X = x, FUN = convert_dataset, mc.cores = proc)
    mcmapply(FUN = create_single_file, infile = x, outfile = out.names,
      mc.cores = proc)
    names(out.names) <- names(x)
    if (verbose)
      message(listing(out.names))
    out.names
  }

  graphics_format_map <- function() c(bitmap = "bmp", mypdf = "pdf",
    postscript = "ps", cairo_pdf = "pdf", cairo_ps = "ps")

  LL(force.aggr, force.disc, gen.iii, force.plate, device, overwrite)

  # If a metadata file name is given, read it into data frame right now to
  # avoid opening the file each time in the batch_process() loop
  if (length(md.args) && is.character(md.args$md)) {
    tmp <- md.args
    names(tmp)[names(tmp) == "md"] <- "object"
    tmp$replace <- NULL
    md.args$md <- do.call(to_metadata, tmp)
  }

  case(output <- match.arg(output),
    yaml = {
      collect <- FALSE
      io.fun <- convert_to_yaml
      create_single_file <- create_yaml
      json <- FALSE
      in.ext <- "both"
      out.ext <- "yml"
    },
    json = {
      collect <- FALSE
      io.fun <- convert_to_yaml
      create_single_file <- create_yaml
      json <- TRUE
      in.ext <- "both"
      out.ext <- "json"
    },
    csv = {
      collect <- FALSE
      io.fun <- convert_to_csv
      create_single_file <- create_csv
      json <- NULL
      in.ext <- "both"
      out.ext <- "tab"
    },
    levelplot = {
      collect <- FALSE
      io.fun <- create_plot_from_file
      create_single_file <- create_plot
      json <- disc.args <- aggr.args <- NULL
      in.ext <- "both"
      out.ext <- map_values(device, graphics_format_map())
      plot.type <- level_plot
    },
    xyplot = {
      collect <- FALSE
      io.fun <- create_plot_from_file
      create_single_file <- create_plot
      json <- disc.args <- aggr.args <- NULL
      in.ext <- "both"
      out.ext <- map_values(device, graphics_format_map())
      plot.type <- xy_plot
    },
    split = {
      collect <- TRUE
      io.fun <- split_files
      in.ext <- "csv"
      fun.args <- list(pattern = '^("Data File",|Data File)', outdir = outdir,
        demo = demo)
    },
    clean = {
      collect <- TRUE
      io.fun <- clean_filenames
      in.ext <- "both"
      fun.args <- list(demo = demo, overwrite = overwrite == "yes")
    }
  )

  if (collect) # the functions have their own 'demo' argument
    invisible(batch_collect(names = names, fun = io.fun, fun.args = fun.args,
      proc = proc, ..., demo = FALSE))
  else if (length(combine.into))
    invisible(convert_to_single_file(names = names, out.ext = out.ext, ...,
      outfile.template = combine.into, demo = demo, verbose = verbose,
      proc = proc))
  else
    batch_process(names = names, out.ext = out.ext, io.fun = io.fun,
      in.ext = in.ext, compressed = TRUE, literally = FALSE, ..., proc = proc,
      overwrite = overwrite, outdir = outdir, verbose = verbose, demo = demo)
}

