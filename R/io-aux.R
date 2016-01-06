read_new_opm <- function(filename) {
  prepare_opm <- function(allx, pos, col, filename) {
    x <- allx[pos, , drop = FALSE]
    cn <- x[1L, ]
    comments <- c(filename, x[2L, col])
    names(comments) <- c(CSV_NAMES[["FILE"]], cn[col])
    x <- x[-1L, -col, drop = FALSE]
    storage.mode(x) <- "double"
    colnames(x) <- cn[-col]
    if (comments[CSV_NAMES[["PLATE_TYPE"]]] == "OTH") {
      comments[CSV_NAMES[["PLATE_TYPE"]]] <- SPECIAL_PLATES[["gen.iii"]]
      x <- repair_oth(x)
    }
    new("OPM", measurements = x, metadata = list(), csv_data = comments)
  }
  x <- as.matrix(read.table(file = filename, colClasses = "character",
    sep = ",", fileEncoding = opm_opt("file.encoding")))
  colnames(x) <- rownames(x) <- NULL
  if (!length(col <- which(x[1L, ] == HOUR)))
    stop("uninterpretable header, probably not a new-style OmniLog CSV file")
  pos <- x[, col <- max(col)] == HOUR
  col <- seq_len(col - 1L)
  if (length(which(pos)) < 2L)
    return(prepare_opm(x, TRUE, col, filename))
  if (opm_opt("warn.mult"))
    warning("trying to read multiple-plate new-style CSV, ",
      "result (if any) is a list")
  pos <- split.default(seq_len(nrow(x)), sections(pos, NA))
  mapply(FUN = prepare_opm, pos = pos, USE.NAMES = FALSE, SIMPLIFY = FALSE,
    MoreArgs = list(filename = filename, col = col, allx = x))
}

read_lims_opm <- function(filename) {
  possible_wells <- function(n = 12L) {
    sprintf("%s%02i", rep(LETTERS, each = n), seq.int(n))
  }
  to_csv_data <- function(x) {
    x <- strsplit(x, "=", TRUE)
    x <- do.call(rbind, x[vapply(x, length, 0L) == 2L])
    structure(x[, 2L], names = x[, 1L])
  }
  to_measurements <- function(x) {
    x <- do.call(rbind, strsplit(chartr('"', " ", x), ",", TRUE))
    storage.mode(x) <- "double"
    colnames(x) <- c(HOUR, possible_wells())[seq.int(ncol(x))]
    if (anyNA(colnames(x)))
      stop("measurement data contain too many columns")
    # TODO: correct time points
    x
  }
  con <- file(description = filename, encoding = opm_opt("file.encoding"))
  on.exit(close(con))
  if (!all(nzchar(x <- readLines(con = con, warn = FALSE))))
    x <- x[nzchar(x)]
  if (length(x <- split.default(x, sections(x == "#"))) != 3L)
    stop("input lines not in three #-separated sections")
  x[[1L]] <- to_csv_data(x[[1L]][-1L])
  pos <- match(c("plate_type", "plate_position", "time_start"), names(x[[1L]]))
  if (anyNA(pos))
    stop("missing plate type, position or setup time")
  x[[3L]] <- c(filename, x[[1L]][pos])
  names(x[[3L]]) <- CSV_NAMES
  x[[1L]] <- lapply(x[[1L]][-pos], type.convert, "NA", TRUE)
  x[[2L]] <- to_measurements(x[[2L]][-1L])
  x[[2L]][, 1L] <- (x[[2L]][, 1L] - 1) * get("read_interval", x[[1L]]) / 60
  new("OPM", measurements = x[[2L]], csv_data = x[[3L]], metadata = x[[1L]])
}

read_old_opm <- function(filename) {

  prepare_comments <- function(x, filename) {
    n <- sub("\\s+$", "", vapply(x, `[[`, "", 1L), FALSE, TRUE)
    n <- n[ok <- nzchar(n)]
    n[n == "Set up Time"] <- CSV_NAMES[["SETUP"]]
    x <- vapply(lapply(x[ok], `[`, -1L), paste0, "", collapse = ",")
    if (any(bad <- grepl("^\\s", x, FALSE, TRUE)))
      x[bad] <- sub("^\\s+", "", x[bad], FALSE, TRUE)
    if (any(bad <- grepl(",$", x, FALSE, TRUE)))
      x[bad] <- sub(",+$", "", x[bad], FALSE, TRUE)
    structure(c(filename, x), names = c(CSV_NAMES[["FILE"]], n))
  }

  prepare_opm <- function(x, pos) {
    pos <- seq_len(match(TRUE, pos, 0L) - 1L)
    comments <- prepare_comments(x[pos], filename)
    x <- grep("\\S", unlist(x[-pos]), FALSE, TRUE, TRUE)
    ncol <- 97L
    if (length(x) %% ncol != 0L)
      stop("data part with wrong number of fields, not organised in 97 columns")
    x <- matrix(as.numeric(x[-seq(ncol)]), length(x) %/% ncol - 1L, ncol,
        TRUE, list(NULL, sub("^\\s+", "", x[seq(ncol)], FALSE, TRUE)))
    # Repair OTH (this affects both data and comments)
    if (comments[CSV_NAMES[["PLATE_TYPE"]]] == "OTH") {
      comments[CSV_NAMES[["PLATE_TYPE"]]] <- SPECIAL_PLATES[["gen.iii"]]
      x <- repair_oth(x)
    }
    new("OPM", measurements = x, metadata = list(), csv_data = comments)
  }

  con <- file(filename, "", TRUE, opm_opt("file.encoding"))
  on.exit(close(con))
  x <- readLines(con, -1L, TRUE, FALSE)
  x <- strsplit(x[nzchar(x)], ",", TRUE) # fixed-string splitting most efficient

  # determine position of first field of data header, then split lines into
  # comments and data fields accordingly; if necessary split into plates
  pos <- sub("^\\s+", "", vapply(x, `[[`, "", 1L), FALSE, TRUE) == HOUR
  if (!any(pos))
    stop("uninterpretable header, probably not an old-style OmniLog CSV file")
  if (length(which(pos)) < 2L)
    return(prepare_opm(x, pos))
  if (opm_opt("warn.mult"))
    warning("trying to read multiple-plate old-style CSV, ",
      "result (if any) is a list")
  if (all(duplicated.default(n <- vapply(x, length, 0L))[-1L]))
    stop("constant number of fields -- ",
      "multiple-plate old-style format saved from Excel?")
  n <- Ckmeans.1d.dp(n, 2L)$cluster
  n <- sections(n == n[[1L]], NA) # split into long and short sections
  mapply(FUN = prepare_opm, x = split.default(x, n),
    pos = split.default(pos, n), SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

read_opm_yaml <- function(filename) {
  result <- yaml.load_file(filename)
  result <- to_opm_list(result, precomputed = FALSE, skip = FALSE)
  if (!length(result))
    stop("YAML file contained no data interpretable by opm")
  result
}

read_microstation_opm <- function(filename) {
  x <- read.table(filename, sep = ",", comment.char = "", header = TRUE,
    check.names = FALSE, stringsAsFactors = FALSE, quote = "",
    fileEncoding = opm_opt("file.encoding"), strip.white = TRUE)
  names(x)[!nzchar(names(x))] <- "N.N."
  pat <- ": Dual Wavelength O\\.D\\.$"
  wells <- grep(pat, names(x), FALSE, TRUE, TRUE)
  if (length(wells) != 96L)
    stop("expected 96 column names ending in ': Dual Wavelength O.D.'")
  wanted <- c("Plate Type", "Created", "Plate Number", "Incubation Time", wells)
  y <- x[, setdiff(names(x), wanted), drop = FALSE]
  x <- x[, wanted, drop = FALSE]
  x <- cbind(filename, x, stringsAsFactors = FALSE)
  names(x) <- c(CSV_NAMES[c("FILE", "PLATE_TYPE", "SETUP", "POS")], HOUR,
    clean_coords(sub(pat, "", wells, FALSE, TRUE)))
  pos <- seq_len(4L)
  x <- to_opm_list.list(lapply(seq_len(nrow(x)), function(i) list(
    csv_data = as.list(x[i, pos, drop = FALSE]),
    measurements = as.list(x[i, -pos, drop = FALSE]),
    metadata = y[i, , drop = TRUE])), FALSE, FALSE, FALSE)
  if (!length(x))
    stop("MicroStation CSV file contained no interpretable data")
  x
}

repair_oth <- function(x) {
  if (!is.matrix(x))
    stop("expected matrix object 'x'")
  result <- unique(x[, -1L, drop = FALSE], MARGIN = 1L)
  if ((nr <- nrow(result)) > 2L || !all(result[1L, ] == 0))
    return(x)
  result <- if (nr == 2L)
    cbind(0.25, result[2L, , drop = FALSE])
  else
    cbind(0, result) # in the unexpected case that ONLY 0s are encountered
  colnames(result)[1L] <- colnames(x)[1L]
  result
}

process_io <- function(files, io.fun, fun.args = list(),
    overwrite = c("no", "older", "yes"), verbose = TRUE) {
  empty <- function(file.status) {
    is.na(file.status$size) || file.status$size == 0
  }
  create_parent <- function(filename) {
    outdir <- dirname(filename)
    file.exists(outdir) || dir.create(outdir, recursive = TRUE,
      showWarnings = FALSE)
  }
  prepare_conversion <- function(infile, outfile, overwrite) {
    istat <- file.info(c(infile, outfile)) # fails if not character
    ostat <- istat[2L, ]
    istat <- istat[1L, ]
    if (empty(istat))
      "infile unknown or empty"
    else
      case(overwrite,
        yes = if (unlink(outfile) == 0L)
          ""
        else
          "could not delete outfile",
        no = if (empty(ostat))
          ""
        else
          "outfile not empty",
        older = if (!empty(ostat) && istat$mtime < ostat$mtime)
          "outfile not empty and newer"
        else if (unlink(outfile) == 0L)
          ""
        else
          "could not delete outfile"
      )
  }
  conduct_conversion <- function(infile, outfile, fun, fun.args) {
    if (!create_parent(outfile))
      return("could not create parent directory")
    problem <- tryCatch({
      do.call(fun, c(infile = infile, outfile = outfile, fun.args))
      ""
    }, error = conditionMessage)
    if (nzchar(problem))
      problem
    else if (empty(file.info(outfile)))
      "outfile not created or empty"
    else
      "ok"
  }
  LL(files, .wanted = 2L)
  overwrite <- match.arg(overwrite)
  result <- list(infile = files[1L], outfile = files[2L], before = "",
    after = "")
  result$before <- prepare_conversion(files[1L], files[2L], overwrite)
  if (!nzchar(result$before)) {
    result$before <- "attempt to create outfile"
    result$after <- conduct_conversion(files[1L], files[2L], io.fun, fun.args)
  }
  if (verbose) {
    lapply(formatDL(unlist(result), style = "list"), message)
    message("")
  }
  unlist(result)
}

finish_template <- function(object, outfile, sep, previous, md.args, demo) {
  if (demo) {
    if (length(previous))
      message(sprintf("\n<= '%s'", previous))
    if (length(outfile) && nzchar(outfile[1L]))
      message(sprintf("\n=> '%s'", outfile))
    return(invisible(object))
  }
  if (length(previous))
    tryCatch(suppressWarnings(
        previous <- do.call(to_metadata, c(list(object = previous), md.args))
      ), error = function(e) {
        if (identical(outfile, previous))
          previous <<- NULL
        else
          stop(conditionMessage(e))
      })
  if (length(previous))
    object <- merge.data.frame(previous, object, all = TRUE)
  object <- unique.data.frame(object)
  if (length(outfile) && nzchar(outfile[1L])) {
    write.table(object, file = outfile, sep = sep, row.names = FALSE)
    invisible(object)
  } else {
    object
  }
}

