html_args <- function(
    character.states = c(`negative reaction` = "-",
    `weak reaction` = "w", `positive reaction` = "+"),
    multiple.sep = "/", organisms.start = "Organisms: ",
    states.start = "Symbols: ", legend.dot = TRUE,
    legend.sep.1 = ", ", legend.sep.2 = "; ",
    table.summary = "character matrix", no.html = TRUE,
    greek.letters = TRUE, css.file = opm_opt("css.file"),
    embed.css = FALSE, ...) {
  args <- as.list(match.call())[-1L]
  defaults <- formals()[setdiff(names(formals()), c(names(args), "..."))]
  lapply(c(defaults, args), eval)
}

safe_labels <- function(x, format, enclose = TRUE, pad = FALSE,
    comment = FALSE) {
  do_pad <- function(x, pad) {
    if (pad)
      sprintf(sprintf("%%-%is", max(nchar(x))), x)
    else
      x
  }
  nexus_quote <- function(x) {
    sprintf("'%s'", gsub("'", "''", x, FALSE, FALSE, TRUE))
  }
  clean_html <- function(x) {
    x <- gsub("&(?!([A-Za-z]+|#\\d+);)", "&amp;", x, FALSE, TRUE)
    x <- gsub("<", "&lt;", x, FALSE, FALSE, TRUE)
    x <- gsub(">", "&gt;", x, FALSE, FALSE, TRUE)
    gsub("\"", "&quot;", x, FALSE, FALSE, TRUE)
  }
  clean_from <- function(x, pat) {
    pat <- sprintf("[%s]+", pat)
    x <- sub(sprintf("^%s", pat), "", x, FALSE, TRUE)
    x <- sub(sprintf("%s$", pat), "", x, FALSE, TRUE)
    gsub(pat, "_", x, FALSE, TRUE)
  }
  surround <- function(x, start, end, enclose) {
    if (enclose)
      c(start, x, end)
    else
      sprintf("%s%s%s", start, x, end)
  }
  LL(enclose, pad, comment)
  format <- match.arg(format, PHYLO_FORMATS)
  if (comment) {
    case(format,
      html = surround( # the replacement used is the one favoured by HTML Tidy
        gsub("--", "==", x, FALSE, FALSE, TRUE), "<!-- ", " -->", enclose),
      hennig = surround(chartr("'", '"', x), "'", "'", enclose),
      nexus = surround(chartr("[]", "{}", x), "[", "]", enclose),
      epf =,
      phylip = stop("comments are not defined for format ", format)
    )
  } else {
    not.newick <- "\\s,:;()" # spaces and Newick-format special characters
    not.nexus <- "\\s()\\[\\]{}/\\,;:=*'\"`+<>-" # see PAUP* manual
    # see http://tnt.insectmuseum.org/index.php/Basic_format (16/04/2012)
    not.hennig <- "\\s;/+-"
    case(format,
      html = clean_html(x),
      phylip = sprintf("%-10s", substr(clean_from(x, not.newick), 1L, 10L)),
      hennig = do_pad(clean_from(x, not.hennig), pad),
      epf = do_pad(clean_from(x, not.newick), pad),
      nexus = do_pad(if (enclose)
        nexus_quote(x)
      else
        clean_from(x, not.nexus), pad)
    )
  }
}

setGeneric("phylo_data", function(object, ...) standardGeneric("phylo_data"))

setMethod("phylo_data", "matrix", function(object,
    format = opm_opt("phylo.fmt"), outfile = "", enclose = TRUE, indent = 3L,
    paup.block = FALSE, delete = c("none", "uninf", "constant", "ambig"),
    join = FALSE, cutoff = 0, digits = opm_opt("digits"),
    comments = comment(object), html.args = html_args(),
    prefer.char = format == "html", run.tidy = FALSE, ...) {
  format <- match.arg(format, PHYLO_FORMATS)
  delete <- match.arg(delete)
  comments <- comments
  object <- new("CMAT", object)
  if (L(prefer.char))
    object <- update(object, how = "NA2int")
  object <- merge(x = object, y = join)
  if (is.list(object) && L(cutoff) > 0)
    object[] <- lapply(object, reduce_to_mode.default, cutoff, FALSE)
  switch(EXPR = delete, none = NULL,
    object <- update(object, how = sprintf("delete.%s", delete)))
  result <- format(x = object, how = format, enclose = enclose, digits = digits,
    indent = indent, paup.block = paup.block, comments = comments,
    html.args = html.args, ...)
  if (L(run.tidy) && format == "html")
    result <- tidy(result, check = FALSE)
  if (nzchar(L(outfile))) {
    write(result, outfile)
    return(invisible(result))
  }
  result
}, sealed = SEALED)

setMethod("phylo_data", "data.frame", function(object, as.labels = NULL,
    subset = "numeric", sep = " ", ...) {
  object <- extract_columns(object, as.labels = as.labels, what = subset,
    direct = FALSE, sep = sep)
  phylo_data(object, ...)
}, sealed = SEALED)

setMethod("phylo_data", "XOPMX", function(object, as.labels,
    subset = param_names("disc.name"), sep = " ", extract.args = list(),
    join = TRUE, discrete.args = list(range = TRUE, gap = TRUE), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = NULL, subset = subset,
    dups = if (is.logical(join) && L(join))
      "ignore"
    else
      "warn", dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  object <- do.call(extract, extract.args)
  if (!is.null(discrete.args) && !is.logical(object)) {
    discrete.args <- as.list(discrete.args)
    discrete.args$x <- object
    object <- do.call(discrete, discrete.args)
  }
  phylo_data(object = object, join = join, ...)
}, sealed = SEALED)

setOldClass("OPMD_Listing")

setMethod("phylo_data", "OPMD_Listing", function(object,
    html.args = html_args(), run.tidy = FALSE) {
  if (!attr(object, "html"))
    return(paste(object, collapse = " "))
  head <- sprintf("Character listing exported by %s",
    paste0(opm_string(version = TRUE), collapse = " version "))
  attr(head, opm_string()) <- TRUE
  head <- html_head(head, html.args$css.file,
    html.args[names(html.args) == "meta"], html.args$embed.css)
  x <- c(HTML_DOCTYPE, "<html>", head, "<body>", unname(object),
    "</body>", "</html>")
  if (L(run.tidy))
    x <- tidy(x, check = FALSE)
  x
}, sealed = SEALED)

setOldClass("OPMS_Listing")

setMethod("phylo_data", "OPMS_Listing", function(object,
    html.args = html_args(), run.tidy = FALSE) {
  prepare_headlines <- function(x) {
    x <- safe_labels(x, format = "html")
    x <- hmakeTag("span", data = x, class = "organism-name",
      title = "organism-name")
    hmakeTag("div", data = x, class = "headline", title = "headline")
  }
  if (!attr(object, "html"))
    return(apply(object, 1L, paste, collapse = " "))
  head <- sprintf("Character listings exported by %s",
    paste0(opm_string(version = TRUE), collapse = " version "))
  attr(head, opm_string()) <- TRUE
  head <- html_head(head, html.args$css.file,
    html.args[names(html.args) == "meta"], html.args$embed.css)
  x <- apply(object, 1L, paste, collapse = "\n")
  x <- as.vector(rbind(prepare_headlines(names(x)), x))
  x <- c(HTML_DOCTYPE, "<html>", head, "<body>", x, "</body>", "</html>")
  if (L(run.tidy))
    x <- tidy(x, check = FALSE)
  x
}, sealed = SEALED)

