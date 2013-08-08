
################################################################################


#' Funktion for annotating a vector for metabolic maps
#'
#' This function provides annotated vectors to be used for further analysis with
#' kegg_map()
#'
#' @param object output of \code{\link{opm_mcp}}, usually an object of class
#'   \code{glht}.
#'
#' @param subset Character scalar passed to \code{\link{extract}}.
#'
#' @param what Character scalar. Defines the ID type used for annotation of the
#'   resulting vector.
#'
#' @return A numeric vector with names attributes containing the KEGG compound
#'   IDs or substrate names.
#'
#' @keywords internal
#'
annotation_vector <- function(object, subset = opm_opt("curve.param"),
                      what = "kegg", output = c("numeric", "logical"),
                      cutoff = NULL) {

  get_match <- function(s, m, i) {
    start <- attr(m, "capture.start")[, i]
    substring(s, start, start + attr(m, "capture.length")[, i] - 1L)
  }
  coef_names_to_ids <- function(x, what, plate) {
    p <- c("[A-Z]\\d{2}\\s+\\((.+)\\)", "[A-Z]\\d{2}\\s+\\[(.+)\\]",
            "([A-Z]\\d{2})", "(.+)")
    p <- paste0("^", p, "\\s+-\\s+", p, "?")
    s <- NULL
    for (i in seq_along(p))
      if (all(attr(m <- regexpr(p[i], x, FALSE, TRUE), "match.length") > 0L)) {
        s <- cbind(get_match(x, m, 1L), get_match(x, m, 2L))
        if (i == 3L)
          s[] <- wells(as.vector(s), TRUE, FALSE, plate = plate)
        break
      }
    if (is.null(s) || !ncol(s <- s[, !is_constant(s, 2L), drop = FALSE]))
      stop("cannot infer substrate names from 'ghlt' estimate names")
    substrate_info(s[, 1L], "kegg")
  }
  coef_to_ids <- function(x, what, plate) structure(x,
                  names = coef_names_to_ids(names(x), what, plate))
  if (inherits(object, "glht")) {
    # cutoff
    # ist die relevanzschwelle
    # cutoff = NULL
    # if (numeric(cutoff)){}
    # significanzen berechnen, wenn cutoff angegeben ist.
    # confint(x) benutzen
    case(match.arg(output),
      numeric = coef_to_ids(coef(object), what, attr(object, "opm")$plate.type),
      logical = if (numeric(cutoff)) {
          print("weiter in zeile 60")
        }
        else {stop("no cutoff-value defined")}
    )
  }
  else if (is(object, "OPMA")) {
    a <- list()
    a[[1]] <- structure(aggregated(object,
      match.arg(subset, param_names()), FALSE)[1L, ],
      names = substrate_info(object, what))
    return(a)
  }
  else if (is(object, "OPMS")) {
    a <- list()
    for (i in 1: length(object)) {
    a[[i]] <- structure(aggregated(object[i], match.arg(subset, param_names()),
        FALSE)[1L, ], names = substrate_info(object[i], what))
    }
        return(a)
  }
  # duchschnitt
  else {
    stop("'x' must be an object of class 'glht', 'OPMA' or 'OPMS'")
  }

}

################################################################################


## TODO: must assess whether this can be integrated in substrate_info()


#' Conduct a web query
#'
#' Search via a web service for substrate information, given the IDs.
#'
#' @param ids Vector of substrate IDs.
#' @param what Character scalar indicating the web service to use.
#' @return Dedicated kind of S3 object, depending on \code{what}.
#' @export
#' @keywords internal
#' @seealso substrate_info
#' @details \acronym{KEGG} queries need the \pkg{KEGGREST} package.
#'
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
      rep(seq.int(ceiling(length(x) / n)), each = n)[seq_along(x)])
    run_keggrest <- function(x, prepend) {
      result <- lapply(chunks(paste0(prepend, x), 10), KEGGREST::keggGet)
      result <- lapply(unlist(result, FALSE), compound_object)
      names(result) <- vapply(result, `[[`, "", "ENTRY")
      found <- match(names(result), x, 0L)
      if (!all(found > 0L))
        stop("KEGG request yielded entries that do not match the query")
      structure(result[found], names = x)
    }
    prepend <- paste0(match.arg(prepend, c("cpd", "drug")), ":")
    got <- get_and_remember(x = x, prefix = "KEGG.", getfun = run_keggrest,
      default = compound_object(list()), prepend = prepend)
    structure(got, names = names(x), class = c("kegg_compounds", "print_easy"))
  }
  case(match.arg(what),
    kegg = get_kegg(ids, "cpd"),
    drug = get_kegg(ids, "drug")
  )
}


################################################################################


#' Collect information from KEGG objects
#'
#' @param x Object of class \sQuote{kegg_compounds}.
#' @param what Character vector indicating which information to include.
#'   Multiple values are possible; the default is to collect everything.
#' @param min.cov Numeric scalar indicating the minimum coverage. See
#'   \code{collect} from the \pkg{pkgutils} package.
#' @param missing.na Logical scalar indicating whether missing compounds should
#'   be coded as \code{NA} (instead of zero).
#' @return Numeric matrix.
#' @name collect
#' @keywords internal
#'
NULL

#' @rdname collect
#' @method collect kegg_compounds
#'
collect.kegg_compounds <- function(x,
    what = c("pathway", "brite", "activity", "exact_mass"), min.cov = 2L,
    missing.na = TRUE, ...) {
  partial_matrix <- function(name, x, min.cov) {
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
      collect(result, "occurrences", min.cov)
  }
  what <- toupper(match.arg(what, several.ok = TRUE))
  result <- do.call(cbind, lapply(what, partial_matrix, x, min.cov))
  if (L(missing.na))
    result[!vapply(x, length, 0L), ] <- as(NA, typeof(result))
  result
}


################################################################################



