
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


#' Get KEGG information
#'
#' Search in \acronym{KEGG} using a substrate IDs. Requires the \pkg{KEGGREST}
#' package.
#'
#' @param object Vector of substrate IDs or \code{\link{OPMX}} object.
#' @return An object of class \sQuote{kegg_compounds}, which is a named list
#'   (using the original query IDs as the names; if this fails, a warning is
#'   issued) of entries of class \sQuote{kegg_compound}. IDs that are not found
#'   yield empty \sQuote{kegg_compound} objects, as do missing IDs (\code{NA}
#'   values).
#' @export
#' @keywords internal
#' @seealso substrate_info
#'
setGeneric("get_info", function(object, ...) standardGeneric("get_info"))

setMethod("get_info", "OPMX", function(object, what = "kegg", ...) {
  structure(get_info(substrate_info(object, what), what),
    names = wells(object, ...))
}, sealed = FALSE)

setMethod("get_info", "character", function(object, what = "kegg") {
  chunks <- function(x, n) split.default(x,
    rep(seq.int(ceiling(length(x) / n)), each = n)[seq_along(x)])
  get_kegg <- function(x, prepend) {
    compound_object <- function(x) {
      pos <- match(c("EXACT_MASS", "MOL_WEIGHT"), names(x), 0L)
      for (p in pos[pos > 0L])
        x[[p]] <- as.numeric(x[[p]])
      class(x) <- c("kegg_compound", "print_easy")
      x
    }
    run_keggrest <- function(x, prepend) {
      result <- lapply(chunks(paste0(prepend, x), 10), KEGGREST::keggGet)
      result <- lapply(unlist(result, FALSE), compound_object)
      names(result) <- vapply(result, `[[`, "", "ENTRY")
      stopifnot(all(names(result) %in% x))
      structure(result[x], names = x)
    }
    structure(get_and_remember(x, "KEGG.", compound_object(list()),
      run_keggrest, FALSE, prepend), class = c("kegg_compounds", "print_easy"))
  }
  case(match.arg(what),
    kegg = get_kegg(object, "cpd:")
  )
}, sealed = FALSE)


################################################################################
