
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

