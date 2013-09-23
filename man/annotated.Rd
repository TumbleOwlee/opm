\name{annotated}
\alias{annotated}
\title{Create vector or matrix with substrate annotation}
\arguments{
  \item{object}{An object of the classes \code{opm_glht} as
  created by \code{\link{opm_mcp}}, \code{\link{OPMA}},
  \code{\link{OPMD}} or \code{\link{OPMS}}.}

  \item{what}{Character scalar indicating the kind of
  annotation to use. Passed as eponymous argument to
  \code{\link{substrate_info}}.}

  \item{how}{Character scalar. Indicating how the
  annotation is inserted.  Currently \sQuote{ids} and
  \sQuote{values} are supported. See below for details.}

  \item{output}{For the \code{\link{OPMA}} and
  \code{\link{OPMS}} methods, the estimated parameter of
  interest (see \code{\link{param_names}}). For the
  \code{opm_glht} method, either a numeric scalar or one of
  the following character scalars: \describe{
  \item{numeric}{Return the coefficients.}
  \item{different}{Return -1, 1 or 0 indicating whether the
  coefficients are significantly smaller or larger than, or
  insignificantly different from the cutoff given by
  \code{opm_opt("threshold")}. This is calculated from the
  confidence intervals stored in \code{object}.}
  \item{smaller}{Return 1 or 0 indicating whether or not
  the coefficients are significantly smaller than the
  default cutoff.} \item{larger}{Return 1 or 0 indicating
  whether or not the coefficients are significantly larger
  than the default cutoff.} \item{equal}{Return 1 or 0
  indicating whether or not the coefficients are
  insignificantly different from the default cutoff.} }
  Alternatively, character scalars such as \sQuote{!75.0},
  \sQuote{<100}, \sQuote{>150} or \sQuote{=85.0} can be
  provided, with the first character translated to the
  corresponding meaning in the list above and the remaining
  string coerced to the cutoff to be used. If a numeric
  scalar is provided, it is used as cutoff in conjunction
  with the \sQuote{different} mode described above.}

  \item{lmap}{Vector to be used for mapping the created
  logical values, if any.  See \code{\link{map_values}} and
  the examples below for details. If \code{NULL}, ignored.
  Also ignored if numeric instead of logical values are
  created.}

  \item{sep}{For the \code{opm_glht} method, the single
  character that has been used as eponymous argument in the
  call to \code{\link{opm_mcp}}.  Necessary to
  unambiguously match substrate names within contrast
  names.  For the \code{\link{OPMS}} method, a numeric
  scalar working like the \code{cutoff} argument of
  \code{\link{listing}}. Has only an effect if discretized
  values are chosen (and are available).}

  \item{conc}{Logical scalar indicating whether
  concentration information should be added to the output,
  either as a further matrix column or as attribute,
  depending on \code{how}. Note that concentration
  information might be partially or entirely \code{NA},
  depending on the wells in use.}
}
\value{
  For \code{how = "ids"}, a numeric or logical vector whose
  names are the IDs of the respective substrates in the
  database as chosen by \code{what}.

  For \code{how = "values"}, a numeric matrix containing
  the chosen computed values as first column together with
  data obtained via web service associated with the chosen
  database, in analogy to the \code{download} argument of
  \code{\link{substrate_info}} but after conversion to a
  numeric matrix. This option is not available for all
  values of \code{what} and requires additional libraries.
  See \code{\link{substrate_info}} for details.

  The first column name of the matrix is like the
  \sQuote{value} entry returned by
  \code{\link{param_names}} in \sQuote{reserved.md.names}
  mode.  The second one is \sQuote{Concentration} if
  \code{conc} is \code{TRUE}.  Depending on the subsequent
  analysis, it might be necessary to convert the matrix to
  a data frame, to convert the column names to syntactical
  names (see \code{make.names} from the \pkg{base}
  package), or to remove all rows and columns with missing
  values.
}
\description{
  These methods create vectors or matrices that include
  numeric values (selected parameter estimates or
  \code{\link{opm_mcp}} results) as well as an annotation
  of the according substrates.
}
\details{
  All methods use \code{\link{substrate_info}} for
  translating substrate names to IDs. The methods differ
  only in the way numeric and logical values are generated.

  The \code{\link{OPMA}} methods simply choses a certain
  parameter. The \code{\link{OPMD}} method can also return
  discretized values and optionally translates them using
  \code{lmap}.

  The \code{\link{OPMS}} method returns the averages of the
  selected parameter estimates over all contained plates.
  It is an error to select discretized values instead if
  they are not available for all plates.  If otherwise, the
  discretized values are aggregated as indicated by the
  \code{sep} argument.

  The \code{opm_glht} method makes only sense if each
  coefficient estimated by \code{\link{opm_mcp}} can be
  linked to a single substrate. This is usually
  \strong{only} possible for the \sQuote{Dunnett} and
  \sQuote{Pairs} type of contrast if applied to the wells.
  Typical applications are the comparison of a single
  control well to a series of other wells and the
  comparison of all or a subset of the wells between two
  metadata-defined groups. See \code{\link{opm_mcp}} for
  details.

  Because the current implementation of the \code{opm_glht}
  method attempts to identify the substrates within the
  names of the estimated coefficients (differences of
  means), some care must be taken when translating well
  coordinates to substrate names in the call to
  \code{\link{opm_mcp}}.  Substrate IDs cannot be
  identified if they are abbreviated, i.e. a low value of
  the \code{max} argument passed to \code{\link{wells}} is
  used, and not accompanied by the well coordinates, i.e.
  if the \code{in.parens} argument is set to \code{FALSE}.

  In the case of the \sQuote{Pairs} type of contrasts, some
  problems can be avoided by setting the
  \sQuote{comb.value.join} entry of \code{\link{opm_opt}}
  to another value. The same value must be used in the
  calls to \code{\link{opm_mcp}} and \code{annotated},
  however.
}
\examples{
## OPMD and OPMS methods

# default settings
head(x <- annotated(vaas_1))
stopifnot(is.numeric(x), x > 0, !is.null(names(x)))
head(y <- annotated(vaas_4)) # this averages per well over all plates
stopifnot(is.numeric(x), y > 0, identical(names(y), names(x)))

# AUC instead of maximum height
head(y <- annotated(vaas_1, output = param_names()[4]))
stopifnot(y > x, identical(names(y), names(x)))

# generation of logical vectors
head(y <- annotated(vaas_4, output = param_names("disc.name")))
stopifnot(is.logical(y), identical(names(y), names(x)))

# mapping of logical vectors: FALSE => 1, NA => 2, TRUE => 3
head(y <- annotated(vaas_4, output = param_names("disc.name"), lmap = 1:3))
stopifnot(is.numeric(y), y > 0, identical(names(y), names(x)))

## opm_glht method
## TODO examples needed
}
\seealso{
  Other multcomp-functions: \code{\link{opm_mcp}}
}
\keyword{htest}

\docType{methods}
\alias{annotated-methods}
\alias{annotated,OPMA-method}
\alias{annotated,OPMD-method}
\alias{annotated,OPMS-method}
\alias{annotated,opm_glht-method}
\usage{
  \S4method{annotated}{OPMA}(object, what = "kegg", how = "ids",
    output = opm_opt("curve.param"), lmap = NULL, sep = NULL, conc = FALSE) 

  \S4method{annotated}{OPMD}(object, what = "kegg", how = "ids",
    output = opm_opt("curve.param"), lmap = NULL, sep = NULL, conc = FALSE) 

  \S4method{annotated}{OPMS}(object, what = "kegg", how = "ids",
    output = opm_opt("curve.param"), lmap = NULL, sep = opm_opt("min.mode"),
    conc = FALSE) 

  \S4method{annotated}{opm_glht}(object, what = "kegg", how = "ids",
    output = "numeric", lmap = NULL, sep = opm_opt("comb.value.join"),
    conc = FALSE) 

}
