\docType{class}
\name{OPM}
\alias{OPM}
\alias{OPM-class}
\alias{OPMA}
\alias{OPMA-class}
\alias{OPMD}
\alias{OPMD-class}
\alias{OPMS}
\alias{OPMS-class}
\title{Real classes of the opm package}
\description{
  Classes whose members can directly be generated and
  manipulated by an \pkg{opm} user: \acronym{OPM},
  \acronym{OPMA}, \acronym{OPMD} and \acronym{OPMS}.
}
\details{
  \acronym{OPM} is an acronym for
  \sQuote{OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  Phenotype Microarray}. This is the class for holding
  single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data without aggregated values, but
  with information read from the original input
  \acronym{CSV} files as well as an additional arbitrary
  amount of arbitrarily organised metadata. Objects of this
  class are usually created by inputting files with
  \code{\link{read_single_opm}} or \code{\link{read_opm}}.

  \acronym{OPM} inherits from \code{\link{WMD}} and, hence,
  has all its methods.

  Regarding the coercion of this class to other classes
  (see the \code{as} method from the \pkg{methods}
  package), consider the following: \itemize{ \item The
  coercion of this class (and its child classes) to a list
  (and vice versa) relies on a mapping between slot names
  and keys in the list, i.e. the list must be appropriately
  named. For instance, this is the mechanism when reading
  from and writing to \acronym{YAML}, see
  \code{\link{to_yaml}}.  \item Coercions to other data
  frames and matrices first coerce the
  \code{\link{measurements}} and then add the other slots
  as attributes.  \item Methods such as
  \code{\link{flatten}} and \code{\link{extract}} might be
  way more appropriate for converting \acronym{OPM}
  objects. }

  \acronym{OPMA} is an acronym for \sQuote{\acronym{OPM},
  aggregated}. This is the class for holding single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data together with aggregated
  values. Objects of this class are usually created by
  calling \code{\link{do_aggr}} on an \acronym{OPM} object,
  or by inputting files with \code{\link{read_single_opm}}
  or \code{\link{read_opm}} if these files already contain
  aggregated data.

  \acronym{OPMA} inherits from \acronym{OPM} and, hence,
  has all its methods.

  \acronym{OPMD} is an acronym for \sQuote{\acronym{OPM},
  discretized}. This is the class for holding single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data together with aggregated
  \strong{and} discretized values. Objects of this class
  are usually created by calling \code{\link{do_disc}} on
  an \acronym{OPMA} object, or by inputting files with
  \code{\link{read_single_opm}} or \code{\link{read_opm}}
  if these files already contain discretized data.

  \acronym{OPMD} inherits from \acronym{OPMA} and, hence,
  has all its methods.

  The discretized data are considered as
  \sQuote{consistent} with the curve parameter from which
  they have been estimated if no \code{FALSE} value
  corresponds to curve parameter larger than the curve
  parameter of any \code{TRUE} value; \code{NA} values are
  not considered when checking consistency. The
  \sQuote{strict.OPMD} entry of \code{\link{opm_opt}}
  determines whether an error or only a warning is issued
  in the case of inconsistency.

  \acronym{OPMS} is the class for holding multi-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data with or without aggregated or
  discretized values. Regarding the name: \acronym{OPMS} is
  just the plural of \acronym{OPM}. Objects of this class
  are usually created by calling \code{\link{opms}} or
  other combination functions on \acronym{OPM} or derived
  objects, or by inputting files with
  \code{\link{read_opm}} if these files altogether contain
  more than a single plate. The data may have been obtained
  from distinct organisms and/or replicates, but
  \strong{must} correspond to the same plate type and
  \strong{must} contain the same wells.

  As a rule, OPMS has the same methods as the \acronym{OPM}
  class, but adapted to a collection of more than one
  \acronym{OPM} object. Also, \acronym{OPMS} can hold
  \acronym{OPMD} and \acronym{OPMA} as well as
  \acronym{OPM} objects, even though this is not indicated
  for all its methods in this manual.
}
\examples{
# conversion of a list to an OPM object is tolerant against re-orderings
# (but not against additions and omissions)
x <- as(vaas_1, "list")
x$measurements <- c(rev(x$measurements[7:8]), rev(x$measurements[-7:-8]))
summary(x)
x <- as(x, "OPM")
summary(x)
stopifnot(identical(measurements(x), measurements(vaas_1)))

# conversion of a list to an OPMA object is tolerant against re-orderings
# and additions (but not against omissions)
x <- as(vaas_1, "list")
x$aggregated <- c(Answer = 42L, rev(x$aggregated), Text = LETTERS)
summary(x)
x <- as(x, "OPMA")
summary(x)
stopifnot(identical(aggregated(x), aggregated(vaas_1)))

# conversion of a list to an OPMD object is tolerant against re-orderings
# and additions (but not against omissions)
x <- as(vaas_1, "list")
x$discretized <- c(Answer = 42L, rev(x$discretized), Text = LETTERS)
summary(x)
x <- as(x, "OPMD")
summary(x)
stopifnot(identical(discretized(x), discretized(vaas_1)))
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{FOE}}, \code{\link{MOA}},
  \code{\link{OPMX}}, \code{\link{WMD}},
  \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

