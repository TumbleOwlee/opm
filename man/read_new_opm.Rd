\name{read_new_opm}
\alias{read_microstation_opm}
\alias{read_new_opm}
\alias{read_old_opm}
\alias{read_opm_yaml}
\alias{repair_oth}
\title{Read OmniLog CSV or YAML file}
\usage{
  read_new_opm(filename)

  read_old_opm(filename)

  read_opm_yaml(filename)

  read_microstation_opm(filename)

  repair_oth(x)
}
\arguments{
  \item{filename}{Character scalar with the obvious
  meaning.}

  \item{x}{Numeric matrix.}
}
\value{
  \code{\link{OPM}} object or list of \code{\link{OPM}}
  objects. In the case of \acronym{YAML} or \acronym{JSON},
  a \code{\link{OPM}}, \code{\link{OPMA}} or
  \code{\link{OPMD}} object, depending on the presence of
  aggregated and discretized data. Can also be a list of
  such objects.  \code{repair_oth} returns a matrix either
  equal to \code{x} or containing a single row only.
}
\description{
  Read OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  data from a single file in one of the following formats:
  \itemize{ \item \acronym{CSV} file in new style (as of
  2011).  \item Pseudo-\acronym{CSV} file in the old style
  (pre-2011).  \item
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}.
  \item \acronym{YAML}.  \item \acronym{JSON}. }
}
\details{
  The
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
  file must be a comma-separated file with a certain number
  of defined columns. There are only end-point measurements
  present: each resulting \code{\link{OPM}} object contains
  only a single time point. It is unclear in that case
  whether \sQuote{Plate Number} can really serve as an
  identifier, as duplicates might be present.

  With \acronym{YAML} or \acronym{JSON}, arbitrary nesting
  levels are allowed, i.e. the \acronym{YAML} input results
  in either a list representing the slots of an
  \code{\link{OPM}} or \code{\link{OPMA}} object, or a list
  whose direct or indirect sublists represent such an
  object. However, it is an error if non-list elements are
  encountered when traversing such lists, and if no
  \code{\link{OPM}} or \code{\link{OPMA}} objects can be
  found at all.

  Plates run in generation-III mode, if converted to
  \acronym{CSV} by the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  software, comprise duplicated rows with only two sets of
  distinct values, the first set containing only \code{0}.
  \code{repair_oth} checks for such a matrix and, if
  identified, reduces it to its single informative row.
}
\references{
  \url{http://www.biolog.com/}

  \url{http://www.yaml.org/}

  \url{http://www.json.org/}
}
\keyword{internal}

