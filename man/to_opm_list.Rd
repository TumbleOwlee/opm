\name{to_opm_list}
\alias{to_opm_list}
\alias{to_opm_list.list}
\alias{try_opms}
\alias{try_opms.list}
\title{Convert to OPM list}
\usage{
  to_opm_list(object, ...)

  \method{to_opm_list}{list} (object, precomputed = TRUE,
    skip = FALSE, group = FALSE)

  try_opms(object, ...)

  \method{try_opms}{list} (object, precomputed = TRUE,
    skip = FALSE)
}
\arguments{
  \item{object}{List of objects that can be passed to
  \code{\link{opms}} (optional for \code{try_opms}).}

  \item{precomputed}{Logical scalar. See
  \code{\link{opms}}.}

  \item{skip}{Logical scalar. See \code{\link{opms}}.}

  \item{group}{Logical scalar. See \code{\link{opms}}.}
}
\value{
  \code{to_opm_list} yields a list, \code{try_opms} an
  \code{\link{OPMS}} object (if conversions was successful)
  or just the input \code{object} (if conversions was
  unsuccessful).
}
\description{
  Convert to list of \code{\link{OPM}} objects, or
  conditionally convert a list to an \code{\link{OPMS}}
  object. Used for building an \code{\link{OPMS}} object by
  \code{\link{opms}} and optionally bulding one by
  \code{\link{c}}.
}
\keyword{internal}

