\name{summary}
\alias{summary}
\title{Summarize OPM or OPMS objects}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{...}{Optional arguments passed to \code{formatDL}.}
}
\value{
  For the \code{\link{OPM}} method, a named list of the
  class \sQuote{OPM_Summary}, returned invisibly. The
  \sQuote{metadata} entry is the number of non-list
  elements in \code{\link{metadata}}. For the
  \code{\link{OPMS}} method, a list of such lists (one per
  plate), also returned invisibly, with the class set to
  \sQuote{OPMS_Summary} and some information on the entire
  object in the attribute \sQuote{overall}.
}
\description{
  Generate a summary (which also prints nicely to the
  screen), or display an \code{\link{OPM}} or
  \code{\link{OPMS}} object on screen.
}
\details{
  Currently the \code{show} methods are just wrappers for
  the \code{summary} methods for these objects with an
  additional call to \code{print}. The \acronym{CMAT}
  method is only for internal use.
}
\examples{
# OPM method
data(vaas_1)
(x <- summary(vaas_1))
stopifnot(is.list(x), is.object(x))
vaas_1 # calls show()

# OPMS method
data(vaas_4)
(x <- summary(vaas_4))
stopifnot(is.list(x), length(x) == 4L, all(sapply(x, is.list)),
  is.object(x))
vaas_4 # calls show()
}
\seealso{
  base::summary base::formatDL methods::show base::print

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{radial_plot}}, \code{\link{xy_plot}}
}
\keyword{attribute}

\docType{methods}
\alias{summary-methods}
\alias{summary,OPM-method}
\alias{summary,OPMS-method}
\alias{show,OPMX-method}
\alias{show-methods}
\alias{show,CMAT-method}
\usage{
  \S4method{summary}{OPM}(object, ...) 

  \S4method{summary}{OPMS}(object, ...) 

  \S4method{show}{OPMX}(object) 

  \S4method{show}{CMAT}(object) 

}
