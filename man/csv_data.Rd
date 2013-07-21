\name{csv_data}
\alias{csv_data}
\alias{filename}
\alias{position}
\alias{setup_time}
\title{Information from input CSV file}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{keys}{Character vector (or other objects usable as
  vector index). An optional sub-selection. If empty (the
  default), all \acronym{CSV} data are returned. By default
  it is an error to select non-existing items. Ignored
  unless \code{what} is \sQuote{select}.}

  \item{strict}{Logical scalar indicating whether or not it
  is an error if \code{keys} are not found. Ignored unless
  \code{what} is \sQuote{select}.}

  \item{what}{Character scalar specifying a subset of the
  data. If \sQuote{select}, use \code{keys} and
  \code{strict}. Otherwise a shortcut for one of the more
  important \acronym{CSV} entries.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Named character vector (unnamed character scalar in the
  case of \code{filename}, \code{setup_time} and
  \code{filename} and if \code{what} is not
  \sQuote{select}).
}
\description{
  Information about the plate as originally read from the
  input \acronym{CSV} file (see \code{\link{read_opm}} and
  \code{\link{read_single_opm}} for reading such files).
}
\details{
  \code{filename}, \code{setup_time} and \code{position}
  are \strong{deprecated} convenience functions for some of
  the more important entries of \code{csv_data}.
}
\examples{
# 'OPM' method
data(vaas_1)

(x <- csv_data(vaas_1, "Setup Time")) # compare this to 'what = "setup_time"'
stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))

(x <- csv_data(vaas_1, what = "filename")) # one file name (of course)
stopifnot(is.character(x), length(x) == 1L)

(x <- csv_data(vaas_1, what = "position")) # single position (of course)
stopifnot(identical(x, " 7-B"))

(x <- csv_data(vaas_1, what = "setup_time")) # single setup time (of course)
# WARNING: It is unlikely that all OmniLog output has this setup time format
(parsed <- strptime(x, format = "\%m/\%d/\%Y \%I:\%M:\%S \%p"))
stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 1)

# 'OPMS' method
data(vaas_4)

(x <- csv_data(vaas_4, "Setup Time")) # one setup time per plate
stopifnot(is.character(x), length(x) == 4)

(x <- csv_data(vaas_4, what = "filename"))  # one file name per plate
stopifnot(is.character(x), length(x) == 4L)

(x <- csv_data(vaas_4, what = "position")) # one position per plate
stopifnot(is.character(x), length(x) == length(vaas_4))

(x <- csv_data(vaas_4, what = "setup_time")) # one setup time per plate
(parsed <- strptime(x, format = "\%m/\%d/\%Y \%I:\%M:\%S \%p"))
stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 4)
}
\seealso{
  base::strptime

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{dim}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{minmax}},
  \code{\link{seq}}, \code{\link{subset}},
  \code{\link{thin_out}}, \code{\link{well}}
}
\keyword{attribute}

\docType{methods}
\alias{csv_data-methods}
\alias{csv_data,OPM-method}
\alias{filename,OPM-method}
\alias{setup_time-methods}
\alias{setup_time,OPM-method}
\alias{position-methods}
\alias{position,OPM-method}
\alias{csv_data-methods}
\alias{csv_data,OPMS-method}
\alias{filename-methods}
\alias{filename,OPMS-method}
\alias{position-methods}
\alias{position,OPMS-method}
\alias{setup_time-methods}
\alias{setup_time,OPMS-method}
\usage{
  \S4method{csv_data}{OPM}(object, keys = character(),
    strict = TRUE, what = c("select", "filename", "setup_time", "position")) 

  \S4method{filename}{OPM}(object) 

  \S4method{setup_time}{OPM}(object) 

  \S4method{position}{OPM}(object) 

  \S4method{csv_data}{OPMS}(object, ...) 

  \S4method{filename}{OPMS}(object, ...) 

  \S4method{position}{OPMS}(object, ...) 

  \S4method{setup_time}{OPMS}(object, ...) 

}
