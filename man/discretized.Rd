\name{discretized}
\alias{disc_settings}
\alias{discretized}
\title{Get discretized data}
\arguments{
  \item{object}{\code{\link{OPMD}} or \code{\link{OPMS}}
  object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Logical vector or matrix in the case of
  \code{discretized}, named list in the case of
  \code{disc_settings}. See the examples for details.
}
\description{
  Get the discretized kinetic data or the discretization
  settings used.
}
\note{
  See \code{\link{do_disc}} for generating discretized
  data.
}
\examples{
# 'OPM' methods
data(vaas_1)
(x <- discretized(vaas_1))[1:3] # => logical vector
stopifnot(is.logical(x), !is.matrix(x), length(x) == dim(x)[2L])
stopifnot(names(x) == colnames(aggregated(vaas_1)))
(x <- disc_settings(vaas_1)) # => named list
stopifnot(is.list(x), !is.null(names(x)))

# 'OPMS' methods
data(vaas_4)
(x <- discretized(vaas_4))[, 1:3] # => logical matrix
stopifnot(is.logical(x), is.matrix(x), ncol(x) == dim(x)[2L])
stopifnot(colnames(x) == colnames(aggregated(vaas_1)))
summary(x <- disc_settings(vaas_4)) # => list of named lists, one per plate
stopifnot(is.list(x), is.null(names(x)), length(x) == length(vaas_4))
stopifnot(duplicated(x)[-1])
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{discretized-methods}
\alias{discretized,OPMD-method}
\alias{disc_settings,OPMD-method}
\alias{discretized-methods}
\alias{discretized,OPMS-method}
\alias{disc_settings-methods}
\alias{disc_settings,OPMS-method}
\usage{
  \S4method{discretized}{OPMD}(object) 

  \S4method{disc_settings}{OPMD}(object) 

  \S4method{discretized}{OPMS}(object, ...) 

  \S4method{disc_settings}{OPMS}(object, ...) 

}
