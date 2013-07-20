\name{aggregated}
\alias{aggr_settings}
\alias{aggregated}
\title{Get aggregated data}
\arguments{
  \item{object}{\code{\link{OPMA}} or \code{\link{OPMS}}
  object.}

  \item{subset}{Character vector. If not \code{NULL},
  restrict to this or these parameter(s). See
  \code{\link{param_names}} for the possible values.}

  \item{ci}{Logical scalar. Include the estimates of
  confidence intervals (CIs) in the output?}

  \item{trim}{Character scalar. Parameter estimates from
  intrinsically negative reactions (i.e., no respiration)
  are sometimes biologically unreasonable because they are
  too large or too small, and some corrections might be
  appropriate. \describe{ \item{no}{No modification.}
  \item{full}{Negative lamdda estimates are set to zero.}
  \item{medium}{Lambda estimates larger than
  \code{\link{hours}(object)} (i.e., the maximum time value
  observed) are set to that value. Negative lambda
  estimates smaller than \code{-hours(object)} are set to
  this value (i.e., the negative maximum time).}
  \item{full}{Like \sQuote{medium}, but all negative values
  are set to zero, which is a less moderate treatment.} }
  Currently the other parameters are not checked, and all
  \code{NA} values, if any, also remain unchanged.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  \code{aggregated} yields a numeric matrix of aggregated
  values (a.k.a. the curve parameters). If bootstrapping
  was used, their CIs are included. The columns represent
  the wells, the rows the estimated parameters and their
  CIs.

  \code{aggr_settings} returns a named list. See the
  examples for details.
}
\description{
  Get the aggregated kinetic data or the aggregation
  settings used.
}
\note{
  See \code{\link{do_aggr}} for generating aggregated data.
}
\examples{
# 'OPMA' methods
data(vaas_1)
# Get full matrix
(x <- aggregated(vaas_1))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(12L, 96L)))
# Subsetting
(x <- aggregated(vaas_1, "lambda"))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), any(x < 0))
# Now with lambda correction
(x <- aggregated(vaas_1, "lambda", trim = "full"))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), !any(x < 0))
# settings
(x <- aggr_settings(vaas_1)) # yields named list
stopifnot(is.list(x), !is.null(names(x)))

# 'OPMS' methods
data(vaas_4)
summary(x <- aggregated(vaas_4)) # => one matrix per OPM object
stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.matrix))
# settings
summary(x <- aggr_settings(vaas_4)) # list of named lists, one per plate
stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.list))
}
\seealso{
  Other getter-functions: \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{subset}}, \code{\link{well}}
}
\keyword{attribute}

\docType{methods}
\alias{aggregated-methods}
\alias{aggregated,OPMA-method}
\alias{aggr_settings,OPMA-method}
\alias{aggregated-methods}
\alias{aggregated,OPMS-method}
\alias{aggr_settings-methods}
\alias{aggr_settings,OPMS-method}
\usage{
  \S4method{aggregated}{OPMA}(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium")) 

  \S4method{aggr_settings}{OPMA}(object) 

  \S4method{aggregated}{OPMS}(object, ...) 

  \S4method{aggr_settings}{OPMS}(object, ...) 

}
