\name{has_aggr}
\alias{anyNA}
\alias{has_aggr}
\alias{has_disc}
\title{Are aggregated or discretised data present?}
\arguments{
  \item{object}{\code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{x}{\code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{recursive}{Logical scalar. When \code{FALSE}, the
  aggregated and discretized values, if available, are
  checked for \code{NA} values (except for the confidence
  intervals); \code{\link{OPM}} objects yield \code{FALSE}
  in that case. When \code{TRUE}, the metadata are checked
  recursively for \code{NA} values.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Logical vector, one element per plate, or logical scalar
  (in the case of \code{anyNA}).
}
\description{
  Check whether aggregated or discretised data are present.
  (See \code{\link{do_aggr}} and \code{\link{do_disc}} for
  generating such data.) This always returns \code{FALSE}
  for the \code{\link{OPM}} class, but not necessarily for
  its child classes. Alternatively, check for \code{NA}
  values in the metadata or the estimated parameters.
}
\examples{
stopifnot(has_aggr(vaas_1), has_disc(vaas_1)) # OPM methods
stopifnot(has_aggr(vaas_4), has_disc(vaas_4)) # OPMS methods
stopifnot(!anyNA(vaas_1), !anyNA(vaas_4))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{contains}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{hours}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{minmax}},
  \code{\link{seq}}, \code{\link{subset}},
  \code{\link{thin_out}}, \code{\link{well}}
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{anyNA-methods}
\alias{anyNA,MOPMX-method}
\alias{anyNA,OPM-method}
\alias{anyNA,OPMA-method}
\alias{anyNA,OPMD-method}
\alias{anyNA,OPMS-method}
\alias{has_aggr-methods}
\alias{has_aggr,MOPMX-method}
\alias{has_aggr,OPM-method}
\alias{has_aggr,OPMS-method}
\alias{has_disc-methods}
\alias{has_disc,MOPMX-method}
\alias{has_disc,OPM-method}
\alias{has_disc,OPMS-method}
\usage{
  \S4method{anyNA}{MOPMX}(x, recursive = TRUE) 
  \S4method{anyNA}{OPM}(x, recursive = TRUE) 
  \S4method{anyNA}{OPMA}(x, recursive = TRUE) 
  \S4method{anyNA}{OPMD}(x, recursive = TRUE) 
  \S4method{anyNA}{OPMS}(x, recursive = TRUE) 

  \S4method{has_aggr}{MOPMX}(object, ...) 
  \S4method{has_aggr}{OPM}(object) 
  \S4method{has_aggr}{OPMS}(object, ...) 

  \S4method{has_disc}{MOPMX}(object, ...) 
  \S4method{has_disc}{OPM}(object) 
  \S4method{has_disc}{OPMS}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
