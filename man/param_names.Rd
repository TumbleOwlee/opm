\name{param_names}
\alias{param_names}
\title{Names of curve parameters}
\usage{
  param_names(what = c("param.names", "reserved.md.names"))
}
\arguments{
  \item{what}{Character scalar. Which kind of names to
  obtain.}
}
\value{
  Character vector.
}
\description{
  Yield the names of the estimated curve parameters used
  internally and in the output. Alternatively, yield names
  that should not be used in metadata entries because they
  are used as predefined column names by functions such as
  \code{\link{flatten}}.
}
\examples{
(x <- param_names())
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
(x <- param_names("reserved"))
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
}
\seealso{
  Other aggregation-functions: \code{\link{do_aggr}},
  \code{\link{fast_estimate}}
}
\keyword{utilities}

