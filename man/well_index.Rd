\name{well_index}
\alias{well_index}
\title{Translate well coordinates}
\usage{
  well_index(x)
}
\arguments{
  \item{x}{Vector, formula or missing. Basically any \R
  object.}
}
\value{
  Either \code{x}, \code{TRUE} or (if a formula) the result
  of evaluating \code{x} in the context of the known well
  coordinate names.
}
\description{
  Translate well coordinates to numeric indexes
}
\keyword{internal}

