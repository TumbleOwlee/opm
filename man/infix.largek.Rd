\name{\%K\%}
\alias{\%K\%}
\alias{infix.largek}
\title{Search in metadata keys}
\arguments{
  \item{x}{Character vector, factor or list.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a character vector as query, this method tests
  whether a given key is present in the metadata and
  fetches an object that is not \code{NULL}. If the key has
  a length > 1, sublists are queried. An empty vector
  results in \code{TRUE}. Note that the values of the
  character vector, not its names, if any, are used for
  querying the metadata. Using a list as query, this
  function behaves like \code{\link{infix.k}}. The factor
  method first converts \code{x} to \sQuote{character}
  mode.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' but
# neither 'Trial' nor 'Organism' nor 'Run':
data(vaas_1)
# In the following we use stopifnot(), which fails unless all arguments
# passed are TRUE.

# Character method

# Single-element queries
stopifnot("Experiment" \%K\% vaas_1) # present
stopifnot("Species" \%K\% vaas_1)  # present
stopifnot(!"Run" \%K\% vaas_1) # not present
stopifnot(!"Trial" \%K\% vaas_1) # not present
stopifnot(!"Organism" \%k\% vaas_1) # not present

# Zero-element queries
stopifnot(character() \%K\% vaas_1) # always results

# Querying with vectors of length > 1 mean nested queries; compare this to
# the behavior of \%k\%!
stopifnot(!c("Species", "Experiment") \%K\% vaas_1)
# I.e. "Experiment" is not within "Species".

# List method
# See \%k\% -- the behavior is identical for lists.
}
\keyword{attribute}

\docType{methods}
\alias{infix.largek-methods}
\alias{infix.largek,character,WMD-method}
\alias{\%K\%,character,WMD-method}
\alias{infix.largek,list,WMD-method}
\alias{\%K\%,list,WMD-method}
\alias{infix.largek,factor,WMD-method}
\alias{\%K\%,factor,WMD-method}
\alias{infix.largek,list,OPMS-method}
\alias{\%K\%,list,OPMS-method}
\alias{infix.largek,character,OPMS-method}
\alias{\%K\%,character,OPMS-method}
\alias{infix.largek,factor,OPMS-method}
\alias{\%K\%,factor,OPMS-method}
\usage{
  \S4method{\%K\%}{character,WMD}(x, table) 

  \S4method{\%K\%}{list,WMD}(x, table) 

  \S4method{\%K\%}{factor,WMD}(x, table) 

  \S4method{\%K\%}{list,OPMS}(x, table) 

  \S4method{\%K\%}{character,OPMS}(x, table) 

  \S4method{\%K\%}{factor,OPMS}(x, table) 

}
