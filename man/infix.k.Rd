\name{\%k\%}
\alias{\%k\%}
\alias{infix.k}
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
  whether all given keys are present in the top-level names
  of the metadata (these may be nested, but all sublists
  are ignored here). An empty query vector results in
  \code{TRUE}. Note that the values of the character
  vector, not its names, if any, are used for querying the
  metadata. Using a list as query, this method tests
  whether all given keys are present in the names of the
  metadata. This works like the character method, but
  because a query list is given, the comparison of keys can
  be applied recursively (by using, of course, a nested
  query list). This is based on \code{\link{contains}} with
  the \code{values} argument set to \code{FALSE}. The
  factor method first converts \code{x} to
  \sQuote{character} mode.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' but
# neither 'Trial' nor 'Organism' nor 'Run':
data(vaas_1)
# In the following we use stopifnot(), which fails unless all arguments
# passed are TRUE.

# Character method
stopifnot("Experiment" \%k\% vaas_1) # present
stopifnot("Species" \%k\% vaas_1) # present
stopifnot(!"Run" \%k\% vaas_1) # missing
stopifnot(c("Species", "Experiment") \%k\% vaas_1) # all present
stopifnot(!c("Species", "Trial") \%k\% vaas_1) # only one present
stopifnot(!c("Organism", "Experiment") \%k\% vaas_1) # only one present
stopifnot(character() \%k\% vaas_1) # empty query always results

# List method
stopifnot(list(Experiment = "whatever") \%k\% vaas_1) # key present
stopifnot(list(Species = "ignored") \%k\% vaas_1) # key present

# This fails because we query with a named sublist but the 'Species'
# metadata entry is not even a list.
stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) \%k\% vaas_1)

# This is OK because we query with an unnamed sublist: it has no names that
# one would fail to find.
stopifnot(list(Species = list("X", "Y")) \%k\% vaas_1)

# More non-nested query examples
stopifnot(!list(Run = 99) \%k\% vaas_1) # key not present
stopifnot(list(Species = "?", Experiment = NA) \%k\% vaas_1) # keys present
stopifnot(!list(Species = "?", Trial = NA) \%k\% vaas_1) # one key missing
stopifnot(!list(Organism = "?", Experiment = NA) \%k\% vaas_1) # likewise
stopifnot(list() \%k\% vaas_1) # empty query always results
}
\keyword{attribute}

\docType{methods}
\alias{infix.k-methods}
\alias{infix.k,character,WMD-method}
\alias{\%k\%,character,WMD-method}
\alias{infix.k,list,WMD-method}
\alias{\%k\%,list,WMD-method}
\alias{infix.k,factor,WMD-method}
\alias{\%k\%,factor,WMD-method}
\alias{infix.k,list,OPMS-method}
\alias{\%k\%,list,OPMS-method}
\alias{infix.k,character,OPMS-method}
\alias{\%k\%,character,OPMS-method}
\alias{infix.k,factor,OPMS-method}
\alias{\%k\%,factor,OPMS-method}
\usage{
  \S4method{\%k\%}{character,WMD}(x, table) 

  \S4method{\%k\%}{list,WMD}(x, table) 

  \S4method{\%k\%}{factor,WMD}(x, table) 

  \S4method{\%k\%}{list,OPMS}(x, table) 

  \S4method{\%k\%}{character,OPMS}(x, table) 

  \S4method{\%k\%}{factor,OPMS}(x, table) 

}
