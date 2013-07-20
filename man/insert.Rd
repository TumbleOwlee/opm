\name{insert}
\alias{insert}
\alias{insert.list}
\title{Insert a list into a list}
\usage{
  insert(object, ...)

  \method{insert}{list} (object, other, ...,
    .force = FALSE, .strict = FALSE)
}
\arguments{
  \item{object}{List.}

  \item{other}{List or other \R object to insert. Can also
  be missing.}

  \item{...}{Optional other items to insert.}

  \item{.force}{Logical scalar. Overwite items that are
  already there?}

  \item{.strict}{Logical scalar. If \code{TRUE}, has
  precedence over \code{.force} and causes some
  restrictions: Only names that are already present are
  allowed, and the classes must match the classes of the
  already contained values.}
}
\value{
  List, potentially modified.
}
\description{
  Insert all values from another list in a list, either by
  overwriting the previously present data or by only
  setting the missing ones. Note that this comparison is
  based on the names. It does not matter whether the values
  are \code{NULL}.
}
\seealso{
  utils::modifyList
}
\keyword{internal}

