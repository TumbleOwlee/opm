\name{metadata}
\alias{metadata}
\alias{metadata_chars}
\title{Get metadata}
\arguments{
  \item{object}{\code{\link{WMD}}, \code{\link{WMDS}} or
  \code{\link{MOPMX}} object.}

  \item{key}{\code{NULL}, vector, factor or formula.
  \itemize{ \item If \code{NULL} or otherwise empty, return
  all metadata.  \item If a non-empty list, treated as list
  of keys. Return value would be the list of corresponding
  metadata values. Here, character vectors of length > 1
  can be used to query nested metadata lists.  \item If
  neither empty nor a list nor a formula (i.e. usually a
  character or numeric vector), \code{key} is treated as a
  single list key. Factors are converted to
  \sQuote{character} mode.  \item Formulae can also be used
  and are converted to a list or character or numeric
  vector using the rules described under \sQuote{Details}.
  \item It is in general not recommended to use numeric
  vectors as \code{key} arguments, either directly or
  within a list or formula. }}

  \item{exact}{Logical scalar. Use exact or partial
  matching of keys? Has no effect if \code{key} is empty.}

  \item{strict}{Logical scalar. Is it an error if a
  \code{NULL} value results from fetching a metadata key?}

  \item{values}{Logical scalar. If \code{FALSE}, metadata
  names, not values, are collected, and \code{classes} is
  ignored (names are always of class \sQuote{character} and
  need not be coerced).}

  \item{classes}{Character vector containing the names of
  classes that should also be collected (and coerced to
  \sQuote{character}), or \code{TRUE}. In that case, a
  mapping template for the classes themselves is returned.
  See the \code{coerce} argument of \code{map_values} for
  details.}

  \item{max.dist}{Numeric scalar. If non-negative, causes
  the construction of a mapping from potential misspellings
  to the putative correct spelling, based on string
  similarity. \code{max.dist} then gives the maximum string
  distance allowed for regarding two strings as synonyms.
  This boundary must not be set too high; otherwise strings
  with distinct meanings might be regarded as misspellings
  (see the example below). The resulting vector should
  always be inspected before passing it to
  \code{\link{map_values}}.  See the \code{map_values}
  method for character vectors as \code{object} and numeric
  vectors as \code{mapping} argument in the \pkg{pkgutils}
  package for further details on such string matching.}

  \item{...}{Optional arguments passed between the methods
  or to \code{\link{map_values}}.}
}
\value{
  \code{metadata} generates a list (empty if metadata were
  not set or if partial selection using \code{key} did not
  result).

  Under default settings \code{metadata_chars} yields a
  character vector, sorted and made unique. Original
  \code{names} attributes, if any, are dropped and replaced
  by the character vector itself. (This might be convenient
  regarding its use with \code{\link{map_metadata}}.) If
  \code{max.distance} is non-negative, the result is
  distinct; see above for details.
}
\description{
  Get meta-information stored together with the data or
  collect all \sQuote{character} entries from the
  meta-information stored together with the measurements.
  Optionally coerce data of other types.
}
\details{
  If a named list is used as \code{key} argument, its names
  will be used within the first level of the resulting
  nested or non-nested list.  That is, \code{key} can be
  used to translate names on the fly, and this can be used
  by all functions that call \code{metadata} indirectly,
  usually via an \code{as.labels} or \code{as.groups}
  argument.

  Even though it is not technically impossible, it is
  usually a bad idea to select metadata entries using
  numeric (positional) or logical keys. The problem is
  that, in contrast to, e.g., data frames, their is no
  guarantee that metadata entries with the same name occur
  in the same position, even if they belong to
  \code{\link{WMD}} objects within a single
  \code{\link{WMDS}} object.

  Note that \code{key = c("a", "b")} would search for an
  element named \code{b} \emph{within} the element named
  \code{a}. To extract two elements at the same (highest)
  level, \code{key = list("a", "b")} should be used.  This
  prevents many \sQuote{subscript out of bounds} errors.

  Formulae passed as \code{key} argument are treated by
  ignoring the left side (if any) and converting the right
  side to a list or other vector. Code enclosed in \code{I}
  is evaluated with a call to \code{eval}. It is up to the
  user to ensure that this call succeeds and yields a
  character vector or a list. Operators in all other code
  within the formula are used just as separators, and all
  names are converted to character scalars. The \code{$}
  operator binds tightly, i.e. it separates elements of a
  character vector (for nested querying) in the output. The
  same effect have other operators of high precedence such
  as \code{::} but their use is not recommended. All
  operators with a lower precedence than \code{$} separate
  list elements.

  Additional options when using formulae are described
  under \code{\link{extract}}.

  The result of \code{metadata_chars} can be used to create
  a mapping for \code{\link{map_metadata}}. The
  \code{\link{WMDS}} method just applies the
  \code{\link{WMD}} method to all contained plates in turn.
}
\examples{
# 'WMD' methods

(x <- metadata(vaas_1, "Strain"))
stopifnot(x == "DSM30083T")
(y <- metadata(vaas_1, ~ Strain)) # using a formula => same result
stopifnot(identical(x, y))

(x <- metadata_chars(vaas_1, values = FALSE))
stopifnot(names(x) == x) # mapping metadata keys to themselves
(x <- metadata_chars(vaas_1, values = TRUE))
stopifnot(names(x) == x) # mapping metadata values to themselves
# See map_metadata() for a potential usage of the metadata_chars() result

# 'WMDS' methods

(x <- metadata(vaas_4, "Strain"))
stopifnot(x == c("DSM18039", "DSM30083T", "DSM1707", "429SC1"))
(y <- metadata(vaas_4, ~ Strain)) # using a formula => same result
stopifnot(identical(x, y))

(x <- metadata_chars(vaas_4, values = TRUE)) # the values
(y <- metadata_chars(vaas_4, values = FALSE)) # the keys
stopifnot(length(x) > length(y))

# detecting misspellings
(x <- metadata_chars(vaas_4, max.dist = 0.1))
stopifnot(length(x) == 0) # no misspellings
(x <- metadata_chars(vaas_4, max.dist = 0.5)) # wrong result!
# distance too high => non-synonyms thought to be misspellings
stopifnot(length(x) == 2, !is.null(names(x)))
(x <- metadata_chars(vaas_4, max.dist = 0.5, exclude = "\\\d"))
stopifnot(length(x) == 0) # strings with numbers excluded
}
\seealso{
  Other metadata-functions: \code{\link{edit}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}}, \code{\link{map_values}},
  \code{\link{metadata.set}},                                         }
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{metadata-methods}
\alias{metadata,MOPMX-method}
\alias{metadata,WMD-method}
\alias{metadata,WMDS-method}
\alias{metadata_chars-methods}
\alias{metadata_chars,MOPMX-method}
\alias{metadata_chars,WMD-method}
\alias{metadata_chars,WMDS-method}
\usage{
  \S4method{metadata}{MOPMX}(object, ...) 
  \S4method{metadata}{WMD}(object, key = NULL, exact = TRUE, strict = FALSE)
  \S4method{metadata}{WMDS}(object, ...) 

  \S4method{metadata_chars}{MOPMX}(object, values = TRUE,
    classes = "factor", max.dist = -1, ...) 
  \S4method{metadata_chars}{WMD}(object, values = TRUE,
    classes = "factor", max.dist = -1, ...) 
  \S4method{metadata_chars}{WMDS}(object, values = TRUE,
    classes = "factor", max.dist = -1, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
