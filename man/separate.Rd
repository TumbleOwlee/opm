\name{separate}
\alias{separate}
\title{Regularly split character vectors if possible}
\arguments{
  \item{object}{Character vector to be split, or data frame
  in which character vectors (or factors) shall be
  attempted to be split, or factor.}

  \item{split}{Character vector or \code{TRUE}. \itemize{
  \item If a character vector, used as container of the
  splitting characters and converted to a vector containing
  only non-duplicated single-character strings. For
  instance, the default \code{split} argument \code{".-_"}
  yields \code{c(".", "-", "_")}.  \item If a vector of
  only empty strings or \code{TRUE}, strings with parts
  representing fixed-width fields are assumed, and
  splitting is done at whitespace-only columns. Beforehand,
  equal-length strings are created by padding with spaces
  at the right. After splitting in fixed-width mode,
  whitespace characters are trimmed from both ends of the
  resulting strings.  }}

  \item{simplify}{Logical scalar indicating whether a
  resulting matrix with one column should be simplified to
  a vector (or such a data frame to a factor).  If so, at
  least one matrix column is kept, even if
  \code{keep.const} is \code{FALSE}.}

  \item{keep.const}{Logical scalar indicating whether
  constant columns should be kept or removed.}

  \item{coerce}{Logical scalar indicating whether factors
  should be coerced to \sQuote{character} mode and then
  also be attempted to be split. The resulting columns will
  be coerced back to factors.}

  \item{name.sep}{Character scalar to be inserted in the
  constructed column names. If more than one column results
  from splitting, the names will contain (i) the original
  column name, (ii) \code{name.sep} and (iii) their index,
  thus creating unique column names (if the original ones
  were unique).}

  \item{list.wise}{Logical scalar. Ignored if \code{split}
  is \code{TRUE}.  Otherwise, \code{object} is assumed to
  contain word lists separated by \code{split}. The result
  is a logical matrix in which the columns represent these
  words and the fields indicate whether or not a word was
  present in a certain item contained in \code{object}.}

  \item{strip.white}{Logical scalar. Remove whitespace from
  the ends of each resulting character scalar after
  splitting? Has an effect on the removal of constant
  columns. Whitespace is always removed if \code{split} is
  \code{TRUE}.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Character matrix, its number of rows being equal to the
  length of \code{object}, or data frame with the same
  number of rows as \code{object} but potentially more
  columns. May be character vector of factor with character
  or factor input and \code{simplify} set to \code{TRUE}.
}
\description{
  From a given set of splitting characters select the ones
  that split a character vector in a regular way, yielding
  the same number of parts for all vector elements. Then
  apply these splitting characters to create a matrix. The
  data frame method applies this to all character vectors
  (and optionally also all factors) within a data frame.
}
\details{
  This function is useful if information coded in the
  elements of a character vector is to be converted to a
  matrix or data frame. For instance, file names created by
  a batch export conducted by a some software are usually
  more or less regularly structured and contain content at
  distinct positions. In such situations, the correct
  splitting approach can be recognised by yielding the same
  number of fields from each vector element.
}
\examples{
# Splitting by characters
x <- c("a-b-cc", "d-ff-g")
(y <- separate(x, ".")) # a split character that does not occur
stopifnot(is.matrix(y), y[, 1L] == x)
(y <- separate(x, "-")) # a split character that does occur
stopifnot(is.matrix(y), dim(y) == c(2, 3))

# Fixed-with splitting
x <- c("  abd  efgh", " ABCD EFGH ", " xyz")
(y <- separate(x, TRUE))
stopifnot(is.matrix(y), dim(y) == c(3, 2))

# Applied to factors
xx <- as.factor(x)
(yy <- separate(xx, TRUE))
stopifnot(identical(yy, as.data.frame(y)))

# List-wise splitting
x <- c("a,b", "c,b", "a,c")
(y <- separate(x, ",", list.wise = TRUE))
stopifnot(is.matrix(y), dim(y) == c(3, 3), is.logical(y))

# Data-frame method
x <- data.frame(a = 1:2, b = c("a-b-cc", "a-ff-g"))
(y <- separate(x, coerce = FALSE))
stopifnot(identical(x, y))
(y <- separate(x)) # only character/factor columns are split
stopifnot(is.data.frame(y), dim(y) == c(2, 4))
stopifnot(sapply(y, class) == c("integer", "factor", "factor", "factor"))
(y <- separate(x, keep.const = FALSE))
stopifnot(is.data.frame(y), dim(y) == c(2, 3))
stopifnot(sapply(y, class) == c("integer", "factor", "factor"))
}
\seealso{
  base::strsplit utils::read.fwf

  Other auxiliary-functions: \code{\link{opm_opt}},
  \code{\link{param_names}}
}
\keyword{character}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{separate-methods}
\alias{separate,character-method}
\alias{separate,data.frame-method}
\alias{separate,factor-method}
\usage{
  \S4method{separate}{character}(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, list.wise = FALSE,
    strip.white = list.wise) 
  \S4method{separate}{data.frame}(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, coerce = TRUE, name.sep = ".", ...) 
  \S4method{separate}{factor}(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
