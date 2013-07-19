\name{explode_dir}
\alias{batch_collect}
\alias{batch_process}
\alias{explode_dir}
\alias{file_pattern}
\title{Batch processing of files}
\usage{
  explode_dir(names, include = NULL, exclude = NULL,
    ignore.case = TRUE, wildcard = TRUE, recursive = TRUE,
    missing.error = TRUE, remove.dups = TRUE)

  batch_collect(names, fun, fun.args = list(), proc = 1L,
    ..., use.names = TRUE, simplify = FALSE, demo = FALSE)

  batch_process(names, out.ext, io.fun, fun.args = list(),
    proc = 1L, outdir = NULL,
    overwrite = c("yes", "older", "no"), in.ext = "any",
    compressed = TRUE,
    literally = inherits(in.ext, "AsIs"), ...,
    verbose = TRUE, demo = FALSE)

  file_pattern(type = c("both", "csv", "yaml", "json", "any", "empty"),
    compressed = TRUE, literally = inherits(type, "AsIs"))
}
\arguments{
  \item{names}{Character vector containing filenames or
  directories, or convertible to such.}

  \item{include}{If a character scalar, used as regular
  expression or wildcard (see the \code{wildcard} argument)
  for selecting from the input files. If \code{NULL},
  ignored. If a list, used as arguments of
  \code{\link{file_pattern}} and its result used as regular
  expression. Note that selection is done \strong{after}
  expanding the directory names to filenames.}

  \item{exclude}{Like \code{include}, but for excluding
  matching input files.  Note that exclusion is done
  \strong{after} applying \code{include}.}

  \item{ignore.case}{Logical scalar. Ignore differences
  between uppercase and lowercase when using \code{include}
  and \code{exclude}? Has no effect for \code{NULL} values
  for \code{include} or \code{exclude}, respectively.}

  \item{wildcard}{Logical scalar. Are \code{include} and
  \code{exclude} wildcards (as used by UNIX shells) that
  first need to be concerted to regular expressions? Has no
  effect if lists are used for \code{include} or
  \code{exclude}, respectively. See
  \code{\link{glob_to_regex}} for details on such wildcards
  (a.k.a. globbing patterns).}

  \item{recursive}{Logical scalar. Traverse directories
  recursively and also consider all subdirectories? See
  \code{list.files} from the \pkg{base} package for
  details.}

  \item{missing.error}{Logical scalar. If a file/directory
  does not exist, raise an error or only a warning?}

  \item{remove.dups}{Logical scalar. Remove duplicates from
  \code{names}? Note that if requested this is done
  \strong{before} expanding the names of directories, if
  any.}

  \item{fun}{Collecting function. Should use the filename
  as first argument.}

  \item{fun.args}{Optional list of arguments to \code{fun}
  or \code{io.fun}.}

  \item{...}{Optional further arguments passed to
  \code{\link{explode_dir}}.}

  \item{proc}{Integer scalar. The number of processes to
  spawn. Cannot be set to more than 1 core if running under
  Windows. See the \code{cores} argument of
  \code{\link{do_aggr}} for details.}

  \item{simplify}{Logical scalar. Should the resulting list
  be simplified to a vector or matrix if possible?}

  \item{use.names}{Logical scalar. Should \code{names} be
  used for naming the elements of the result?}

  \item{out.ext}{Character scalar. The extension of the
  outfile names (without the dot).}

  \item{outdir}{Character vector. Directories in which to
  place the outfiles.  If \code{NULL} or only containing
  empty strings, each infile's directory is used.}

  \item{in.ext}{Character scalar. Passed through
  \code{\link{file_pattern}}, then used for the replacement
  of old file extensions with new ones.}

  \item{type}{Character scalar indicating the file types to
  be matched by extension. Alternaticely, directly the
  extension or extensions, or a list of file names (not
  \code{NA}).}

  \item{compressed}{Logical scalar. Shall compressed files
  also be matched?  This affects the returned pattern as
  well as the pattern used for extracting file extensions
  from complete file names (if \code{literally} is
  \code{TRUE}).}

  \item{literally}{Logical scalar. Interpret \code{type}
  literally? This also allows for vectors with more than a
  single element, as well as the extraction of file
  extensions from file names.}

  \item{demo}{Logical scalar. In the case of
  \code{batch_process}, if \code{TRUE} do not convert
  files, but print the attempted infile-outfile conversions
  and invisibly return a matrix with infiles in the first
  and outfiles in the second column? For the other
  functions, the effect is equivalent.}

  \item{io.fun}{Conversion function. Should accept
  \code{infile} and \code{outfile} as the first two
  arguments.}

  \item{overwrite}{Character scalar. If \sQuote{yes},
  conversion is always tried if \code{infile} exists and is
  not empty. If \sQuote{no}, conversion is not tried if
  \code{outfile} exists and is not empty. If
  \sQuote{older}, conversion is tried if \code{outfile}
  does not exist or is empty or is older than \code{infile}
  (with respect to the modification time).}

  \item{verbose}{Logical scalar. Print conversion and
  success/failure information?}
}
\value{
  \code{explode_dir} returns a character vector (which
  would be empty if all existing files, if any, had been
  unselected).

  \code{batch_collect} returns a list, potentially
  simplified to a vector, depending on the output of
  \code{fun} and the value of \code{simplify}. See also
  \code{demo}.

  In normal mode, \code{batch_process} creates an invisibly
  returned character matrix in which each row corresponds
  to a named character vector with the keys
  \sQuote{infile}, \sQuote{outfile}, \sQuote{before} and
  \sQuote{after}. The latter two describe the result of the
  action(s) before and after attempting to convert
  \code{infile} to \code{outfile}. \sQuote{after} is the
  empty string if no conversion was tried (see
  \code{overwrite}), \sQuote{ok} if conversion was
  successful and a message describing the problems
  otherwise. For the results of the \code{demo} mode see
  above.

  \code{file_pattern} yields a character scalar, holding a
  regular expression.
}
\description{
  Batch-collect information from a series of input files or
  batch-convert data from infiles to data in outfiles.
  Alternatively, turn a mixed file/directory list into a
  list of files or create a regular expression matching
  certain file extensions. These functions are not normally
  directly called by an \pkg{opm} user but by the other IO
  functions of the package such as
  \code{\link{collect_template}} or
  \code{\link{batch_opm}}. One can use their \code{demo}
  argument directly for testing the results of the applied
  filename patterns.
}
\note{
  Other functions that call \code{explode_dir} should have
  a \code{demo} argument which, if set to \code{TRUE},
  caused the respective function to do no real work but
  print the names of the files that it would process in
  normal running mode.
}
\examples{
# explode_dir()
# Example with temporary directory
td <- tempdir()
tf <- tempfile()
(x <- explode_dir(td))
write(letters, tf)
(y <- explode_dir(td))
stopifnot(length(y) > length(x))
unlink(tf)
(y <- explode_dir(td))
stopifnot(length(y) == length(x))

# Example with R installation directory
(x <- explode_dir(R.home(), include = "*/doc/html/*"))
(y <- explode_dir(R.home(), include = "*/doc/html/*", exclude = "*.html"))
stopifnot(length(x) == 0L || length(x) > length(y))

# More interesting use cases are provided by the functions that call
# explode_dir(). Consider to first try them in 'demo' mode. Globbing
# examples are given under glob_to_regex().

# batch_collect()
# Read the first line from each of the OPM test dataset files
f <- opm_files("testdata")
if (length(f) > 0) { # if the files are found
  x <- batch_collect(f, fun = readLines, fun.args = list(n = 1L))
  # yields a list with the input files as names and the result from each
  # file as values (exactly one line)
  stopifnot(is.list(x), identical(names(x), f))
  stopifnot(sapply(x, is.character), sapply(x, length) == 1L)
} else {
  warning("test files not found")
}

# batch_process()
# Read the first line from each of the OPM test dataset files and store it
# in temporary files
pf <- function(infile, outfile) write(readLines(infile, n = 1), outfile)
infiles <- opm_files("testdata")
if (length(infiles) > 0) { # if the files are found
  x <- batch_process(infiles, out.ext = "tmp", io.fun = pf,
    outdir = tempdir())
  stopifnot(is.matrix(x), identical(x[, 1], infiles))
  stopifnot(file.exists(x[, 2]))
  unlink(x[, 2])
} else {
  warning("test files not found")
}

# file_pattern()
(x <- file_pattern())
(y <- file_pattern(type = "csv", compressed = FALSE))
stopifnot(nchar(x) > nchar(y))
# constructing pattern from existing files
(files <- list.files(pattern = "[.]"))
(x <- file_pattern(I(files))) # I() causes 'literally' to be TRUE
stopifnot(grepl(x, files, ignore.case = TRUE))
}
\seealso{
  base::list.files base::Sys.glo

  Other io-functions: \code{\link{batch_opm}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}}, \code{\link{opm_files}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}
\keyword{character}

