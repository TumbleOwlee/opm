\name{opm_files}
\alias{opm_files}
\alias{select_colors}
\title{Files and colour sets used by the package}
\usage{
  opm_files(what = c("scripts", "testdata", "auxiliary", "demo", "doc", "css", "sql",
    "omnilog", "single", "multiple", "growth"))

  select_colors(set = c("w3c", "w3c.i", "w3c.r", "nora", "nora.i", "nora.r", "brewer",
    "brewer.i", "brewer.r", "roseo", "roseo.i", "roseo.r", "ssnot", "ssnot.i", "ssnot.r",
    "phrogz", "phrogz.i", "phrogz.r", "groups", "groups.i", "groups.r", "jp01", "jp01.i",
    "jp01.r", "jp02", "jp02.i", "jp02.r", "jp03", "jp03.i", "jp03.r", "jp04", "jp04.i",
    "jp04.r", "jp05", "jp05.i", "jp05.r", "jp06", "jp06.i", "jp06.r", "jp07", "jp07.i",
    "jp07.r", "jp08", "jp08.i", "jp08.r", "jp09", "jp09.i", "jp09.r", "jp10", "jp10.i",
    "jp10.r", "jp11", 
        "jp11.i", "jp11.r", "jp12", "jp12.i", "jp12.r", "jp13", "jp13.i", "jp13.r",
    "jp14", "jp14.i", "jp14.r", "jp15", "jp15.i", "jp15.r", "jp16", "jp16.i", "jp16.r",
    "jp17", "jp17.i", "jp17.r", "jp18", "jp18.i", "jp18.r", "jp19", "jp19.i", "jp19.r",
    "jp20", "jp20.i", "jp20.r", "jp21", "jp21.i", "jp21.r", "jp22", "jp22.i", "jp22.r",
    "jp23", "jp23.i", "jp23.r", "jp24", "jp24.i", "jp24.r", "jp25", "jp25.i", "jp25.r",
    "jp26", "jp26.i", "jp26.r", "jp27", "jp27.i", "jp27.r", "jp28", "jp28.i", "jp28.r",
    "jp29", 
        "jp29.i", "jp29.r", "jp30", "jp30.i", "jp30.r", "jp31", "jp31.i", "jp31.r",
    "jp32", "jp32.i", "jp32.r", "jp33", "jp33.i", "jp33.r", "jp34", "jp34.i", "jp34.r",
    "jp35", "jp35.i", "jp35.r", "jp36", "jp36.i", "jp36.r", "jp37", "jp37.i", "jp37.r",
    "jp38", "jp38.i", "jp38.r", "jp39", "jp39.i", "jp39.r", "jp40", "jp40.i", "jp40.r"))
    }
\arguments{
  \item{what}{Character scalar indicating the subdirectory
  to search in or the kind of names to obtain. Currently
  the following subdirectories are included: \describe{
  \item{auxiliary}{Miscellaneous files which are neither
  executable scripts nor test data.} \item{css}{Predefined
  \acronym{CSS} files for \acronym{HTML} files generated
  by, e.g., \code{\link{phylo_data}}.} \item{doc}{The
  vignette (documentation) in several formats, including
  the extracted \R code.} \item{demo}{Example \R code using
  the \pkg{opm} package that neither fitted into these help
  pages nor into the vignette. Can directly be loaded via
  \code{demo}; see \code{demo(package = "opm")}.}
  \item{growth}{Growth-measurement example files.}
  \item{multiple}{Not directly readable (i.e.,
  multiple-plate) test files.} \item{omnilog}{Directly
  readable (i.e., single-plate) test files from
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  runs.} \item{scripts}{\R script files for non-interactive
  uses of the \pkg{opm} package, particularly for the batch
  processing of many files.  When called without input
  arguments or with the \sQuote{-h} switch, the scripts
  output usage information.} \item{single}{Directly
  readable (i.e., single-plate) test files.}
  \item{sql}{\acronym{SQL} files for working with
  relational databases.} \item{testdata}{Files as output by
  the devices such as the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  instrument.  Included here as examples for data input
  (and metadata management).} }}

  \item{set}{Character scalar. Name of the colour vector to
  use. Colour vectors have been optimised for maximum
  contrast between adjacent colours, either manually or
  using \code{max_rgb_contrast} from the \pkg{pkgutils}
  package.  Names ending in \sQuote{.i} indicate vectors in
  inverse order (compared to the vector with the same name
  except \sQuote{.i}). Names ending in \sQuote{.i} indicate
  vectors in random order. For credits regarding the
  vectors see the code.

  \code{set} can also be a numeric scalar, which is then
  used to select the set of colours thought to optimally
  yield contrasting colours for this number of items. Up to
  40 items are supported.}
}
\value{
  Character vector of file names or reserved parameter
  names or names of colours.
}
\description{
  Get list of files from the \pkg{opm} package of interest
  for the user or select from some predefined colour sets
  for plotting.
}
\details{
  In addition to the results of \sQuote{reserved.md.names},
  it should be avoided to use metadata keys that start with
  a dot, as such keys might also be created intermediary by
  methods that have to compile metadata together with other
  information.

  Note that \code{pkg_files} might fail with very unusual
  installations of the \pkg{opm} package.

  See \code{\link{xy_plot}} for a usage example of
  \code{select_colors}. This function is not normally
  directly called by an \pkg{opm} user but could be used
  for testing before doing some serious plotting.
}
\examples{
## example input files
isRfile <- function(x) grepl("\\\.R$", x, ignore.case = TRUE)
(x <- opm_files("auxiliary"))
stopifnot(!isRfile(x))
(x <- opm_files("demo"))
stopifnot(isRfile(x))
(x <- opm_files("scripts"))
stopifnot(isRfile(x))
(x <- opm_files("testdata"))
stopifnot(!isRfile(x))
for (name in c("growth", "single", "multiple", "omnilog")) {
  print(y <- opm_files(name))
  stopifnot(y \%in\% x) # i.e., a subset of the set of all input example files
}

# On UNIX systems you should be able to do this if Rscript and the optparse
# package are properly installed:
# invisible(sapply(paste("Rscript", opm_files()), system))
# ...and get the usage messages of all scripts.

# Check the names of the example files: they should be ok
(x <- pkgutils::clean_filenames(opm_files("testdata"), demo = TRUE))
stopifnot(length(x) == 0)

## colours
(x <- select_colors("nora"))
(y <- select_colors("nora.i")) # same in reverse order
stopifnot(is.character(x), length(x) > 0L, identical(x, rev(y)))
}
\references{
  \url{http://www.colorbrewer.org}
}
\seealso{
  pkgutils::pkg_files utils::demo grDevices::colors
  grDevices::rainbow grDevices::grey

  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}}, \code{\link{gen_iii}},
  \code{\link{listing}}, \code{\link{plate_type}},
  \code{\link{register_plate}},
  \code{\link{substrate_info}}, \code{\link{wells}}
}
\keyword{color}
\keyword{utilities}

