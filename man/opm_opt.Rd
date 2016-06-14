\name{opm_opt}
\alias{opm_opt}
\alias{param_names}
\title{Global \pkg{opm} options}
\arguments{
  \item{x}{Character scalar or list or missing. If not
  given, all current settings are returned (as a named
  list). If \code{x} is a character scalar, it is used for
  querying for a single value.

  If \code{x} is a list, it is expected to contain
  key-value pairs that can be set. In that case, it is an
  error if a key is unknown, i.e. it is an error to use a
  name that is not already contained (\pkg{opm} would never
  query for it anyway). It is also illegal to attempt set
  novel values whose classes are not identical to, or
  derived from, the classes of the old value. Further, it
  is illegal to set a zero-length value.}

  \item{...}{Optional arguments. If \code{x} is missing,
  these arguments are concatenated into a list and used as
  if \code{x} was given as a list (see above). That is, the
  argument names are used as the keys for setting values.
  This is usually easier than working with a list.}

  \item{what}{Character scalar indicating the parameters of
  interest. See \sQuote{Details}.}
}
\value{
  List or atomic vector. If one to several values are set,
  the previous entries are returned invisibly.
}
\description{
  Get and set global \pkg{opm} options, or get list of
  predefined parameter names of interest for the user
}
\details{
  The following keys can be used with the following kinds
  of values: \describe{ \item{colors}{Default colour set
  used by the \code{\link{OPMS}} method of
  \code{\link{xy_plot}} and other plotting functions.}
  \item{color.borders}{Character vector with default colour
  borders between which \code{\link{level_plot}}
  interpolates to obtain a colour palette.}
  \item{comb.key.join}{Used by functions that support
  combination of metadata entries converted to data-frame
  columns immediately after their selection. Sets the
  character string that is used when joining old names to
  new name. Should normally only be a single character.}
  \item{comb.value.join}{Used by functions that support
  combination of metadata entries converted to data-frame
  columns immediately after their selection. Sets the
  character string that is used when joining old values to
  new values. Should normally only be a single character;
  must be a single character when used by
  \code{\link{opm_mcp}}.} \item{contrast.type}{Character
  scalar indicating the default type of contrast used by
  \code{\link{opm_mcp}}.} \item{css.file}{Character scalar.
  Default \acronym{CSS} file linked by
  \code{\link{phylo_data}} when producing \acronym{HTML}
  output. Ignored if empty.} \item{csv.keys}{Character
  vector with names of entries of \code{\link{csv_data}} be
  used by \code{\link{include_metadata}}.  Should be kept a
  subset of \code{opm_opt("csv.selection")}.}
  \item{csv.selection}{Character vector with names of
  entries of \code{\link{csv_data}} (must be a valid
  \sQuote{keys} argument) to be extracted by
  \code{\link{collect_template}}.}
  \item{curve.param}{Character scalar. Default
  \sQuote{subset} argument of \code{\link{extract}} and the
  plotting functions.} \item{disc.param}{Character scalar.
  Default \sQuote{subset} argument of
  \code{\link{do_disc}}. It is usually not advisable to
  change it.} \item{digits}{Integer scalar. Number of
  digits used by some functions generating output text.}
  \item{file.encoding}{Character scalar. Character encoding
  in input files as assumed by \code{\link{read_opm}}.}
  \item{file.split.tmpl}{Character scalar. Template used as
  \sQuote{format} argument by \code{\link{split_files}}.}
  \item{gen.iii}{Character scalar indicating whether
  \code{\link{read_opm}} and other IO functions based on it
  automatically convert to this plate type. If empty,
  nothing is changed.} \item{group.name}{Character scalar
  used as column name for trivial groups (either all items
  in the same group or each item in a group of its own)
  created by \code{\link{extract}}.}
  \item{heatmap.colors}{Colour palette used by
  \code{\link{heat_map}}.} \item{html.attr}{Used by
  \code{\link{phylo_data}} for automatically creating
  \acronym{HTML} \sQuote{title} and \sQuote{class}
  attributes.} \item{input.try.order}{Integer vector
  indicating the preferred order when trying to read
  \acronym{CSV} files with \code{\link{read_single_opm}}.
  See there for details.} \item{key.join}{Used by
  \code{\link{metadata}} and some other functions that must
  be in sync with it for joining metadata keys used in
  nested queries (because the resulting object is
  \sQuote{flat}).} \item{machine.id}{Integer scalar that
  can be used for identifying an
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  instrument. Useful for \code{\link{collect_template}} if
  several such machines are in use.}
  \item{max.chars}{Integer scalar used when abbreviating
  full substrate names. See \code{\link{wells}} for an
  example.} \item{md.duration}{Default name of the key to
  be treated as overall running time in the metadata.
  Currently only relevant for easing the inclusion of the
  total measurement duration in the metadata.}
  \item{md.id.name}{Default name of the key to be treated
  as \acronym{ID} in the metadata. Currently only relevant
  for easing the inclusion of plate-specific \acronym{ID}s
  in the metadata.} \item{md.id.start}{The first value to
  inserted next as \acronym{ID} value in the metadata.
  Currently only relevant for easing the inclusion of
  plate-specific \acronym{ID}s in the metadata.}
  \item{min.mode}{Used when making discretisation results
  uniform within a group. The minimum proportion the most
  frequent value much reach to be used for representing all
  values (if less, frequent, \code{NA} is used). Must be a
  numeric scalar between 0 and 1.}
  \item{phylo.fmt}{Character scalar indicating the default
  output format used by \code{\link{phylo_data}}.}
  \item{split}{Character scalar indicating the default
  splitting characters used by \code{\link{separate}}.}
  \item{strict.OPMD}{Logical scalar indicating whether
  \code{\link{OPMD}} objects can only be created if the
  discretised data are consistent with the parameter from
  which they have been estimated.} \item{threshold}{Numeric
  scalar indicating the default threshold used by
  \code{\link{annotated}}.} \item{time.zone}{Character
  scalar indicating the time zone to be used when parsing
  \code{setup_time} entries. This is relevant for
  \code{\link{merge}}, which by default attempts to sort by
  parsed setup times} \item{time.fmt}{Character vector
  indicating the time formats used for parsing the
  \code{setup_time} entries (in the given order). Also
  relevant for \code{\link{merge}} by default. It is
  advisable to put the more specific formats to the front
  because otherwise information such as an \sQuote{AM} or
  \sQuote{PM} indication might be lost. A wrong format
  might well match a given entry, causing \pkg{opm} to
  misinterpret the time or even the date.}
  \item{warn.mult}{Issue a warning if
  \code{\link{read_single_opm}} reads a multiple-plate
  old-style or new-style \acronym{CSV} file.} }

  The following kinds of parameter names applied by
  \pkg{opm} cannot be modified by the user (but displayed
  using \code{param_names}): \describe{
  \item{param.names}{Names of the estimated curve
  parameters used internally and in the output.}
  \item{disc.name}{Alternative name used to select
  discretised values instead.}
  \item{reserved.md.names}{Names that should not be used in
  metadata entries because they are used as predefined
  column names by functions such as \code{\link{flatten}}.}
  \item{split.at}{The name of the column in data frames
  generated by \code{\link{extract}} that separates data
  from metadata.} \item{hours}{The name of the column that
  holds the overall running time when \code{\link{extract}}
  is used to obtain it.} }
}
\examples{
# fetching a value
(digits <- opm_opt("digits"))
stopifnot(digits == 4)

# setting a value; previous value is returned as list
(old.opts <- opm_opt(digits = 5L))
stopifnot(is.list(old.opts), length(old.opts) == 1L)
stopifnot(old.opts$digits == 4)

# fetching the value again: should now be changed
(digits <- opm_opt("digits"))
stopifnot(digits == 5)

# resetting the value
(old.opts <- opm_opt(old.opts))
stopifnot(is.list(old.opts), length(old.opts) == 1L)
stopifnot(old.opts$digits == 5)
(digits <- opm_opt("digits"))
stopifnot(digits == 4)

## reserved parameter names
(x <- param_names())
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
(x <- param_names("reserved"))
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
stopifnot(param_names("split.at") \%in\% x)
}
\seealso{
  base::options base::getOption

  Other auxiliary-functions: \code{\link{separate}}
}
\keyword{utilities}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{opm_opt-methods}
\alias{opm_opt,character-method}
\alias{opm_opt,list-method}
\alias{opm_opt,missing-method}
\alias{param_names-methods}
\alias{param_names,character-method}
\alias{param_names,missing-method}
\usage{
  \S4method{opm_opt}{character}(x) 
  \S4method{opm_opt}{list}(x) 
  \S4method{opm_opt}{missing}(x, ...) 

  \S4method{param_names}{character}(what) 
  \S4method{param_names}{missing}(what) 
}
%% END INSERTION BY repair_S4_docu.rb
