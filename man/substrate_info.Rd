\name{substrate_info}
\alias{substrate_info}
\title{Provide information on substrates}
\arguments{
  \item{object}{Query character vector, factor or list, S3
  object of class \sQuote{substrate_match},
  \code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{what}{Character scalar indicating which kind of
  information to output.  \describe{ \item{all}{Create
  object of S3 class \sQuote{substrate_data} containing all
  available information and useful for display.}
  \item{cas}{\acronym{CAS} registry number, optionally
  expanded to an \acronym{URL}.}
  \item{chebi}{\acronym{ChEBI} database ID, optionally
  expanded to an \acronym{URL}.}
  \item{concentration}{Attempt to extract concentration
  information (as used in \pkg{opm} substrate names) from
  \code{object}. Return \code{NA} wherever this fails.}
  \item{downcase}{Substrate name converted to lower case,
  protecting one-letter specifiers, acronyms and chemical
  symbols, and translating relevant characters from the
  Greek alphabet.} \item{drug}{\acronym{KEGG} drug database
  ID, optionally expanded to an \acronym{URL}.}
  \item{greek}{Substrate name after translation of relevant
  characters to Greek letters.} \item{html}{Like
  \kbd{greek}, but using \acronym{HTML} tags, and also
  converting other parts of compound names that require
  special formatting.} \item{kegg}{\acronym{KEGG} compound
  database ID, optionally expanded to an \acronym{URL}.}
  \item{mesh}{\acronym{MeSH} database name (useful for
  conducting \acronym{PubMed} searches), optionally
  expanded to an \acronym{URL}.}
  \item{metacyc}{\acronym{MetaCyc} database ID, optionally
  expanded to an \acronym{URL}.} \item{peptide}{List of
  character vectors representing amino acids in
  three-letter code, in order, contained in the substrate
  if it is a peptide. Empty character vectors are returned
  for non-peptide substrates.  Amino acids without
  \sQuote{L} or \sQuote{D} annotation are assumed to be in
  \sQuote{L} conformation, i.e. \sQuote{L-} is removed from
  the beginning of the amino acid codes.}
  \item{peptide2}{Like \code{peptide}, but without removal
  of \sQuote{L-} from the beginning of the amino acid
  codes.} \item{seed}{\acronym{SEED} compound database ID,
  optionally expanded to an \acronym{URL}.} } See the
  references for information on the databases.}

  \item{browse}{Numeric scalar. If non-zero, an
  \acronym{URL} is generated from each \acronym{ID}. If
  positive, this number of \acronym{URL}s (counted from the
  beginning) is also opened in the default web browser; if
  negative, the \acronym{URL}s are only returned. It is an
  error to try this with those values of \code{what} that
  do not yield an \acronym{ID}.}

  \item{download}{Logical scalar indicating whether, using
  the available IDs, substrate information should be
  queried from the according web services and returned in
  customised objects. Note that this is unavailable for
  most values of \code{what}. At the moment only \kbd{kegg}
  and \kbd{drug} can be queried for if the \pkg{KEGGREST}
  package is available. This would yield S3 objects of the
  class \code{kegg_compounds}.}

  \item{...}{Optional other arguments passed between the
  methods.}
}
\value{
  The character method returns a character vector with
  \code{object} used as names and either a matched entry or
  \code{NA} as value. Only if \code{what} is set to
  \sQuote{peptide} a named list is returned instead.  The
  factor method works like the character method, whereas
  the list method traverses a list and calls
  \code{substrate_info} on suitable elements, leaving
  others unchanged. The \code{\link{OPM}} and
  \code{\link{OPMS}} methods work like the character
  method, using their own substrates.

  Depending on the \code{browse} argument, the returned
  \acronym{ID}s might have been converted to
  \acronym{URL}s, and as a side effect tabs in the default
  web browser might have been opened. For suitable values
  of \code{what}, setting \code{download} to \code{TRUE}
  yielded special objects as described above.

  The \code{\link{MOPMX}} method yield a list with one
  element of one of the kinds described above per element
  of \code{object}.
}
\description{
  Return information on substrates such as their
  \acronym{CAS} number or other database ID or convert
  substrate names.
}
\details{
  The query names must be written exactly as used in the
  stored plate annotations. To determine their spelling,
  use \code{\link{find_substrate}}.  Each spelling might
  include a concentration indicator, but the same
  underlying substrate name yielded the same \acronym{ID}
  irrespective of the concentration.

  Note that the information is only partially complete,
  depending on the well and the database. While it is
  possible to link almost all substrates to, say,
  \acronym{CAS} numbers, they are not necessarily contained
  in the other databases. Thanks to the work of the
  \acronym{ChEBI} staff, which is gratefully acknowledged,
  \acronym{ChEBI} information is complete as far as
  possible (large molecules such as proteins or other
  polymers are not covered by \acronym{ChEBI}).

  For some wells, even a main substrate cannot be
  identified, causing all its \acronym{ID}s to be missing.
  This holds for all control wells, for all wells that
  contain a mixture of (usually two) substrates, and for
  all wells that are only specified by a certain pH.

  The generated \acronym{URL}s should provide plenty of
  information on the respective substrate. In the case of
  \acronym{ChEBI}, \acronym{KEGG} and \acronym{MetaCyc},
  much information is directly displayed on the page
  itself, whereas the chosen \acronym{CAS} site contains a
  number of links providing additional chemical details.
  The \acronym{MeSH} web pages directly link to according
  \acronym{PubMed} searches.
}
\examples{
# Character method; compare correct and misspelled substrate name
(x <- substrate_info(c("D-Glucose", "D-Gloucose")))
stopifnot(anyNA(x), !all(is.na(x)))
stopifnot(identical(x, # Factor method yields same result
  substrate_info(as.factor(c("D-Glucose", "D-Gloucose")))))

# Now with generation of URLs
(y <- substrate_info(c("D-Glucose", "D-Gloucose"), browse = -1))
stopifnot(is.na(y) | nchar(y) > nchar(x))
# NA remains NA (and the function would not try to open it in the browser)

# Character method, safe conversion to lower case
(x <- substrate_info(c("a-D-Glucose", "a-D-Gloucose"), "downcase"))
stopifnot(nchar(x) > nchar(c("a-D-Glucose", "a-D-Gloucose")))
# note the protection of 'D' and the conversion of 'a'
# whether or not substrate names are known does not matter here

# Peptide extraction (note treatment of non-standard amino acids)
(x <- substrate_info(c("Ala-b-Ala-D-Glu", "Glucose", "Trp-Val"), "peptide"))
stopifnot(is.list(x), sapply(x, length) == c(3, 0, 2))

# List method
(x <- substrate_info(find_substrate(c("D-Glucose", "D-Gloucose"))))
stopifnot(length(x[[1]]) > length(x[[2]]))

# OPM and OPMS methods
(x <- substrate_info(vaas_1[, 1:3], "all"))
stopifnot(inherits(x, "substrate_data"))
stopifnot(identical(x, substrate_info(vaas_4[, , 1:3], "all")))
\dontrun{

  # this would open up to 96 tabs in your browser...
  substrate_info(vaas_4, "kegg", browse = 100)
}
}
\references{
  Bochner, B. R., pers. comm.

  \url{http://www.cas.org/content/chemical-substances/faqs}

  \url{http://www.genome.jp/kegg/}

  Kanehisa, M., Goto, S., Furumichi, M., Tanabe, M., and
  Hirakawa, M. 2010 KEGG for representation and analysis of
  molecular networks involving diseases and drugs.
  \emph{Nucleic Acids Research} \strong{38}: D355--D360.

  \url{http://metacyc.org/}

  Caspi, R., Altman, T., Dreher, K., Fulcher, C.A.,
  Subhraveti, P., Keseler, I.M., Kothari, A., Krummenacker,
  M., Latendresse, M., Mueller, L.A., Ong, Q., Paley, S.,
  Pujar, A., Shearer, A.G., Travers, M., Weerasinghe, D.,
  Zhang, P., Karp, P.D. 2012 The MetaCyc database of
  metabolic pathways and enzymes and the BioCyc collection
  of pathway/genome databases. \emph{Nucleic Acids
  Research} \strong{40}: D742--D753.

  \url{http://www.ncbi.nlm.nih.gov/mesh}

  Coletti, M.H., Bleich, H.L 2001 Medical subject headings
  used to search the biomedical literature. \emph{Journal
  of the American Medical Informatics Association}
  \strong{8}: 317--323.

  \url{http://www.ebi.ac.uk/chebi/}

  Hastings, J., de Matos, P., Dekker, A., Ennis, M.,
  Harsha, B., Kale, N., Muthukrishnan, V., Owen, G.,
  Turner, S., Williams, M., Steinbeck, C. 2013 The ChEBI
  reference database and ontology for biologically relevant
  chemistry: enhancements for 2013. \emph{Nucleic Acids
  Research} \strong{41}: D456--D463.

  Overbeek, R., Begley, T., Butler, R., Choudhuri, J.,
  Chuang, H., Cohoon, M., de Crecy-Lagard, V., Diaz, N.,
  Disz, T., Edwards, R., Fonstein, M., Frank, E., Gerdes,
  S., Glass, E., Goesmann, A., Hanson, A., Iwata-Reuyl, D.,
  Jensen, R., Jamshidi, N., Krause, L., Kubal, M., Larsen,
  N., Linke, B., McHardy, A., Meyer, F., Neuweger, H.,
  Olsen, G., Olson, R., Osterman, A., Portnoy, V., Pusch,
  G., Rodionov, D., Rueckert, C., Steiner, J., Stevens, R.,
  Thiele, I., Vassieva, O., Ye, Y., Zagnitko, O., Vonstein,
  V. 2005 The subsystems approach to genome annotation and
  its use in the project to annotate 1000 genomes.
  \emph{Nucleic Acids Research} \strong{33}: 5691--5702.
}
\seealso{
  utils::browseURL

  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}}, \code{\link{gen_iii}},
  \code{\link{listing}}, \code{\link{opm_files}},
  \code{\link{plate_type}}, \code{\link{register_plate}},
  \code{\link{select_colors}}, \code{\link{wells}}
}
\keyword{utilities}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{substrate_info-methods}
\alias{substrate_info,MOPMX-method}
\alias{substrate_info,OPM-method}
\alias{substrate_info,OPMS-method}
\alias{substrate_info,character-method}
\alias{substrate_info,factor-method}
\alias{substrate_info,list-method}
\alias{substrate_info,substrate_match-method}
\usage{
  \S4method{substrate_info}{MOPMX}(object, ...) 
  \S4method{substrate_info}{OPM}(object, ...) 
  \S4method{substrate_info}{OPMS}(object, ...) 
  \S4method{substrate_info}{character}(object,
    what = c("cas", "kegg", "drug", "metacyc", "chebi", "mesh", "seed",
      "downcase", "greek", "concentration", "html", "peptide", "peptide2",
      "all"), browse = 0L, download = FALSE, ...)
  \S4method{substrate_info}{factor}(object, ...) 
  \S4method{substrate_info}{list}(object, ...) 
  \S4method{substrate_info}{substrate_match}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
