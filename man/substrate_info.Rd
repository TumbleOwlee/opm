\name{substrate_info}
\alias{substrate_info}
\title{Provide information on substrates}
\arguments{
  \item{object}{Query character vector, factor or list, S3
  object of class \sQuote{substrate_match},
  \code{\link{OPM}} or \code{\link{OPMS}} object.}

  \item{what}{Character scalar indicating which kind of
  information to output.  \describe{ \item{all}{Create
  object of S3 class \sQuote{substrate_data} containing all
  available information and useful for display.}
  \item{cas}{\acronym{CAS} registry number.}
  \item{chebi}{\acronym{ChEBI} database ID.}
  \item{downcase}{Substrate name converted to lower case,
  protecting one-letter specifiers, acronyms and chemical
  symbols, and translating relevant characters from the
  Greek alphabet.} \item{drug}{\acronym{KEGG} drug database
  ID.} \item{greek}{Substrate name after translation of
  relevant characters to Greek letters.} \item{html}{Like
  \sQuote{greek}, but use \acronym{HTML} tags.}
  \item{kegg}{\acronym{KEGG} compound database ID.}
  \item{mesh}{\acronym{MeSH} database ID.}
  \item{metacyc}{\acronym{MetaCyc} database ID.} } See the
  references for information on the databases.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  The character method returns a character vector with
  \code{object} used as names and either a matched entry or
  \code{NA} as value. The factor method does the same,
  whereas the list method traverses a list and calls
  \code{substrate_info} on its elements. The
  \code{\link{OPM}} and \code{\link{OPMS}} methods work
  like the character method, using their own substrates.
}
\description{
  Return information on substrates such as their
  \acronym{CAS} number or other database ID or convert
  substrate names.
}
\details{
  The query names must be written exactly as used in the
  stored plate annotations. To determine their spelling,
  use \code{\link{find_substrate}}.

  Currently the information is incomplete, particularly for
  the PM-M plates.  While it should eventually be possible
  to link all substrates to \acronym{CAS} numbers, they are
  not necessarily contained in the other databases.
}
\examples{
# Character method; compare correct and misspelled substrate name
(x <- substrate_info(c("D-Glucose", "D-Gloucose")))

# Factor method
(y <- substrate_info(as.factor(c("D-Glucose", "D-Gloucose"))))
stopifnot(identical(x, y))

# Character method, safe conversion to lower case
(x <- substrate_info(c("a-D-Glucose", "a-D-Gloucose"), "downcase"))
stopifnot(nchar(x) > nchar(c("a-D-Glucose", "a-D-Gloucose")))
# note the protection of 'D' and the conversion of 'a'
# whether ot not substrate names are known does not matter here

# List method
(x <- substrate_info(find_substrate(c("D-Glucose", "D-Gloucose"))))
stopifnot(length(x[[1]]) > length(x[[2]]))

# OPM and OPMS methods
data(vaas_1)
(x <- substrate_info(vaas_1[, 1:3], "all"))
stopifnot(inherits(x, "substrate_data"))
data(vaas_4)
(y <- substrate_info(vaas_4[, , 1:3], "all"))
stopifnot(identical(x, y))
}
\references{
  Bochner, B. R., pers. comm.

  \url{http://en.wikipedia.org/wiki/CAS_registry_number}

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

  \url{http://www.ebi.ac.uk/chebi/}

  Hastings, J., de Matos, P., Dekker, A., Ennis, M.,
  Harsha, B., Kale, N., Muthukrishnan, V., Owen, G.,
  Turner, S., Williams, M., Steinbeck, C. 2013 The ChEBI
  reference database and ontology for biologically relevant
  chemistry: enhancements for 2013. \emph{Nucleic Acids
  Research} \strong{41}: D456--D463.
}
\seealso{
  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}}, \code{\link{listing}},
  \code{\link{well_to_substrate}}
}
\keyword{utilities}

\docType{methods}
\alias{substrate_info-methods}
\alias{substrate_info,character-method}
\alias{substrate_info,substrate_match-method}
\alias{substrate_info,list-method}
\alias{substrate_info,OPM-method}
\alias{substrate_info,factor-method}
\alias{substrate_info,OPMS-method}
\usage{
  \S4method{substrate_info}{character}(object,
    what = c("cas", "kegg", "drug", "metacyc", "chebi", "mesh", "downcase",
      "greek", "html", "all"), ...) 

  \S4method{substrate_info}{substrate_match}(object, ...) 

  \S4method{substrate_info}{list}(object, ...) 

  \S4method{substrate_info}{OPM}(object, ...) 

  \S4method{substrate_info}{factor}(object, ...) 

  \S4method{substrate_info}{OPMS}(object, ...) 

}
