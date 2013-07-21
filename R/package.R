

################################################################################
################################################################################
#
# Package description
#


#' The opm package
#'
#' Package for analysing OmniLog Phenotype MicroArray data.
#'
#' @name opm.package
#' @docType package
#'
#' @details Here is a brief guideline for using this manual:
#' \describe{
#'
#'   \item{families}{All functions and methods belong to a family of functions
#'   and methods with similar purposes. The respective other family members are
#'   found in each \sQuote{See Also} entry.}
#'
#'   \item{classes}{Users normally will create at least one object of the class
#'   \code{\link{OPM}} or derived classes. All these classes store PM data; they
#'   differ in whether they also contain aggregated values (\code{\link{OPMA}})
#'   or aggregated and discretized values (\code{\link{OPMD}}), and whether they
#'   contain more than a single plate (\code{\link{OPMS}}). Example objects are
#'   available via \code{\link{vaas_4}}.}
#'
#'   \item{input}{Most \pkg{opm} users will start with data input using
#'   \code{\link{read_opm}}, which create the appropriate objects.
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray
#'   data are structured in \strong{plates}. Each plate has 12 x 8 \strong{well}
#'   layout, and each well contains the respiration measurements on one
#'   substrate or inhibitor, or combination of substrates or inhibitors. For
#'   input example files, see \code{\link{opm_files}}.}
#'
#'   \item{global options}{Options affecting the default parameters of a number
#'   of \pkg{opm} functions can be set and queried for using
#'   \code{\link{opm_opt}}.}
#'
#'   \item{forbidden names}{Some names should be used with caution when
#'   annotating \pkg{opm} objects; see \code{\link{param_names}} for details.}
#'
#'   \item{YAML}{Input and output of \acronym{YAML} files is based on the
#'   \pkg{yaml} package. Up to \pkg{opm} version 0.7, this package was not
#'   required for the installation of \pkg{opm}. It is now mandatory to install
#'   one of the newer versions of \pkg{yaml} (>= v2.1.5). These are based on
#'   libyaml as parser instead of Syck, are faster and contain some bug fixes.
#'   The \acronym{YAML}-related functions of \pkg{opm} are \code{\link{to_yaml}}
#'   and \code{\link{batch_opm}}.}
#'
#'   \item{running time}{Computations on such high-dimensional data may take
#'   some time. The limiting steps are aggregating (curve-parameter estimation)
#'   and plotting many curves together. The former step can be conducted in
#'   parallel if \code{mclapply} from the \pkg{parallel} package can be run with
#'   more than 1 core (basically anywhere except for Windows). There is also a
#'   fast estimation method for the parameters \sQuote{area under the curve} and
#'   \sQuote{maximum height}. See \code{\link{do_aggr}} and the methods it
#'   refers to for details.}
#'
#'   \item{advanced plotting}{The \pkg{gplots} package is also not required for
#'   the installation of \pkg{opm} but can be used to draw more advanced
#'   heatmaps. See \code{\link{heat_map}} and its accompanying methods for
#'   details. The other customized plotting functions of the package are
#'   contained in the same method family.}
#' }
#' @references \url{http://www.biolog.com/}
#' @references Bochner, B. R., Gadzinski, P., Panomitros, E. 2001 Phenotype
#'   MicroArrays for high throughput phenotypic testing and assay of gene
#'   function. \emph{Genome Research} \strong{11}, 1246--1255.
#' @references Bochner, B. R. 2009 Global phenotypic characterization of
#'   bacteria. \emph{FEMS Microbiological Reviews} \strong{33}, 191--205.
#' @references \url{http://opm.dsmz.de/}
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#' @references \url{http://www.yaml.org/}
#' @keywords package
#'
NULL



