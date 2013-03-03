\name{opm_mcp}
\alias{opm_mcp}
\title{Multiple comparison of group means}
\arguments{
  \item{object}{\code{\link{OPMS}} object or data frame
  derived via \code{\link{extract}} with specified labels
  (factor-variables) determining experimental groups for
  multiple comparison of means.}

  \item{as.labels}{List or character vector specifying the
  factor variables which determine the experimental groups
  to be compared. If \code{object} is of class
  \code{\link{OPMS}}, \code{as.labels} is passed to
  \code{\link{extract}} and must not be \code{NULL}, but
  given as a list. If \code{object} is of class
  \code{data.frame}, \code{as.labels} can be given as a
  character vector, and by default all factor variables
  included in \code{object} are used.}

  \item{model}{A model formula or a character-vector or a
  list containing the names of factors to be included in
  the model for fitting. The operator can be specified usin
  \code{op}. See \code{formula} for further details (in the
  \pkg{stats} package).}

  \item{op}{character scalar containing "+", "*", or ":",
  with "+" as default.  It indicates the operator(s) to
  insert between the variables in the right part of the
  \code{model }formula. See description of \code{formula}
  for further details. Ignored if \code{model} is given as
  a formula.}

  \item{m.type}{Character scalar indicating which of the
  following model types to use in model fitting:
  \sQuote{glm}, \sQuote{aov} or \sQuote{lm}. See \code{lm}
  (in \pkg{stats}) for details.}

  \item{mcp.def}{Character scalar. A specification of the
  linear hypotheses to be tested analogously to
  \code{linfct} in \code{glht}: Linear functions can be
  specified by either the matrix of coefficients or by
  symbolic descriptions of one or more linear hypotheses.
  See also \code{contrMat} from the \pkg{multcomp}
  package.}

  \item{glht.args}{List of additional arguments for the
  multiple comparison procedure passed to \code{glht}. See
  \code{glht} in the \pkg{multcomp} package for details.}

  \item{do.mcp}{Logical scalar that determines whether or
  not a multiple comparison of groups means should be
  performed. If \code{FALSE}, just a reshaped data frame is
  returned which contains the variables given in
  \code{as.labels} as factors and can be used for more
  complex model building by the user user.}

  \item{...}{Optional argument passed to
  \code{\link{extract}}.}
}
\value{
  Usually an object of class \sQuote{glht} with
  \code{print}, \code{summary}, \code{confint}, \code{coef}
  and \code{vcov} methods being available. See \code{glht}
  in the \pkg{multcomp} package for details. As an
  exception, if \code{do.mcp} is \code{FALSE}, no multiple
  comparison is performed but the return value is a data
  frame containing the reshaped data with one column for
  the measured values, one factorial variable determining
  the well, one factorial variable for the parameter and
  additional factorial variables if labels have been
  selected.
}
\description{
  This function invokes functionality from \code{glht}
  (from package \pkg{multcomp}) and thus provides linear
  hypothesis-testing and multiple comparisons for group
  means of curve parameters. It is an error if this
  function is called and the package \pkg{multcomp} is
  unavailable (even though it is not formally a dependency
  of \pkg{opm}).
}
\details{
  This function internally reshapes the data into a
  \sQuote{flat} data frame containing column for the
  measured values, one factorial variable determining the
  wells, one factorial variable for the parameter and
  additional factorial variables if labels have been
  selected. By invoking function \code{glht} the user is
  then enabled to set up (general linear) models and,
  indicating a contrast type, user-defined simultaneous
  multiple testing procedures. If \code{do.mcp} is
  \code{FALSE} no multiple comparisons are performed and
  just the reshaped data frame is returned Since this
  function makes use of \code{mcp}, we refer to the
  \sQuote{details} section from \code{glht}. The \code{mcp}
  function muss be used with care when defining parameters
  of interest in two-way ANOVA or ANCOVA models. The
  definition of treatment differences might be
  problem-specific. An automated determination of the
  parameters of interest would be impossible and thus only
  comparisons for the main effects (ignoring covariates and
  interactions) would be generated and a warning issued.
}
\examples{
## OPMS method
data(vaas_4)

# without computation of multiple comparisons of means
summary(x <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"),
  do.mcp = FALSE))
stopifnot(is.data.frame(x), dim(x) == c(384L, 6L))

# comparison using specified model comparing 'Species' pooled over
# complete plates
(x <- opm_mcp(vaas_4, as.labels = list("Species"), m.type = "lm",
  mcp.def = mcp(Species = "Dunnett")))
stopifnot(inherits(x, "glht"), length(coef(x)) == 1)
# creating an informative plot
op <- par(no.readonly = TRUE) # default plotting settings
par(mar = c(3, 20, 3, 2))
plot(x)
par(op) # reset plotting settings

# comparison of only A01 - A04 against each other
# note that the left side of the model is set automatically
(x <- opm_mcp(vaas_4[, , 1:4], as.labels = list("Species", "Strain"),
  model = ~ Well + Species, m.type = "lm",
  mcp.def = mcp(Well = "Tukey")))
stopifnot(inherits(x, "glht"), length(coef(x)) == 6)
# creating an informative plot
op <- par(no.readonly = TRUE) # default plotting settings
par(mar = c(3, 18, 3, 2))
plot(x)
par(op) # reset plotting settings

# user-defined contrast matrix
a <- mcp(Well = "Dunnett")
(x <- opm_mcp(vaas_4[, , 1:4], as.labels = list("Species", "Strain"),
  m.type = "lm", mcp.def = a, model = Value ~ Well))
stopifnot(inherits(x, "glht"), length(coef(x)) == 3)
# creating an informative plot
op <- par(no.readonly = TRUE) # default plotting settings
par(mar = c(3, 20, 3, 2))
plot(x)
par(op) # reset plotting settings


## data-frame method
x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
  dataframe = TRUE)

# without performing the MCP
(y <- opm_mcp(x, do.mcp = FALSE, as.labels = list("Species", "Strain")))
stopifnot(is.data.frame(y), dim(y) == c(384L, 6L))

# now with conducting the test
(y <- opm_mcp(x, as.labels = "Species", m.type = "lm",
  mcp.def = mcp(Species = "Dunnett")))
stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
# creating an informative plot
op <- par(no.readonly = TRUE) # default plotting settings
par(mar = c(3, 15, 3, 2))
plot(y)
par(op) # reset plotting settings

# testing for subsets of object
(y <- opm_mcp(subset(x, x$Species == "Escherichia coli"),
  mcp.def = mcp(Strain = "Dunnett"), as.labels = "Strain", m.type = "lm"))
stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
# creating an informative plot
op <- par(no.readonly = TRUE) # default plotting settings
par(mar = c(3, 15, 3, 2))
plot(y)
par(op) # reset plotting settings
}
\seealso{
  multcomp::glht stats::lm stats::formula
}
\keyword{htest}

\docType{methods}
\alias{opm_mcp-methods}
\alias{opm_mcp,OPMS-method}
\alias{opm_mcp,data.frame-method}
\usage{
  \S4method{opm_mcp}{OPMS}(object, as.labels, mcp.def,
    op = "+", model = as.labels, m.type = "glm", #sub.list = NULL,
    glht.args = list(), do.mcp = TRUE, ...) 

  \S4method{opm_mcp}{data.frame}(object, as.labels, mcp.def,
    op = c("+", ":", "*"), model = as.labels, m.type = c("glm", "lm", "aov"),
    #sub.list = NULL,
    glht.args = list(), do.mcp = TRUE) 

}
