

################################################################################


#' Multiple comparison of group means
#'
#' This function provides linear-hypothesis testing and multiple comparisons for
#' group means of curve parameters. It is an error if this function is called
#' and the package \pkg{multcomp} is unavailable (even though it is not formally
#' a dependency of \pkg{opm}).
#'
#' @param object \code{\link{OPMS}} object or data frame derived via
#'   \code{\link{extract}} containing factor variables that determine
#'   experimental groups for multiple comparison of means and can be selected
#'   using \code{as.labels}.
#'
#' @param model A model formula or a character vector or a list containing the
#'   names of factors to be included in the model for fitting. For model
#'   specifications using formulas in general, see \code{formula} (in the
#'   \pkg{stats} package).
#'
#'   If \code{object} is of class \code{\link{OPMS}}, \code{model} is passed to
#'   \code{\link{extract}} after removal of the reserved names (see
#'   \code{\link{param_names}}, which can nevertheless be included in the model
#'   formula as they always contained in the resulting data frame. If
#'   \code{model} is a list or vector, it is then automatically converted to a
#'   formula, using operator(s) as specified with \code{ops}.
#'
#' @param m.type Character scalar indicating which of the following model types
#'   to use in model fitting: \sQuote{glm}, \sQuote{aov} or \sQuote{lm}. See
#'   \code{lm} (in \pkg{stats}) for details.
#'
#' @param mcp.def Character scalar. A specification of the linear hypotheses to
#'   be tested analogously to \code{linfct} in \code{glht}. Linear functions can
#'   be specified by either the matrix of coefficients or by symbolic
#'   descriptions of one or more linear hypotheses. See also \code{contrMat}
#'   from the \pkg{multcomp} package.
#'
#' @param glht.args List of additional arguments for the multiple comparison
#'   procedure passed to \code{glht}. See \code{glht} in the \pkg{multcomp}
#'   package for details.
#'
#' @param do.mcp Logical scalar that determines whether or not a multiple
#'   comparison of groups means should be performed. If \code{FALSE}, just a
#'   reshaped data frame is returned which contains the variables given in
#'   \code{as.labels} as factors and can be used for more complex model building
#'   by the user user.
#'
#' @param split.at Character vector. See \code{\link{extract}}. Cannot be set in
#'   the case of the \code{\link{OPMS}} method.
#'
#' @param ops character vector containing \sQuote{+}, \sQuote{*}, or \sQuote{:},
#'   as elements (with \sQuote{+} as default). It indicates the operator(s) to
#'   insert between the variables in the right part of the \code{model} formula.
#'   See description of \code{formula} for further details (in the \pkg{stats}
#'   package). \code{ops} is ignored if \code{model} is directly given as a
#'   formula.
#'
#' @param ... Optional argument passed to \code{\link{extract}}.
#'
#' @return Usually an object of class \sQuote{glht} with \code{print},
#'   \code{summary}, \code{confint}, \code{coef} and \code{vcov} methods being
#'   available. See \code{glht} in the \pkg{multcomp} package for details.
#'
#'   As an exception, if \code{do.mcp} is \code{FALSE}, no multiple comparison
#'   is performed but the return value is the \sQuote{flat} data frame described
#'   under \sQuote{Details}. containing the reshaped data with one column for
#'   the measured values, one factorial variable determining the well, one
#'   factorial variable for the parameter and additional factorial variables if
#'   labels have been selected.
#'
#' @keywords htest
#' @export
#'
#' @author Lea A.I. Vaas
#' @family multcomp-functions
#' @seealso multcomp::glht stats::lm stats::formula
#'
#' @details In the first step, this function internally reshapes the data into a
#'   \sQuote{flat} data frame containing one column for the measured values, one
#'   factorial variable indicating the wells, one factorial variable for the
#'   curve parameter (see \code{\link{param_names}} and additional factorial
#'   variables determined by \code{model}. In the second step (which is
#'   conducted unless \code{do.mcp} is \code{FALSE}), \code{glht} from the
#'   \pkg{multcomp} package is applied to this data frame. Thus, (general
#'   linear) models and, by indicating a contrast type, user-defined
#'   simultaneous multiple testing procedures are applied to the
#'   \code{\link{OPMS}} object.
#'
#'   Since either the user or this function itself makes use of \code{mcp}, we
#'   refer to the \sQuote{Details} section of the \code{glht} function. The
#'   \code{mcp} function must be used with care when defining parameters of
#'   interest in two-way ANOVA or ANCOVA models. The definition of treatment
#'   differences might be problem-specific. An automated determination of the
#'   parameters of interest would be impossible and thus only comparisons for
#'   the main effects (ignoring covariates and interactions) would be generated
#'   and a warning issued.
#'
#' @examples
#'
#' ## OPMS method
#' data(vaas_4)
#'
#' # without computation of multiple comparisons of means
#' summary(x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   do.mcp = FALSE))
#' stopifnot(is.data.frame(x), dim(x) == c(384L, 6L))
#'
#' # comparison using specified model comparing 'Species' pooled over
#' # complete plates
#' (x <- opm_mcp(vaas_4, model = list("Species"), m.type = "lm",
#'   mcp.def = mcp(Species = "Dunnett")))
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 1)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 20, 3, 2))
#' plot(x)
#' par(op) # reset plotting settings
#'
#' # comparison of only A01 - A04 against each other
#' # note that the left side of the model is set automatically
#' (x <- opm_mcp(vaas_4[, , 1:4],
#'   model = ~ Well + Species, m.type = "lm",
#'   mcp.def = mcp(Well = "Tukey")))
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 6)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 18, 3, 2))
#' plot(x)
#' par(op) # reset plotting settings
#'
#' # user-defined contrast matrix
#' a <- mcp(Well = "Dunnett")
#' (x <- opm_mcp(vaas_4[, , 1:4], model = Value ~ Well,
#'   m.type = "lm", mcp.def = a))
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 3)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 20, 3, 2))
#' plot(x)
#' par(op) # reset plotting settings
#'
#' # manually defined contrast matrix
#' contr <- rbind(
#'   "A01 (Negative Control) - A02 (Dextrin)" = c(1, -1, 0, 0),
#'   "A01 (Negative Control) - A03 (D-Maltose)" = c(-1, 0, 1, 0),
#'   "A01 (Negative Control) - A04 (D-Trehalose)" = c(-1, 0, 0, -1),
#'   "A03 (D-Maltose) - A04 (D-Trehalose)" = c(0, 0, 1, -1))
#' x <- opm_mcp(vaas_4[, , 1:4],
#'   model = ~ Well, m.type = "lm", mcp.def = contr)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 20, 3, 2))
#' plot(x)
#' par(op) # reset plotting settings
#'
#' ## data-frame method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
#'   dataframe = TRUE)
#'
#' # without performing the MCP
#' (y <- opm_mcp(x, do.mcp = FALSE, model = list("Species", "Strain")))
#' stopifnot(is.data.frame(y), dim(y) == c(384L, 6L))
#'
#' # now with conducting the test
#' (y <- opm_mcp(x, model = "Species", m.type = "lm",
#'   mcp.def = mcp(Species = "Dunnett")))
#' stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 15, 3, 2))
#' plot(y)
#' par(op) # reset plotting settings
#'
#' # testing for subsets of object
#' (y <- opm_mcp(subset(x, x$Species == "Escherichia coli"),
#'   mcp.def = mcp(Strain = "Dunnett"), model = "Strain", m.type = "lm"))
#' stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 15, 3, 2))
#' plot(y)
#' par(op) # reset plotting settings
#'
setGeneric("opm_mcp",
  function(object, ...) standardGeneric("opm_mcp"))

setMethod("opm_mcp", OPMS, function(object, model, mcp.def = c(Dunnett = 1L),
    m.type = "glm", glht.args = list(), ops = "+", do.mcp = TRUE, ...) {
  object <- extract(object = object, dataframe = TRUE, ...,
    as.labels = metadata_key(model, FALSE, ops = ops,
      remove = RESERVED_NAMES[c("well", "value", "parameter")]))
  opm_mcp(object = object, model = model, mcp.def = mcp.def, ops = ops,
    m.type = m.type, split.at = param_names("split.at"), glht.args = glht.args,
    do.mcp = do.mcp)
}, sealed = SEALED)

setMethod("opm_mcp", "data.frame", function(object, model,
    mcp.def = c(Dunnett = 1L), ops = "+", m.type = c("glm", "lm", "aov"),
    glht.args = list(), split.at = param_names("split.at"), do.mcp = TRUE) {

  # helper functions and dependencies
  convert_and_check_model <- function(model, ops) {
    enforce_left_side <- function(f) {
      if (length(f) < 3L) # f must be a formula
        f[[3L]] <- f[[2L]]
      f[[2L]] <- as.name(RESERVED_NAMES[["value"]])
      f
    }
    enforce_left_side(metadata_key(model, TRUE, ops = ops))
#     if (inherits(model, "formula")) # user-defined formula
#       return(enforce_left_side(model))
#     if (!length(model))
#       stop("'model' must not be empty")
#     if (is.list(model))
#       # convert the list to what would be expected if it had previously been
#       # passed to extract() because by default it is the same than
#       # 'as.labels';
#       model <- names(metadata_key(model, FALSE)) # see metadata()
#     else if (is.numeric(model) || is.logical(model))
#       model <- as.labels[model]
#     else if (!is.character(model))
#       stop("'model' must either be a list, a vector or a formula")
#     # TODO: currently only the first element of 'ops' is included
#     as.formula(paste(RESERVED_NAMES[["value"]], "~",
#       paste(sprintf("`%s`", model), collapse = ops)))
  }
#   convert_and_check_labels <- function(as.labels, column.names) {
#     if (is.list(as.labels))
#       # convert a list to what would be expected if it had previously been
#       # passed to extract(); see metadata() for why this works
#       as.labels <- names(metadata_key(as.labels, FALSE))
#     if (is.character(as.labels)) {
#       if (length(bad <- which(!as.labels %in% column.names)))
#         stop("cannot find column name: ", as.labels[bad[1L]])
#     } else
#       as.labels <- column.names[as.labels]
#     if (anyDuplicated(as.labels))
#       warning("'as.labels' entries are not unique")
#     as.labels
#   }
  convert_hypothesis_spec <- function(mcp.def, model) {
    if (!length(mcp.def))
      stop("hypothesis definition 'mcp.def' must not be empty")
    # TODO Lea: the next line includes all kinds of objects that are used
    # directly; check whether these and only these make sense
    # TODO Lea: check whether 'expression' objects could be given here
    if (inherits(mcp.def, c("mcp", "matrix")))
      return(mcp.def)
    if (inherits(mcp.def, "AsIs")) # created using I()
      return(if (is.list(mcp.def))
          do.call(mcp, mcp.def)
        else
          mcp.def)
    mcp.def <- metadata_key(mcp.def, FALSE)
    # TODO Lea: check whether additional kinds of conversions could make sense;
    # we could also create a helper function -- the main idea is that the user
    # should calculate mcp.def easily from as.labels.
    if (is.list(mcp.def))
      result <- names(mcp.def)
    else if (is.numeric(mcp.def) || is.logical(mcp.def))
      result <- names(metadata_key(model, FALSE))[mcp.def]
    else if (is.character(mcp.def))
      result <- mcp.def
    else
      stop("invalid object passed as 'mcp.def' argument")
    names(result) <- if (is.null(names(mcp.def)))
      # TODO Lea: check whether other defaults might make sense
      rep.int("Dunnett", length(result))
    else
      names(mcp.def)
    do.call(mcp, as.list(result))
  }
#   assert_all_factors_are_variable <- function(x) {
#     pos <- seq.int(match("Well", colnames(x)) - 2L)
#     bad <- vapply(pos, is_constant, logical(1L))
#     if (any(bad))
#       stop("constant factor(s) encountered: ",
#         paste(sprintf("'%s'", colnames(x)[pos][bad]), collapse = ", "))
#     NULL
#   }
  if (!suppressWarnings(suppressPackageStartupMessages(require(
      multcomp, quietly = TRUE, warn.conflicts = FALSE))))
    stop("package 'multcomp' must be available to run this function")

  param.pos <- assert_splittable_matrix(object, split.at)

#   as.labels <- convert_and_check_labels(as.labels,
#     colnames(object)[seq.int(param.pos)])

  #print(names(object)[1:5])

  # create reshaped data frame and set helper column '.ID' to avoid non-unique
  # values when setting 'row.names'; note according shift of column positions!
  object <- reshape(cbind(.ID = seq.int(nrow(object)), object),
    direction = "long",
    idvar = c(".ID", colnames(object)[seq.int(param.pos - 1L)]),
    varying = colnames(object)[seq.int(param.pos + 1L, ncol(object))],
    v.names = RESERVED_NAMES[["value"]],
    timevar = RESERVED_NAMES[["well"]],
    times = colnames(object)[seq.int(param.pos + 1L, ncol(object))])
  rownames(object) <- NULL
  object[, RESERVED_NAMES[["well"]]] <- as.factor(
    object[, RESERVED_NAMES[["well"]]])

  if (!do.mcp)
    return(object)

  # TODO LEA: the next step has been extremely simplified; please check
  # whether assert_all_factors_are_variable() still does what it should do
  #
  # TODO LEA: your own tests fail if the function really checks the non-
  # uniqueness of the factors -- so what is it good for?
  #
  ##assert_all_factors_are_variable(object)

  model <- convert_and_check_model(model, match.arg(ops))

  # TODO Lea: look this function up and once finished describe the mcp.def
  # argument properly and introduce examples
  mcp.def <- convert_hypothesis_spec(mcp.def, model)

  model <- do.call(match.arg(m.type), list(formula = model, data = object))

  # fitting the linear model according to 'm.type'
  glht.args <- c(list(model = model, linfct = mcp.def), as.list(glht.args))
  result <- do.call(glht, glht.args)

  # check the number of calculated tests
  if (length(confint(result)$confint[, 1L]) > 20L)
    warning("number of performed comparisons exceeds 20")

  result
}, sealed = SEALED)


################################################################################



