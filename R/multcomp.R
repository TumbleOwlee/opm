

################################################################################


#' Multiple comparison of group means
#'
#' This function invokes functionality coded in \code{glht} (from the
#' \pkg{multcomp} package) and thus provides linear-hypothesis testing and
#' multiple comparisons for group means of curve parameters. It is an error if
#' this function is called and the package \pkg{multcomp} is unavailable (even
#' though it is not formally a dependency of \pkg{opm}).
#'
#' @param object \code{\link{OPMS}} object or data frame derived via
#'   \code{\link{extract}} containing factor variables that determine
#'   experimental groups for multiple comparison of means and can be selected
#'   using \code{as.labels}.
#'
#' @param as.labels List or character vector specifying the factor variables
#'   which determine the experimental groups to be compared. If \code{object} is
#'   of class \code{\link{OPMS}}, \code{as.labels} is passed to
#'   \code{\link{extract}} and must not be \code{NULL} but given as a list. If
#'   \code{object} is of class \code{data.frame}, \code{as.labels} can be given
#'   as a character vector, and by default all factor variables included in
#'   \code{object} are used. A logical or numeric vector could also be passed in
#'   that case and would be used to select from the column names.
#'
#' @param model A model formula or a character vector or a list containing the
#'   names of factors to be included in the model for fitting. If a numeric or
#'   logical vector, used to select the names from \code{as.labels}. The
#'   operator can be specified using \code{op}. See \code{formula} for further
#'   details (in the \pkg{stats} package).
#'
#' @param op character scalar containing \sQuote{+}, \sQuote{*}, or \sQuote{:},
#'   with \sQuote{+} as default. It indicates the operator(s) to insert between
#'   the variables in the right part of the \code{model} formula. See
#'   description of \code{formula} for further details (in the \pkg{stats}
#'   package). \code{op} is ignored if \code{model} is directly given as a
#'   formula.
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
#' @param ... Optional argument passed to \code{\link{extract}}.
#'
#' @return Usually an object of class \sQuote{glht} with \code{print},
#'   \code{summary}, \code{confint}, \code{coef} and \code{vcov} methods being
#'   available. See \code{glht} in the \pkg{multcomp} package for details. As an
#'   exception, if \code{do.mcp} is \code{FALSE}, no multiple comparison is
#'   performed but the return value is a data frame containing the reshaped data
#'   with one column for the measured values, one factorial variable determining
#'   the well, one factorial variable for the parameter and additional factorial
#'   variables if labels have been selected.
#'
#' @keywords htest
#' @export
#'
#' @family multcomp-functions
#' @seealso multcomp::glht stats::lm stats::formula
#'
#' @details This function internally reshapes the data into a \sQuote{flat} data
#'   frame containing column for the measured values, one factorial variable
#'   determining the wells, one factorial variable for the parameter and
#'   additional factorial variables if labels have been selected. By invoking
#'   function \code{glht} the user is then enabled to set up (general linear)
#'   models and, indicating a contrast type, user-defined simultaneous multiple
#'   testing procedures. If \code{do.mcp} is \code{FALSE} no multiple
#'   comparisons are performed and just the reshaped data frame is returned
#'   Since this function makes use of \code{mcp}, we refer to the
#'   \sQuote{details} section from \code{glht}. The \code{mcp} function muss be
#'   used with care when defining parameters of interest in two-way ANOVA or
#'   ANCOVA models. The definition of treatment differences might be
#'   problem-specific. An automated determination of the parameters of interest
#'   would be impossible and thus only comparisons for the main effects
#'   (ignoring covariates and interactions) would be generated and a warning
#'   issued.
#'
#' @examples
#'
#' ## OPMS method
#' data(vaas_4)
#'
#' # without computation of multiple comparisons of means
#' summary(x <- opm_mcp(vaas_4, as.labels = list("Species", "Strain"),
#'   do.mcp = FALSE))
#' stopifnot(is.data.frame(x), dim(x) == c(384L, 6L))
#'
#' # comparison using specified model comparing 'Species' pooled over
#' # complete plates
#' (x <- opm_mcp(vaas_4, as.labels = list("Species"), m.type = "lm",
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
#' (x <- opm_mcp(vaas_4[, , 1:4], as.labels = list("Species", "Strain"),
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
#' (x <- opm_mcp(vaas_4[, , 1:4], as.labels = list("Species", "Strain"),
#'   m.type = "lm", mcp.def = a, model = Value ~ Well))
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 3)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 20, 3, 2))
#' plot(x)
#' par(op) # reset plotting settings
#'
#'
#' ## data-frame method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
#'   dataframe = TRUE)
#'
#' # without performing the MCP
#' (y <- opm_mcp(x, do.mcp = FALSE, as.labels = list("Species", "Strain")))
#' stopifnot(is.data.frame(y), dim(y) == c(384L, 6L))
#'
#' # now with conducting the test
#' (y <- opm_mcp(x, as.labels = "Species", m.type = "lm",
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
#'   mcp.def = mcp(Strain = "Dunnett"), as.labels = "Strain", m.type = "lm"))
#' stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
#' # creating an informative plot
#' op <- par(no.readonly = TRUE) # default plotting settings
#' par(mar = c(3, 15, 3, 2))
#' plot(y)
#' par(op) # reset plotting settings
#'
setGeneric("opm_mcp",
  function(object, ...) standardGeneric("opm_mcp"))

setMethod("opm_mcp", OPMS, function(object, as.labels,
    mcp.def = c(Dunnett = 1L),
    model = as.labels, op = "+", m.type = "glm", glht.args = list(),
    do.mcp = TRUE, ...) {
  opm_mcp(object = extract(object = object, as.labels = as.labels,
    dataframe = TRUE, ...), as.labels = as.labels, mcp.def = mcp.def,
    op = op, model = model, m.type = m.type, split.at = "Parameter",
    glht.args = glht.args, do.mcp = do.mcp)
}, sealed = SEALED)

setMethod("opm_mcp", "data.frame", function(object, as.labels,
    mcp.def = c(Dunnett = 1L),
    model = as.labels, op = c("+", ":", "*"), m.type = c("glm", "lm", "aov"),
    glht.args = list(), split.at = "Parameter", do.mcp = TRUE) {

  # helper functions and dependencies
  convert_and_check_model <- function(model, op, as.labels) {
    enforce_left_side <- function(f) {
      if (length(f) < 3L) # f must be a formula
        f[[3L]] <- f[[2L]]
      f[[2L]] <- as.name("Value")
      f
    }
    if (inherits(model, "formula")) # user-defined formula
      return(enforce_left_side(model))
    if (!length(model))
      stop("'model' must not by empty")
    if (is.list(model))
      # convert the list to what would be expected if it had previously been
      # passed to extract() because by default it is the same than 'as.labels';
      model <- names(create_names(model)) # see metadata() for why this works
    else if (is.numeric(model) || is.logical(model))
      model <- as.labels[model]
    else if (!is.character(model))
      stop("'model' must either be a list, a vector or a formula")
    # TODO: currently only the first element of 'op' is included
    as.formula(paste("Value ~", paste(sprintf("`%s`", model), collapse = op)))
  }
  convert_and_check_labels <- function(as.labels, column.names) {
    if (is.list(as.labels))
      # convert a list to what would be expected if it had previously been
      # passed to extract(); see metadata() for why this works
      as.labels <- names(create_names(as.labels))
    if (is.character(as.labels)) {
      if (length(bad <- which(!as.labels %in% column.names)))
        stop("cannot find column name: ", as.labels[bad[1L]])
    } else
      as.labels <- column.names[as.labels]
    if (anyDuplicated(as.labels))
      warning("'as.labels' entries are not unique")
    as.labels
  }
  convert_hypothesis_spec <- function(mcp.def, as.labels) {
    if (!length(mcp.def))
      stop("hypothesis definition 'mcp.def' must not be empty")
    # TODO Lea: the next line includes all kinds of objects that are used
    # directly; check whether these and only these make sense
    if (inherits(mcp.def, c("mcp", "matrix", "expression")))
      return(mcp.def)
    if (inherits(mcp.def, "AsIs")) # created using I()
      return(if (is.list(mcp.def))
          do.call(mcp, mcp.def)
        else
          mcp.def)
    # TODO Lea: check whether additional kinds of conversions could make sense;
    # we could also create a helper function -- the main idea is that the user
    # should calculate mcp.def easily from as.labels.
    if (is.list(mcp.def))
      result <- mcp.def <- names(create_names(mcp.def))
    else if (is.numeric(mcp.def) || is.logical(mcp.def))
      result <- as.labels[mcp.def]
    else if (is.character(mcp.def))
      result <- mcp.def
    else
      stop("invalid object passed as 'mcp.def' argument")
    names(result) <- if (is.null(names(mcp.def)))
      # TODO Lea: check whether other defaults might make sense
      rep.int("Dunnett", length(result))
    else
      names(mcp.def)
    do.call(mcp, result)
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

  as.labels <- convert_and_check_labels(as.labels,
    colnames(object)[seq.int(param.pos)])

  # create reshaped data frame and set helper column '.ID' to avoid non-unique
  # values when setting 'row.names'; note according shift of column positions!
  object <- reshape(cbind(.ID = seq.int(nrow(object)), object),
    direction = "long",
    idvar = c(".ID", colnames(object)[seq.int(param.pos - 1L)]),
    varying = colnames(object)[seq.int(param.pos + 1L, ncol(object))],
    v.names = "Value",
    timevar = "Well",
    times = colnames(object)[seq.int(param.pos + 1L, ncol(object))])
  rownames(object) <- NULL
  object$Well <- as.factor(object$Well)

  if (!do.mcp)
    return(object)

  # TODO LEA: the next step has been extremely simplified; please check
  # whether assert_all_factors_are_variable() still does what it should do
  #
  # TODO LEA: your own tests fail if the function really checks the non-
  # uniqueness of the factors -- so what is it good for?
  #
  ##assert_all_factors_are_variable(object)

  model <- convert_and_check_model(model, match.arg(op), as.labels)
  model <- do.call(match.arg(m.type), list(formula = model, data = object))

  # TODO Lea: look this function up and once finished describe the mcp.def
  # argument properly and introduce examples
  mcp.def <- convert_hypothesis_spec(mcp.def, as.labels)

  # fitting the linear model according to 'm.type'
  glht.args <- c(list(model = model, linfct = mcp.def), as.list(glht.args))
  result <- do.call(glht, glht.args)

  # check the number of calculated tests
  if (length(confint(result)$confint[, 1L]) > 20L)
    warning("number of performed comparisons exceeds 20")

  result
}, sealed = SEALED)


################################################################################



