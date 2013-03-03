

################################################################################


#' Multiple comparison of group means
#'
#' This function invokes functionality from \code{glht} (from package
#' \pkg{multcomp}) and thus provides linear hypothesis-testing and multiple
#' comparisons for group means of curve parameters. It is an error if this
#' function is called and the package \pkg{multcomp} is unavailable (even though
#' it is not formally a dependency of \pkg{opm}).
#'
#' @param object \code{\link{OPMS}} object or data frame derived via
#'   \code{\link{extract}} with  specified labels (factor-variables) determining
#'   experimental groups for multiple comparison of means.
#'
#' @param as.labels List or character vector specifying the factor variables
#'   which determine the experimental groups to be compared. If \code{object} is
#'   of class \code{\link{OPMS}}, \code{as.labels} is passed to
#'   \code{\link{extract}} and must not be \code{NULL}, but given as a list. If
#'   \code{object} is of class \code{data.frame}, \code{as.labels} can be given
#'   as a character vector, and by default all factor variables included in
#'   \code{object} are used.
#'
#' @param model A model formula or a character-vector or a list containing the
#'   names of factors to be included in the model for fitting. The operator can
#'   be specified usin \code{op}. See \code{formula} for further details (in
#'   the \pkg{stats} package).
#'
#' @param op character scalar containing "+", "*", or ":", with "+" as default.
#'   It indicates the operator(s) to insert between the variables in the right
#'   part of the \code{model }formula. See description of \code{formula} for
#'   further details. Ignored if \code{model} is given as a formula.
#'
#' @param m.type Character scalar indicating which of the following model types
#'   to use in model fitting: \sQuote{glm}, \sQuote{aov} or \sQuote{lm}. See
#'   \code{lm} (in \pkg{stats}) for details.
#'
#' @param mcp.def Character scalar. A specification of the linear hypotheses to
#'   be tested analogously to \code{linfct} in \code{glht}: Linear functions can
#'   be specified by either the matrix of coefficients or by symbolic
#'   descriptions of one or more linear hypotheses. See also \code{contrMat}
#'   from the \pkg{multcomp} package.
#'
#~ @param sub.list Numerical vector determining whether instead of complete
#~   plates only a subset of substrates should be used for the comparisons. The
#~   default, \code{NULL}, compares complete plates.
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
#' @param ... Optional argument passed to \code{\link{extract}}.
#'
#' @return
#'   Usually an object of class \sQuote{glht} with \code{print}, \code{summary},
#'   \code{confint}, \code{coef} and \code{vcov} methods being available. See
#'   \code{glht} in the \pkg{multcomp} package for details. As an exception, if
#'   \code{do.mcp} is \code{FALSE}, no multiple comparison is performed but the
#'   return value is a data frame containing the reshaped data with one column
#'   for the measured values, one factorial variable determining the well, one
#'   factorial variable for the parameter and additional factorial variables if
#'   labels have been selected.
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

setMethod("opm_mcp", OPMS, function(object, as.labels, mcp.def,
    op = "+", model = as.labels, m.type = "glm", #sub.list = NULL,
    glht.args = list(), do.mcp = TRUE, ...) {
  opm_mcp(object = extract(object = object, as.labels = as.labels,
    dataframe = TRUE, ...), as.labels = as.labels, mcp.def = mcp.def,
    op = op, model = model, m.type = m.type, #sub.list = sub.list,
    glht.args = glht.args, do.mcp = do.mcp)
}, sealed = SEALED)

setMethod("opm_mcp", "data.frame", function(object, as.labels, mcp.def,
    op = c("+", ":", "*"), model = as.labels, m.type = c("glm", "lm", "aov"),
    #sub.list = NULL,
    glht.args = list(), do.mcp = TRUE) {

  # helper functions and dependencies
  enforce_left_side <- function(f, what = as.name("Value")) {
    if (length(f) < 3L) # f must be a formula
      f[[3L]] <- f[[2L]]
    f[[2L]] <- what
    f
  }
  create_formula <- function(model, op) {
    if (!length(model))
      stop("'model' must not by empty")
    as.formula(paste("Value ~", paste(sprintf("`%s`", model), collapse = op)))
  }
  if (!suppressWarnings(require(
      multcomp, quietly = TRUE, warn.conflicts = FALSE)))
    stop("package 'multcomp' must be available to run this function")

  # check data-frame structure
  param.pos <- which(colnames(object) == "Parameter")
  if (length(param.pos) != 1L)
    stop("need data frame with exactly one column called 'Parameter'")
  if (param.pos == ncol(object))
    stop("no numerical data or 'Parameter'-column at wrong position")

  # convert and check 'as.labels'
  if (is.list(as.labels))
    # convert a list to what would be expected if it had previously been
    # passed to extract(); see metadata() for why this works
    as.labels <- names(create_names(as.labels))
  # now check for duplicates and missing values
  if (anyDuplicated(as.labels))
    warning("'as.labels' entries are not unique")
  bad <- which(!as.labels %in% colnames(object)[seq.int(param.pos)])
  if (length(bad))
    stop("cannot find column name: ", as.labels[bad[1L]])

  # create reshaped data frame
  # set helper column to avoid non-unique values when setting 'row.names'
  object <- cbind(seq.int(nrow(object)), object)
  result <- reshape(object,
    direction = "long",
    idvar = colnames(object[, seq.int(param.pos)]),
    varying = colnames(object[, seq.int(param.pos + 2L, ncol(object))]),
    v.names = "Value",
    timevar = "Well",
    times = colnames(object[, seq.int(param.pos + 2L, ncol(object))]))
  rownames(result) <- NULL
  result$Well <- as.factor(result$Well)

  if (!do.mcp)
    return(result)

  # raise error if any of the factors is constant
  bad <- !vapply(pos <- seq.int(match("Well", colnames(result))),
    function(i) anyDuplicated(result[, i]), integer(1L))
  if (any(bad))
    stop("Only one level for factor variable(s): ",
      paste(colnames(result)[pos][bad], collapse = ", "))
  # TODO LEA: the last section has been extremely simplified; please check
  # whether it still does what it should do

  # set the model up
  op <- match.arg(op)
  if (is.character(model))
    model <- create_formula(model, op)
  else if (is.list(model))
    # first convert the list to what would be expected if it had previously
    # been passed to extract() (because by default it is the same than
    # 'as.labels'); see metadata() for why this works
    model <- create_formula(names(create_names(model)), op)
  else if (inherits(model, "formula"))
    model <- enforce_left_side(model) # user-defined model statement
  else
    stop("'model' must either be a list, a character vector or a formula")

  # fitting the linear model according to 'm.type'
  lmmod <- case(match.arg(m.type), lm = lm(model, data = result),
    aov = aov(model, data = result), glm = glm(model, data = result))
  glht.args <- c(list(model = lmmod, linfct = mcp.def), as.list(glht.args))
  result <- do.call(glht, glht.args)

  # check the number of calculated tests
  if (length(confint(result)$confint[, 1L]) > 20L)
    ## TODO Lea: either this is worth a warning or it should be omitted
    message("number of performed comparisons exceeds 20")

  result
}, sealed = SEALED)


################################################################################



