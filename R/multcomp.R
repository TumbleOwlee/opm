

################################################################################


#' Multiple comparison of group means
#'
#' This function provides linear-hypothesis testing and multiple comparisons for
#' group means of curve parameters. Note that for most \code{output} modes (see
#' below) it would be an error if this function was called and the package
#' \pkg{multcomp} was unavailable (even though that package is not formally a
#' dependency of \pkg{opm}).
#'
#' @param object Either an \code{\link{OPMS}} object, or a data frame derived
#'   via \code{\link{extract}} containing factor variables that determine
#'   experimental groups for multiple comparison of means and can be selected
#'   using \code{model}.
#'
#' @param model A model formula, or a character vector or a list containing the
#'   names of factors to be included in the model for fitting.
#'   In order to join two or more metadata-variables into one factor use
#'   pseudofunction \code{J} (described in \code{\link{extract}}). This is
#'   necessary especially when \code{linfct = Pairs}, see the examples below.
#'   For model specifications using formulas in general, see \code{formula} (in
#'   the \pkg{stats} package).
#'   For the way models are used by \pkg{opm} for selecting metadata entries,
#'   see \code{\link{metadata}}.
#'
#'   If \code{object} is of class \code{\link{OPMS}}, \code{model} is passed to
#'   \code{\link{extract}} after removal of the relevant reserved names (see
#'   \code{\link{param_names}}, which can nevertheless be included in the model
#'   formula as they are always contained in the resulting data frame. If
#'   \code{model} is a list or vector, it is used for selecting metadata, too,
#'   and for running the tests automatically converted to a formula, using one
#'   to several operators as specified with \code{ops}. Non-syntactical names
#'   within \code{formula} are converted to syntactical ones for use with
#'   \code{glht}. This is done in the same way in the data frame passed to that
#'   function.
#'
#'   If the \code{output} argument is set to \sQuote{model}, the function
#'   returns the converted \code{model} argument, which is useful in exploring
#'   the conducted conversions before running the proper tests.
#'
#' @param linfct A specification of the linear hypotheses to be tested
#'   analogously to \code{linfct} in \code{glht}. A variety of objects can be
#'   used for this argument: \itemize{
#'
#'   \item One of the classes of objects accepted by \code{glht} from the
#'   \pkg{multcomp} package as \code{linfct} argument. Such objects will not be
#'   modified. Linear functions can be specified by either the matrix of
#'   coefficients or by symbolic descriptions of one or more linear hypotheses.
#'   The set of existing types of contrast is extended by the contrast type
#'   \sQuote{Pairs}. Here all pair-wise comparison concerning the first entry in
#'   \code{model} are computed. Alternatively, the factor which levels should
#'   determine the pairs can be addressed directly with, for example
#'   \code{linfct = c(Pairs.Well = 1)}.
#'   The Dunnett-type contrast has the special feature, that users are free to
#'   directly define the group which should serve as the control or base in the
#'   set of contrasts. Analogously to \code{Pairs}, the name of the level,
#'   separated from the string 'Dunnett' by any sign, can be stated.
#'   See examples below and in the Vignettes and further \code{contrMat} from
#'   the \pkg{multcomp} package.
#'   For situations where metadata have non-syntactic names, special signs are
#'   exchanged against dots. When applying \code{linfct = c(Pairs = 1)} or
#'   \code{linfct = c(Dunnett = 1)} with the above mentioned extension, the sign
#'   between the linfct-name and the metadata-name must not be a dot.
#'
#'   \item An object inheriting from the \sQuote{AsIs} as created by \code{I}
#'   from the \pkg{base} package. Such objects, irrespective of their class,
#'   will be converted to an argument list for and then passed to \code{mcp}
#'   from the \pkg{multcomp} package.
#'
#'   \item Other objects will be treated as a selection of factors from the data
#'   just like \code{model}, i.e. they will be converted like any
#'   \code{\link{metadata}} key (but note that character vectors would be passed
#'   to \code{glht}). If this yielded a numeric or logical vector, it would be
#'   treated as specifying the positions of factors within \code{model}. If
#'   names were present, they would be used as the values of the arguments
#'   passed to \code{mcp}. Otherwise \code{opm_opt("contrast.type")} would be
#'   used. (See the \code{type} argument of \code{contrMat}.) The modified
#'   object would then be used as the argument list in a call to \code{mcp}.
#'
#'   }
#'   After the conversions, if any, this argument is passed to \code{glht} as
#'   \code{linfct} argument.
#'
#'   If the \code{output} argument is set to \sQuote{linfct}, the function
#'   returns the converted \code{linfct} argument, which is useful in exploring
#'   the conducted conversions before running the proper tests.
#'
#' @param m.type Character scalar indicating which of the following model types
#'   to use in model fitting: \sQuote{glm}, \sQuote{aov} or \sQuote{lm}. See
#'   the eponymous functions in the \pkg{stats} package for details.
#'
#' @param glht.args List of additional arguments for the multiple comparison
#'   procedure passed to \code{glht}. See \code{glht} in the \pkg{multcomp}
#'   package for details.
#'
#' @param ops Character vector. \code{ops} is ignored if \code{model} is
#'   directly provided as a formula. Otherwise the provided list or character
#'   vector is converted to formula, and \code{ops} then specifies the
#'   operator(s) to insert between the variables in the right part of the
#'   \code{model} formula. Thus, \code{ops} should contain \sQuote{+},
#'   \sQuote{*}, or \sQuote{:} as elements. \sQuote{+} is the default, and the
#'   elements are recycled as usual if necessary. See the description of
#'   \code{formula} for further details (in the \pkg{stats} package).
#'
#' @param output Character scalar determining the main output mode. See below
#'   under \sQuote{Value}.
#'
#' @param split.at Character vector. See \code{\link{extract}}. Cannot be set in
#'   the case of the \code{\link{OPMS}} method.
#'
#' @param ... Optional argument passed to \code{\link{extract}}.
#'
#' @return The kind of object returned by this function are determined by the
#'   \code{output} argument: \describe{
#'     \item{mcp}{The default. An object of class \sQuote{glht} as the result of
#'     the multiple comparison of means. Methods for \code{print},
#'     \code{summary}, \code{confint}, \code{coef} and \code{vcov} are available
#'     for this class. See \code{glht} in the \pkg{multcomp} package for
#'     details.}
#'     \item{data}{Reshaped (\sQuote{flattened}) data frame. It contains one
#'     column for the measured values, one factorial variable determining the
#'     well, one factorial variable for the curve parameter (see
#'     \code{\link{param_names}}) and additional factorial variables selected by
#'     \code{model} as factors. The column names are converted to syntactical
#'     names. Such a data frame might be of use for
#'     model-building approaches not covered by this function.}
#'     \item{model}{The \code{model} argument \emph{after} the conversions
#'     conducted by \code{opm_mcp}, if any.}
#'     \item{linfct}{The \code{linfct} argument \emph{after} the conversions
#'     conducted by \code{opm_mcp}, if any.}
#'     \item{contrast}{A list of contrast matrices as specified by \code{model}
#'     and \code{linfct}. As these matrices would be guaranteed to fit to
#'     \code{object}, they could serve as template matrices to be further
#'     specified by the user. Note that this only works if \code{linfct} either
#'     is an object of class \sQuote{mcp} or convertible to such an object,
#'     and if its values are character scalars that can be passed as \code{type}
#'     argument to \code{contrMat} from the \pkg{multcomp} package.}
#'   }
#'
#' @keywords htest
#' @export
#'
#' @author Lea A.I. Vaas, Markus Goeker
#' @family multcomp-functions
#' @seealso multcomp::glht multcomp::contrMat stats::lm stats::formula
#'
#' @details This function internally reshapes the data in \code{object} into a
#'   \sQuote{flat} data frame the structure of which is described under
#'   \sQuote{value}. In the default \code{output} mode, \code{glht} from the
#'   \pkg{multcomp} package is applied to this data frame. This causes (general
#'   linear) models and, by indicating a contrast type, user-defined
#'   simultaneous multiple testing procedures to be inferred.
#'
#'   Since either the user or this function itself makes use of \code{mcp}, we
#'   refer to the \sQuote{Details} section of the \code{glht} function. The
#'   \code{mcp} function must be used with care when defining parameters of
#'   interest in two-way \acronym{ANOVA} or \acronym{ANCOVA} models. The
#'   definition of treatment differences might be problem-specific. An automated
#'   determination of the parameters of interest would be impossible and thus
#'   only comparisons for the main effects (ignoring covariates and
#'   interactions) would be generated and a warning issued.
#'
#' @examples
#'
#' # helper function for plotting with better suitable margins
#' plot_with_margin <- function(x, mar, ...) {
#'   old.mar <- par(mar = mar)
#'   on.exit(par(old.mar)) # tidy up
#'   plot(x, ...)
#' }
#' do.plot <- FALSE # change this to see the plots
#'
#' ## OPMS method
#'
#' # return the intermediary data frame, do not conduct statistical tests
#' head(x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   output = "data"))
#' stopifnot(is.data.frame(x), dim(x) == c(384, 5))
#'
#' # watch the converted 'model' argument
#' (x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   output = "model"))
#' stopifnot(inherits(x, "formula")) # left side is set automatically
#'
#' # watch the converted 'linfct' argument
#' (x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   linfct = c(Dunnett = 1), output = "linfct"))
#' stopifnot(inherits(x, "mcp"))
#'
#' # create a template contrast matrix
#' (x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   linfct = c(Dunnett = 1), output = "contrast"))
#' stopifnot(is.list(x), sapply(x, inherits, "contrMat"),
#'   names(x) == "Species") # the selection is as specified by 'linfct'
#'
#' # comparison using specified model comparing 'Species' pooled over
#' # complete plates
#' (x <- opm_mcp(vaas_4, model = list("Species"), m.type = "lm",
#'   linfct = c(Dunnett = 1))) # refers to 'Species'
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 1)
#' if (do.plot)
#'   plot_with_margin(x, c(3, 20, 3, 2), main = "Species")
#'
#' # comparison of only A01 - A04 against each other, Tukey style
#' # note that the left side of the model is set automatically
#' (x <- opm_mcp(vaas_4[, , 1:4],
#'   model = ~ Well + Species, m.type = "lm",
#'   linfct = c(Tukey = 1))) # the number refers to 'Well'
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 6)
#' if (do.plot)
#'   plot_with_margin(x, c(3, 18, 3, 2), main = "Tukey, A01 - A04")
#'
#' # Dunnett-type comparison of selected wells
#' (x <- opm_mcp(vaas_4[, , 1:4], model = ~ Well,
#'   m.type = "lm", linfct = c(Dunnett = 1)))
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 3)
#' if (do.plot)
#'   plot_with_margin(x, c(3, 20, 3, 2), main = "Dunnett, A01 vs. A02 - A04")
#' # by default 'Dunnett' uses first level as reference
#'
#' # Dunnett-type comparison with selected control-group
#' (x <- opm_mcp(vaas_4[, , 1:5], output = "mcp", model = ~ Well,
#'   linfct = c(`Dunnett.A05 (D-Cellobiose)` = 1)))
#' if (do.plot)
#'   plot_with_margin(x, c(3, 20, 3, 2), main = "Dunnett, vs. A05")
#'
#' # manually defined contrast matrix
#' (contr <- opm_mcp(vaas_4[, , 1:4], linfct = c(Tukey = 1),
#'   model = ~ Well, output = "contrast")) # create template, Tukey style
#' contr <- contr$Well[c(1:3, 6), ] # select comparisons of interest
#' (x <- opm_mcp(vaas_4[, , 1:4],
#'   model = ~ Well, m.type = "lm", linfct = contr)) # run tests
#' if (do.plot)
#'   plot_with_margin(x, c(3, 20, 3, 2), main = "My own contrasts")
#'
#' # joining of selected metadata using pseudofunction J
#' (x <- opm_mcp(vaas_4[, , 1:4], model = ~ J(Well + Species),
#'   linfct = c(Dunnett = 1), full = FALSE)) # use short well names
#' if (do.plot)
#'   plot_with_margin(x, c(3, 22, 3, 2), main = "Dunnett, Well/Species joined")
#'
#' # comparing wells pairwise regarding the tested species
#' (x <- opm_mcp(vaas_4[, , 1:4], model = ~ J(Well + Species),
#'   linfct = c(Pairs.Well = 1), full = FALSE)) # use short well names
#' if (do.plot)
#'   plot_with_margin(x, c(3, 22, 3, 2),
#'     main = "Wells compared between species")
#' # i.e. 'Pairs.Well' means 'Pairs' type of comparison for each 'Well'
#' # separately within a joined factor (the first one in 'model', hence
#' # 'c(Pairs.Well = 1)', with '1' referring to the elements of 'model').
#'
#' # pairwise comparison of Species regarding the tested strains
#' xx <- c(vaas_4, vaas_4) # temporary test data
#' (x <- opm_mcp(xx[, , 1:4], model = ~ J(Strain + Species),
#'   linfct = c(Pairs.Species = 1), full = FALSE)) # use short well names
#' if (do.plot)
#'   plot_with_margin(x, c(3, 22, 3, 2),
#'     main = "Strains compared within species")
#' # i.e. 'Pairs.Species' means 'Pairs' type of comparison for each 'Species'
#' # separately within a joined factor (the first one in 'model', hence
#' # 'c(Pairs.Species = 1)', with '1' referring to the elements of 'model').
#'
#' ## one could check the number of calculated tests as follows:
#' #if (nrow(confint(result)$confint) > 20L)
#' #  warning("number of performed comparisons exceeds 20")
#'
#' ## data-frame method (usually unnecessary to directly apply it)
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
#'   dataframe = TRUE)
#'
#' # without the tests, returning the converted data frame
#' head(y <- opm_mcp(x, output = "data", model = list("Species", "Strain")))
#' stopifnot(is.data.frame(y), dim(y) == c(384, 5)) # same result as above
#'
#' # now with conducting the tests
#' (y <- opm_mcp(x, model = "Species", m.type = "lm",
#'   linfct = c(Dunnett = 1)))
#' stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
#' if (do.plot)
#'   plot_with_margin(y, c(3, 20, 3, 2), main = "Species (from data frame)")
#'
#' # testing for subsets of object
#' (y <- opm_mcp(subset(x, x$Species == "Escherichia coli"),
#'   linfct = c(Dunnett = 1), model = "Strain", m.type = "lm"))
#' stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
#' if (do.plot)
#'   plot_with_margin(y, c(3, 15, 3, 2), main = "Dunnett (from data frame)")
#'
setGeneric("opm_mcp",
  function(object, ...) standardGeneric("opm_mcp"))

setMethod("opm_mcp", OPMS, function(object, model, linfct = 1L,
    m.type = "glm", glht.args = list(), ops = "+", output = "mcp", ...) {
  annotation <- list(plate.type = plate_type(object))
  object <- extract(object = object, dataframe = TRUE, ...,
    as.labels = metadata_key(model, FALSE, ops = ops, syntactic = FALSE,
      remove = RESERVED_NAMES[c("well", "value", "parameter")]))
  attr(object, opm_string()) <- annotation
  opm_mcp(object = object, model = model, linfct = linfct, ops = ops,
    m.type = m.type, split.at = param_names("split.at"), glht.args = glht.args,
    output = output)
}, sealed = SEALED)

setMethod("opm_mcp", "data.frame", function(object, model, linfct = 1L,
    m.type = c("glm", "lm", "aov"), glht.args = list(), ops = "+",
    output = c("mcp", "data", "model", "linfct", "contrast"),
    split.at = param_names("split.at")) {

  ## helper functions

  convert_model <- function(model, ops) {
    enforce_left_side <- function(f) {
      if (length(f) < 3L) # f must be a formula
        f[[3L]] <- f[[2L]]
      f[[2L]] <- as.name(RESERVED_NAMES[["value"]])
      f
    }
    enforce_left_side(metadata_key(model, TRUE, ops = ops, syntactic = TRUE))
  }

  # Generate all pairs of factor levels for a given data column, considering
  # column joining if applicable. Resulting character vector can be passed to
  # multcomp::mcp().
  level_pairs <- function(spec, column, data) {
    spec_to_column_names <- function(spec, joined, column) {
      if (nchar(spec) < 7L)
        spec <- "1"
      else
        spec <- unlist(strsplit(spec, substr(spec, 6L, 6L), TRUE))[-1L]
      if (!all(grepl("^\\d+$", spec, FALSE, TRUE)))
        return(spec)
      spec <- as.integer(spec)
      if (is.null(joined)) # TODO: this would never yield pairs at the moment
        joined <- as.list(structure(column, names = column))
      joined[[column]][spec]
    }
    pair_indices <- function(x) {
      last <- length(nums <- seq_along(x))
      do.call(rbind, lapply(nums[-last],
        FUN = function(j) cbind(I = seq.int(j + 1L, last), J = j)))
    }
    all_pairs <- function(x) {
      idx <- pair_indices(x <- unique.default(x))
      sprintf("`%s` - `%s` == 0L", x[idx[, 1L]], x[idx[, 2L]])
    }
    spec <- spec_to_column_names(spec, attr(data, "joined.columns"), column)
    groups <- split(as.character(data[, column]), data[, spec])
    result <- unlist(lapply(groups, all_pairs))
    if (!length(result))
      stop("no pairs found -- are selected factors constant?")
    result
  }

  # Create a Dunnett contrast matrix using 'level' as base, which is hopefully
  # found in the 'column' of 'data'.
  #
  dunnett_with_base <- function(data, column, level) {
    f <- as.factor(data[, column])
    if (grepl("^\\d+$", level, FALSE, TRUE)) {
      base <- as.integer(level)
      if (base > length(levels(f)))
        stop(sprintf("level no. %i does not exist", base))
    } else {
      base <- match(level, levels(f), nomatch = 0L)
      if (!base)
        stop(sprintf("level '%s' does not exist", level))
    }
    multcomp::contrMat(c(table(f)), "Dunnett", base)
  }

  # Convert the 'linfct' argument into its final form. 'model' is needed when
  # getting column names from it if given as positions within the model, 'data'
  # is necessary when computing on factor levels.
  #
  convert_hypothesis_spec <- function(linfct, model, data) {
    if (!length(linfct))
      stop("hypothesis definition 'linfct' must not be empty")
    # note that the glht() methods actually dispatch over 'linfct'...
    used.by.glht <- c("mcp", "matrix", "expression", "character", "means")
    if (inherits(linfct, used.by.glht))
      return(linfct)
    if (inherits(linfct, "AsIs")) # created using I()
      return(do.call(multcomp::mcp, as.list(linfct)))
    linfct <- metadata_key(linfct, FALSE)
    if (is.list(linfct))
      result <- names(linfct)
    else if (is.numeric(linfct) || is.logical(linfct)) {
      result <- names(metadata_key(model, FALSE))[linfct]
      names(result) <- names(linfct)
    } else if (is.character(linfct))
      result <- linfct
    else
      stop("invalid object passed as 'linfct' argument")
    if (is.null(names(result)))
      names(result) <- rep(get("contrast.type", OPM_OPTIONS),
        length.out = length(result))
    # At this stage we have a character vector with contrast types as names and
    # column names as values. Names and values are now swapped and then passed
    # as list to multcomp::mcp().
    result <- as.list(structure(names(result), names = result))
    # Special treatments for special contrast types must be done here.
    if (any(convert <- grepl("^Pairs", result, FALSE, TRUE)))
      result[convert] <- mapply(FUN = level_pairs, spec = result[convert],
        column = names(result)[convert], MoreArgs = list(data = data),
        SIMPLIFY = FALSE, USE.NAMES = FALSE)
    if (any(convert <- grepl("^Dunnett..+", result, FALSE, TRUE)))
      result[convert] <- mapply(FUN = dunnett_with_base,
        level = substr(result, 9L, nchar(result))[convert],
        column = names(result)[convert], MoreArgs = list(data = data),
        SIMPLIFY = FALSE, USE.NAMES = FALSE)
    do.call(multcomp::mcp, result)
  }

  convert_data <- function(object, split.at, model) {
    param.pos <- assert_splittable_matrix(object, split.at)
    # create reshaped data frame and set temporary helper column '_ID' to avoid
    # non-unique values when setting 'row.names'; note according shift of column
    # positions!
    object <- reshape(cbind(`_ID` = seq_len(nrow(object)), object),
      direction = "long",
      idvar = c("_ID", colnames(object)[seq_len(param.pos - 1L)]),
      varying = colnames(object)[seq.int(param.pos + 1L, ncol(object))],
      v.names = RESERVED_NAMES[["value"]],
      timevar = RESERVED_NAMES[["well"]],
      times = colnames(object)[seq.int(param.pos + 1L, ncol(object))])
    rownames(object) <- NULL
    object[, RESERVED_NAMES[["well"]]] <- as.factor(
      object[, RESERVED_NAMES[["well"]]])
    object$`_ID` <- NULL
    # the next step would combine the columns that have not yet been combined
    if (is.list(attr(model, "combine")))
      object <- extract_columns(object, attr(model, "combine"), direct = TRUE)
    if (!is.null(joined <- attr(object, "joined.columns"))) {
      names(joined) <- make.names(names(joined))
      joined <- lapply(joined, make.names)
    }
    colnames(object) <- make.names(colnames(object))
    object
  }
  contrast_matrices <- function(data, linfct, model) {
    linfct <- convert_hypothesis_spec(linfct, model, data)
    if (!inherits(linfct, "mcp"))
      stop("in 'contrast' mode, 'linfct' must yield an object of class 'mcp'")
    n <- lapply(data[, names(linfct), drop = FALSE], table)
    mapply(multcomp::contrMat, n = n, type = linfct, SIMPLIFY = FALSE)
  }

  # conversions and early returns, if requested
  model <- convert_model(model, ops)
  case(match.arg(output),
    data = return(convert_data(object, split.at, model)),
    model = return(model),
    linfct = return(convert_hypothesis_spec(linfct, model,
      convert_data(object, split.at, model))),
    contrast = return(contrast_matrices(convert_data(object, split.at, model),
      linfct, model)),
    mcp = NULL
  )

  annotation <- attr(object, opm_string())
  object <- convert_data(object, split.at, model)
  linfct <- convert_hypothesis_spec(linfct, model, object)

  # necessary at this stage because otherwise glht() does not find its
  # dependencies
  if (!suppressWarnings(suppressPackageStartupMessages(require(
      multcomp, quietly = TRUE, warn.conflicts = FALSE))))
    stop("package 'multcomp' must be available to run this function")

  # fit the linear model according to 'm.type', then run glht()
  model <- do.call(match.arg(m.type), list(formula = model, data = object))
  glht.args <- c(list(model = model, linfct = linfct), as.list(glht.args))
  result <- do.call(glht, glht.args)

  attr(result, opm_string()) <- annotation
  result
}, sealed = SEALED)


################################################################################


#' Create vector or matrix with substrate annotation
#'
#' These methods create vectors or matrices that include numeric values
#' (selected parameter estimates or \code{\link{opm_mcp}} results) as well as
#' an annotation of the according substrates.
#'
#' @param object An object of the classes \code{glht} as created by
#'   \code{\link{opm_mcp}} (other objects of that class are unlikely to work),
#'   \code{\link{OPMA}} or \code{\link{OPMS}}.
#' @param what Character scalar indicating the kind of annotation to use. Passed
#'   as eponymous argument to \code{\link{substrate_info}}.
#' @param how Character scalar. Indicating how the annotation is inserted.
#'   Currently \sQuote{ids} and \sQuote{values} are supported. See below for
#'   details.
#' @param output For the \code{\link{OPMA}} and \code{\link{OPMS}} methods, the
#'   estimated parameter of interest (see \code{\link{param_names}}). For the
#'   \code{glht} method, either a numeric scalar or one of the following
#'   character scalars:
#'   \describe{
#'     \item{numeric}{Return the coefficients.}
#'     \item{different}{Return -1, 1 or 0 indicating whether the coefficients
#'     are significantly smaller or larger than, or insignificantly different
#'     from the cutoff given by \code{opm_opt("threshold")}. This is calculated
#'     from the confidence intervals stored in \code{object}.}
#'     \item{smaller}{Return 1 or 0 indicating whether or not the coefficients
#'     are significantly smaller than the default cutoff.}
#'     \item{larger}{Return 1 or 0 indicating whether or not the coefficients
#'     are significantly larger than the default cutoff.}
#'     \item{equal}{Return 1 or 0 indicating whether or not the coefficients
#'     are insignificantly different from the default cutoff.}
#'   }
#'   Alternatively, character scalars such as \code{!75.0}, \code{<100},
#'   \code{>150} or \code{=85.0} can be provided, with the first character
#'   translated to the corresponding meaning in the list above and the remaining
#'   string translated to the cutoff to be used. If a numeric scalar is
#'   provided, it is used as cutoff in conjunction with the \sQuote{different}
#'   mode described above.
#' @return For \code{how = "ids"}, a numeric or logical vector whose names
#'   are the IDs of the respective substrates in the database as chosen by
#'   \code{what}.
#'
#'   For \code{how = "values"}, a numeric matrix containing the chosen computed
#'   values together with data obtained via web service associated with the
#'   chosen database, in analogy to the \code{download} argument of
#'   \code{\link{substrate_info}}.
#'
#' @family multcomp-functions
#' @keywords htest
#' @export
#' @examples
#' ## TODO
#'
setGeneric("annotated", function(object, ...) standardGeneric("annotated"))

setMethod("annotated", "OPMA", function(object, what = "kegg", how = "ids",
    output = opm_opt("curve.param")) {
  stop(NOT_YET)
}, sealed = SEALED)

setMethod("annotated", "OPMD", function(object, what = "kegg", how = "ids",
    output = opm_opt("curve.param")) {
  stop(NOT_YET)
}, sealed = SEALED)

setMethod("annotated", "OPMS", function(object, what = "kegg", how = "ids",
    output = opm_opt("curve.param")) {
  stop(NOT_YET)
}, sealed = SEALED)

setOldClass("glht")

setMethod("annotated", "glht", function(object, what = "kegg", how = "ids",
    output = "numeric") {
  get_match <- function(i, matched, string) {
    start <- attr(matched, "capture.start")[, i]
    substring(string, start, start + attr(matched, "capture.length")[, i] - 1L)
  }
  all_matches <- function(matched, string) {
    do.call(cbind, lapply(seq.int(ncol(attr(matched, "capture.start"))),
      get_match, matched, string))
  }
  names_to_ids <- function(x, what, plate) {
    p <- c(
      "`[A-Z]\\d{2}\\s+(?:\\(([^)]+)\\)|\\[([^\\]]+)\\])(?:[^`]+)?`",
      "`(?:[^`]+?)?[A-Z]\\d{2}\\s+(?:\\(([^)]+)\\)|\\[([^\\]]+)\\])`",
      "`(?:([A-Z]\\d{2})(?:[^`]+)?|(?:[^`]+?)?([A-Z]\\d{2}))`"
    )
    p <- paste0("^", p, "\\s+-\\s+", p, "$")
    s <- NULL
    for (i in seq_along(p))
      if (all(attr(m <- regexpr(p[i], x, FALSE, TRUE), "match.length") > 0L)) {
        s <- all_matches(m, x)
        s <- s[, !is_constant(s, 2L), drop = FALSE]
        if (i %in% c(3L)) {
          if (!length(plate))
            stop("need special 'glht' object with PM plate annotation")
          s[] <- wells(as.vector(s), TRUE, FALSE, plate = plate)
        }
        break
      }
    if (length(s))
      s <- s[, 1L]
    else
      s <- rep.int(NA_character_, length(x))
    substrate_info(s, what)
  }
  different <- function(x, cutoff, how) {
    structure(case(how,
      different = ifelse(x[, "lwr"] > cutoff, 1L,
        ifelse(x[, "upr"] < cutoff, -1L, 0L)),
      equal = as.integer(cutoff > x[, "lwr"] & cutoff < x[, "upr"]),
      larger = as.integer(x[, "lwr"] > cutoff),
      smaller = as.integer(x[, "upr"] < cutoff)
    ), names = rownames(x), test = how, cutoff = cutoff)
  }
  coef_to_ids <- function(x, what, plate)
    structure(x, names = coef_names_to_ids(names(x), what, plate))
  if (is.numeric(L(output))) {
    cutoff <- output
    output <- "different"
  } else if (grepl("^[!=<>]", output, FALSE, TRUE)) {
    cutoff <- must(as.numeric(substr(output, 2L, nchar(output))))
    output <- c(`!` = "different", `=` = "equal", `<` = "smaller",
      `>` = "larger")[[substr(output, 1L, 1L)]]
  } else {
    output <- tolower(output)
    cutoff <- get("threshold", OPM_OPTIONS)
  }
  result <- switch(output, numeric = coef(object),
    different(confint(object)$confint, cutoff, output))
  names(result) <- names_to_ids(names(result), what,
    attr(object, opm_string())$plate.type)
  case(how, ids = result, values = stop(NOT_YET))
}, sealed = SEALED)


################################################################################


