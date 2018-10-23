set_spline_options <- function(type = c("tp.spline",
    "p.spline", "smooth.spline"),
    knots = NULL, gamma = 1, est.method = c("REML", "ML", "GCV"), s.par = NULL,
    correlation = NULL, save.models = FALSE, filename = NULL, ...) {

  if (!missing(...))
    warning(sQuote("..."), " currently not passed to fitting functions")
  type <- match.arg(type)
  class <- ifelse(type == "tp.spline", "tp",
    ifelse(type == "p.spline", "psp", "smooth.spline"))

  method <- match.arg(est.method)
  if (est.method == "ML" && is.null(correlation))
    stop(sQuote(paste0("est.method = ", dQuote("ML"))), " can only be used if ",
      sQuote("correlation"), " is specified")
  if (est.method == "GCV" && !is.null(correlation))
    stop(sQuote(paste0("est.method = ", dQuote("GCV"))),
      " can only be used if no ", sQuote("correlation"), " is specified")
  if (type == "smoothing-splines" && !is.null(s.par))
    warning(sQuote("s.par"), " ignored if ",
      sQuote('type = "smoothing-splines"'))
  if (!is.null(filename) && !save.models) {
    save.models <- TRUE
    warning(sQuote("filename"), " specified, ", sQuote("save.models"),
      " set to TRUE")
  }
  list(type = type, knots = knots, gamma = gamma, est.method = method,
    s.par = s.par, correlation = correlation, save.models = save.models,
    filename = filename, class = class, ...)
}

setGeneric("do_aggr", function(object, ...) standardGeneric("do_aggr"))

setMethod("do_aggr", "OPM", function(object, boot = 0L, verbose = FALSE,
    cores = 1L, options = if (identical(method, "splines"))
      set_spline_options()
    else
      list(), method = "splines", plain = FALSE, logt0 = FALSE) {

  # Convert to OPMA
  integrate_in_opma <- function(object, result) {
    items <- c(METHOD, OPTIONS, SOFTWARE, VERSION)
    settings <- attributes(result)[items]
    for (item in items)
      attr(result, item) <- NULL
    new(Class = "OPMA", measurements = measurements(object),
      metadata = metadata(object), csv_data = csv_data(object),
      aggregated = result, aggr_settings = settings)
  }

  # Add our own changes of the default
  # make_grofit_control <- function(verbose, boot, add) {
  #   result <- grofit.control()
  #   orig.class <- class(result)
  #   result <- insert(unclass(result), interactive = FALSE,
  #     suppress.messages = !verbose, fit.opt = "s", nboot.gc = boot,
  #     .force = TRUE)
  #   result <- insert(result, as.list(add), .force = TRUE)
  #   class(result) <- orig.class
  #   result
  # }

  # run_grofit <- function(time, data, control) {
  #   extract_curve_params(grofit(time = time, data = data,
  #     ec50 = FALSE, control = control))
  # }

  run_mgcv <- function(x, y, data, options, boot) {
    mod <- fit_spline(y = y, x = x, data = data, options = options)
    if (boot > 0L) {
      ## draw bootstrap sample
      folds <- rmultinom(boot, nrow(data), rep(1 / nrow(data), nrow(data)))
      res <- lapply(seq_len(boot),
        function(i) {
          fit_spline(y = y, x = x, data = data, options = options,
            weights = folds[, i])
      })
      class(res) <- "splines_bootstrap"
      params <- as.vector(summary(res))
      return(list(params = params, model = mod, bootstrap = res))
    }
    list(params = extract_curve_params(mod), model = mod)
  }

  copy_A_param <- function(x) {
    map <- unlist(map_param_names(opm.fast = TRUE))
    result <- matrix(data = NA_real_, nrow = length(map), ncol = length(x),
      dimnames = list(unname(map), names(x)))
    result[map[["A.point.est"]], ] <- x
    result
  }

  if (anyDuplicated.default(hours(object, "all")))
    warning("duplicate time points are present, which makes no sense")

  if (L(cores) <= 0L) {
    cores <- detectCores() + cores
    if (cores <= 0L)
      stop("attempt to use <1 computational core")
  }

  if (dim(object)[1L] < 2L && (plate_type(object) %in% SPECIAL_PLATES ||
      custom_plate_is(plate_type(object)))) {

    result <- copy_A_param(well(object))
    attr(result, METHOD) <- "shortcut"
    attr(result, OPTIONS) <- list(boot = boot,
      preceding_transformation = "none")

  } else {

    case(method <- match.arg(method, KNOWN_METHODS$aggregation),

      grofit = {
        stop("'grofit' is not currently available")
        # control <- make_grofit_control(verbose, boot, add = options)
        # grofit.time <- to_grofit_time(object)
        # grofit.data <- to_grofit_data(object, logt0)
        # result <- mclapply(X = as.list(seq_len(nrow(grofit.data))),
        #   FUN = function(row) {
        #     run_grofit(grofit.time[row, , drop = FALSE],
        #       grofit.data[row, , drop = FALSE], control)
        #   }, mc.cores = cores)
        # result <- do.call(cbind, result)
        # attr(result, OPTIONS) <- unclass(control)
      },

      `opm-fast` = {
        options <- insert(as.list(options), boot = boot, .force = FALSE)
        mat <- measurements(object, , logt0)
        result <- rbind(
          do.call(do_aggr, c(list(object = mat, what = "AUC"), options)),
          do.call(do_aggr, c(list(object = mat, what = "A"), options)),
          matrix(nrow = 6L, ncol = ncol(mat) - 1L, data = NA_real_)
        )
        rownames(result)[7L:9L] <- sub("^[^.]+", "lambda",
          rownames(result)[1L:3L], FALSE, TRUE)
        rownames(result)[10L:12L] <- sub("^[^.]+", "mu",
          rownames(result)[1L:3L], FALSE, TRUE)
        map <- map_param_names(opm.fast = TRUE)
        result <- result[names(map), , drop = FALSE]
        rownames(result) <- as.character(map)
        attr(result, OPTIONS) <- options
      },

      splines = {
        ## extract data
        data <- as.data.frame(measurements(object, , logt0))
        ## get well names
        wells <- wells(object)
        indx <- as.list(seq_len(length(wells)))
        result <- mclapply(X = indx,
          FUN = function(i) {
            run_mgcv(x = HOUR, y = wells[i], data = data, options = options,
              boot = boot)
          }, mc.cores = cores)
        options <- insert(as.list(options), boot = boot)

        if (options$save.models) {
            opm_models <- lapply(result, `[[`, "model")
            if (boot > 0L) {
              opm_bootstrap <- lapply(result, `[[`, "bootstrap")
            } else {
              opm_bootstrap <- NA
            }
            names(opm_models) <- wells
            class(opm_models) <- "opm_models"
            if (is.null(options$filename))
              options$filename <- paste0("opm_models_",
                format(Sys.time(), "%Y-%m-%d_%H:%M:%S"), ".RData")
            save("opm_models", "opm_bootstrap", file = options$filename)
            cat("Models saved as 'opm_models' on disk in file\n  ",
              getwd(), "/", options$filename, "\n\n", sep = "")
        }
        result <- sapply(result, `[[`, "params")
        rn <- rownames(result)
        result <- matrix(unlist(result),
          ncol = ncol(result), nrow = nrow(result))
        rownames(result) <- rn
        ## attach bootstrap CIs if necessary
        if (boot <= 0L)
          result <- rbind(result, matrix(NA, nrow = 8L, ncol = ncol(result)))
        ## dirty hack:
        map <- map_param_names(opm.fast = TRUE)
        rownames(result) <- as.character(map)
        colnames(result) <- wells
        attr(result, OPTIONS) <- unclass(options)
      }

    )

    attr(result, METHOD) <- method
    attr(result, OPTIONS)$preceding_transformation <- if (logt0)
        "logt0"
      else
        "none"

  }

  tmp <- opm_string(version = TRUE)
  attr(result, SOFTWARE) <- tmp[[1L]]
  attr(result, VERSION) <- tmp[[2L]]

  if (L(plain))
    return(result)
  integrate_in_opma(object, result)

}, sealed = SEALED)

setMethod("do_aggr", "OPMS", function(object, ...) {
  object@plates <- lapply(X = object@plates, FUN = do_aggr, ...)
  object
}, sealed = SEALED)

setMethod("do_aggr", "MOPMX", function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = do_aggr, ...)
  object
}, sealed = SEALED)

setMethod("do_aggr", "matrix", function(object, what = c("AUC", "A"),
    boot = 100L, ci = 0.95, as.pe = "median", ci.type = "norm",
    time.pos = 1L, transposed = FALSE, raw = FALSE, ...) {
  LL(time.pos, boot, ci, transposed, raw)
  if (transposed)
    object <- t(object)
  y <- object[, time.pos]
  object <- object[, -time.pos, drop = FALSE]
  object.colnames <- colnames(object)
  ## i arguments are required by boot
  case(what <- match.arg(what),
    A = boot_fun <- function(x, i) apply(x[i, , drop = FALSE], 2L, max),
    AUC = {
      n.obs <- nrow(object)
      y <- y[-1L] - y[-n.obs]
      object <- 0.5 * (object[-1L, , drop = FALSE] +
        object[-n.obs, , drop = FALSE])
      boot_fun <- function(x, i) colSums(x[i, , drop = FALSE] * y[i])
    }
  )
  result <- boot(data = object, statistic = boot_fun, R = boot, ...)
  if (raw)
    return(result)
  result <- pe_and_ci(result, ci = ci, as.pe = as.pe, type = ci.type,
    fill.nas = what == "A")
  colnames(result) <- object.colnames
  rownames(result) <- paste(what, rownames(result), sep = ".")
  result
}, sealed = SEALED)

