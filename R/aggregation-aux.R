setGeneric("to_grofit_time",
  function(object, ...) standardGeneric("to_grofit_time"))

setMethod("to_grofit_time", "OPM", function(object) {
  tp <- hours(object, "all")
  as.data.frame(matrix(rep.int(tp, length(wells(object))), ncol = length(tp),
    byrow = TRUE))
}, sealed = SEALED)

setGeneric("to_grofit_data",
  function(object, ...) standardGeneric("to_grofit_data"))

setMethod("to_grofit_data", "OPM", function(object) {
  w <- wells(object)
  names <- matrix(nrow = length(w), ncol = 3L,
    dimnames = list(well = w, value = c("well", "plate_id", "concentration")))
  names[, 1L] <- w
  names[, 2L] <- paste(csv_data(object, what = "setup_time"),
    csv_data(object, what = "position"), collapse = "-")
  names <- as.data.frame(names, stringsAsFactors = FALSE)
  names[, 3L] <- 1L # dummy concentration
  cbind(names, as.data.frame(t(measurements(object)[, -1L, drop = FALSE])))
}, sealed = SEALED)

extract_curve_params <- function(x, ...) UseMethod("extract_curve_params")

extract_curve_params.grofit <- function(x, ...) {
  settings <- c(x$control)
  x <- summary(x$gcFit)
  map <- map_param_names()
  structure(.Data = t(as.matrix(x[, names(map)])),
    dimnames = list(map, x[, "TestId"]), settings = settings)
}

extract_curve_params.fake_opm_model <- function(x, ...) {
  as.data.frame(as.list(c(mu = NA_real_, lambda = NA_real_, A = x[[3L]],
    AUC = (x[[2L]] - x[[1L]]) * x[[3L]])))
}

extract_curve_params.opm_model <- function(x, all = FALSE, ...) {
  if (!inherits(x, "smooth.spline"))
    x <- as.gam(x)
  pred <- fitted(x)
  x <- get_data(x)[, 1L]
  ## quick and dirty
  deriv <- diff(pred) / diff(x)
  slope <- max(deriv, na.rm = TRUE)
  ## index of max. slope
  idx <- which.max(deriv):(which.max(deriv) + 1L)
  ## x-value of max. slope
  x_ms <- mean(x[idx])
  ## y-value of max. slope
  y_ms <- mean(pred[idx])
  ## intercept
  intercept <- y_ms - slope * x_ms
  ## lag
  lag <- - (intercept / slope)
  ## maximum
  maximum <- max(pred)
  ## AUC
  AUC <- AUC(x, pred)
  if (all)
      return(list(mu = slope, lambda = lag, A = maximum, AUC = AUC,
        derivative = deriv, intercept = intercept))
  return(data.frame(mu = slope, lambda = lag, A = maximum, AUC = AUC))
}

summary.splines_bootstrap <- function (object, ...) {

  cnames <- unlist(map_param_names(), use.names = FALSE)

  res <- data.frame(t(sapply(object, extract_curve_params)))
  res$mu <- unlist(res$mu)
  res$lambda <- unlist(res$lambda)
  res$A <- unlist(res$A)
  res$AUC <- unlist(res$AUC)

  mu <- mean(res$mu, na.rm = TRUE)
  lambda <- mean(res$lambda, na.rm = TRUE)
  A <- mean(res$A, na.rm = TRUE)
  AUC <- mean(res$AUC, na.rm = TRUE)
  mu.sd <- sd(res$mu, na.rm = TRUE)
  lambda.sd <- sd(res$lambda, na.rm = TRUE)
  A.sd <- sd(res$A, na.rm = TRUE)
  AUC.sd <- sd(res$AUC, na.rm = TRUE)
  table <- c(mu, lambda, A, AUC,
    mu - qnorm(0.975) * mu.sd,
    lambda - qnorm(0.975) * lambda.sd,
    A - qnorm(0.975) * A.sd,
    AUC - qnorm(0.975) * AUC.sd,
    mu + qnorm(0.975) * mu.sd,
    lambda + qnorm(0.975) * lambda.sd,
    A + qnorm(0.975) * A.sd,
    AUC + qnorm(0.975) * AUC.sd)
  table <- data.frame(t(table))
  colnames(table) <- cnames
  return(table)
}

pe_and_ci <- function(x, ...) UseMethod("pe_and_ci")

pe_and_ci.boot <- function(x, ci = 0.95, as.pe = c("median", "mean", "pe"),
    type = c("basic", "perc", "norm"), fill.nas = FALSE, ...) {
  LL(ci, fill.nas)
  as.pe <- match.arg(as.pe)
  type <- match.arg(type)
  if (nrow(x$t)) {
    cis <- lapply(X = seq_along(x$t0), FUN = boot.ci, boot.out = x, conf = ci,
      type = type, ...)
    ok <- !vapply(cis, is.null, NA)
    cis[!ok] <- list(c(NA_real_, NA_real_))
    cis[ok] <- lapply(X = cis[ok], FUN = `[[`, i = type, exact = FALSE)
    cis[ok] <- lapply(lapply(cis[ok], c), tail, 2L)
    cis <- do.call(cbind, cis)
  } else {
    if (as.pe != "pe") {
      warning("zero bootstrap replicates -- using real point estimate")
      as.pe <- "pe"
    }
    cis <- matrix(nrow = 2L, ncol = length(x$t0), data = NA_real_)
  }
  rownames(cis) <- c("ci.low", "ci.high")
  point.est <- case(as.pe,
    median = apply(x$t, 2L, median),
    mean = colMeans(x$t),
    pe = x$t0
  )
  if (fill.nas) {
    boot.nas <- !is.na(x$t0) & is.na(cis[1L, ]) & is.na(cis[2L, ])
    cis[2L, boot.nas] <- cis[1L, boot.nas] <- x$t0[boot.nas]
  }
  rbind(point.est, cis)
}

