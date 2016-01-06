setClass("WMD",
  slots = c(metadata = "list"),
  contains = "VIRTUAL",
  sealed = SEALED
)

setClass("WMDS",
  slots = c(plates = "list"),
  contains = "VIRTUAL",
  sealed = SEALED
)

NULL

setClassUnion("WMDX", c("WMD", "WMDS"))

NULL

setClassUnion("FOE", c("formula", "expression"))

setClass("OPM",
  slots = c(measurements = "matrix", csv_data = "character"),
  contains = "WMD",
  validity = function(object) {
    errs <- c(opm_problems(object@measurements), opm_problems(object@csv_data))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass("OPMA",
  slots = c(aggregated = "matrix", aggr_settings = "list"),
  contains = "OPM",
  validity = function(object) {
    settings <- object@aggr_settings
    if (length(errs <- opma_problems(settings)))
      settings <- NULL # => no settings-based checks of the matrix
    errs <- c(errs, opma_problems(object@aggregated, object@measurements,
      settings))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass("OPMD",
  slots = c(discretized = "logical", disc_settings = "list"),
  contains = "OPMA",
  validity = function(object) {
    errs <- opmd_problems(object@disc_settings)
    errs <- c(errs, opmd_problems(object@aggregated, object@discretized,
      object@disc_settings[[c(OPTIONS, "parameter")]]))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass("OPMS",
  contains = "WMDS",
  validity = function(object) {
    if (length(errs <- opms_problems(object@plates)))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass("MOPMX",
  contains = "list",
  slots = c(names = "character"),
  prototype = prototype(names = character()),
  #prototype = structure(list(), names = character()),
  validity = function(object) {
    if (all(vapply(object@.Data, is, NA, "OPMX")))
      TRUE
    else
      "not ell elements inherit from 'OPMX'"
  }, sealed = SEALED
)

setClass("OPM_MCP_OUT",
  contains = "data.frame",
  validity = function(object) {
    errs <- NULL
    for (name in RESERVED_NAMES[c("well", "value")])
      if (!name %in% colnames(object))
        errs <- c(errs, sprintf("missing column named '%s'", name))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

NULL

setClassUnion("OPMX", c("OPM", "OPMS"))

NULL

setClassUnion("XOPMX", c("MOPMX", "OPMS", "OPM"))

NULL

setOldClass("print_easy")

setClassUnion("YAML_VIA_LIST", c("OPM", "OPMS", "print_easy"))

setClass("CMAT",
  contains = "matrix",
  validity = function(object) {
    errs <- character()
    if (is.null(rownames(object)) || anyNA(rownames(object)))
      errs <- c(errs, "missing row names")
    mode <- typeof(object)
    if (mode == "list") {
      mode <- unique.default(vapply(object, typeof, ""))
      if (length(mode) > 1L)
        errs <- c(errs, "non-uniform list elements contained")
      if (any(vapply(object, length, 0L) < 1L))
        errs <- c(errs, "empty list elements contained")
    }
    mode <- setdiff(mode, c("character", "integer", "double", "logical"))
    if (length(mode))
      errs <- c(errs, sprintf("unsupported storage mode: '%s'", mode))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setAs("OPM", "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs("OPM", "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs("OPM", "list", function(from) {
  list(metadata = metadata(from), csv_data = as.list(csv_data(from)),
    measurements = as.list(as.data.frame(measurements(from))))
})

setAs("list", "OPM", function(from) {
  convert_measurements <- function(mat) {
    mat <- must(do.call(cbind, lapply(mat, as.numeric)))
    if (length(hour.pos <- which(colnames(mat) == HOUR)) != 1L)
      stop("uninterpretable column names in list element 'measurements'")
    sorted.names <- c(colnames(mat)[hour.pos],
      sort.int(colnames(mat)[-hour.pos]))
    mat[, sorted.names, drop = FALSE]
  }
  md <- repair_na_strings.list(as.list(from$metadata), "character")
  new("OPM", csv_data = map_names(unlist(from$csv_data), rescue_dots),
    metadata = map_names(md, rescue_dots),
    measurements = convert_measurements(from$measurements))
})

setAs("OPMA", "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs("OPMA", "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs("OPMA", "list", function(from) {
  result <- as(as(from, "OPM"), "list")
  result$aggregated <- apply(aggregated(from), MARGIN = 2L, FUN = as.list)
  result$aggr_settings <- aggr_settings(from)
  result
})

setAs("list", "OPMA", function(from) {
  select_aggr <- function(x, wanted) {
    x <- repair_na_strings(lapply(x, `[`, unlist(map_param_names())))
    x <- do.call(cbind, x[wanted])
    must(mode(x) <- "numeric")
    x # should now be matrix, reduced to the known wells, parameters and CIs
  }
  x <- as(from, "OPM")
  new("OPMA", measurements = measurements(x),
    csv_data = csv_data(x), metadata = metadata(x),
    aggregated = select_aggr(from$aggregated, colnames(x@measurements)[-1L]),
    aggr_settings = update_settings_list(as.list(from$aggr_settings)))
})

setAs("OPMD", "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs("OPMD", "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs("OPMD", "list", function(from) {
  result <- as(as(from, "OPMA"), "list")
  result$discretized <- as.list(from@discretized)
  result$disc_settings <- from@disc_settings
  result
})

setAs("list", "OPMD", function(from) {
  # up to official release opm 0.10.0, the discretized curve parameter had
  # not been included in the discretization settings
  repair_missing_parameter <- function(x) {
    if (x[[SOFTWARE]] != opm_string())
      return(x)
    if (is.null(x[[c(OPTIONS, "parameter")]])) {
      warning("assuming discretized parameter is opm_opt('curve.param')")
      x[[c(OPTIONS, "parameter")]] <- opm_opt("curve.param")
    }
    x
  }
  x <- as(from, "OPMA")
  settings <- update_settings_list(as.list(from$disc_settings))
  settings <- repair_missing_parameter(settings)
  discretized <- from$discretized[colnames(x@aggregated)]
  discretized <- unlist(repair_na_strings(discretized, "logical"))
  new("OPMD", csv_data = csv_data(x), measurements = measurements(x),
    metadata = metadata(x), aggr_settings = aggr_settings(x),
    aggregated = aggregated(x), discretized = discretized,
    disc_settings = settings)
})

setAs("OPMS", "list", function(from) {
  lapply(from@plates, as, Class = "list")
})

setAs("list", "OPMS", function(from) {
  opmd.slots <- setdiff(slotNames("OPMD"), opma.slots <- slotNames("OPMA"))
  opma.slots <- setdiff(opma.slots, slotNames("OPM"))
  new("OPMS", plates = lapply(from, function(x) {
    as(x, if (all(opma.slots %in% names(x)))
      if (all(opmd.slots %in% names(x)))
        "OPMD"
      else
        "OPMA"
      else
        "OPM")
  }))
})

setAs("list", "MOPMX", function(from) {
  new("MOPMX", from) # overwritten to enforce consistency checks
})

setAs("OPMX", "MOPMX", function(from) {
  x <- new("MOPMX", list(from))
  names(x) <- plate_type(from)
  x
})

setAs("MOPMX", "OPMX", function(from) {
  if (length(from) != 1L)
    stop("conversion impossible: number of elements is not 1")
  from[[1L]]
})

setAs("matrix", "CMAT", function(from) {
  new("CMAT", from) # overwritten to enforce consistency checks
})

setClass("OPM_DB",
  contains = "DBTABLES",
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)

setClass("OPMA_DB",
  contains = "OPM_DB",
  # the superclass slots must be repeated here to enforce the ordering
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame", aggr_settings = "data.frame",
    aggregated = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer()),
    aggr_settings = data.frame(id = integer(), plate_id = integer()),
    aggregated = data.frame(id = integer(), well_id = integer(),
      aggr_setting_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)

setClass("OPMD_DB",
  contains = "OPMA_DB",
  # the superclass slots must be repeated here to enforce the ordering
  slots = c(plates = "data.frame", wells = "data.frame",
    measurements = "data.frame", aggr_settings = "data.frame",
    aggregated = "data.frame", disc_settings = "data.frame",
    discretized = "data.frame"),
  prototype = list(plates = data.frame(id = integer(), machine_id = integer()),
    wells = data.frame(id = integer(), plate_id = integer()),
    measurements = data.frame(id = integer(), well_id = integer()),
    aggr_settings = data.frame(id = integer(), plate_id = integer()),
    aggregated = data.frame(id = integer(), well_id = integer(),
      aggr_setting_id = integer()),
    disc_settings = data.frame(id = integer(), plate_id = integer()),
    discretized = data.frame(id = integer(), well_id = integer(),
      disc_setting_id = integer())),
  validity = fkeys_valid,
  sealed = SEALED
)

setAs("OPM", "OPMA", function(from) {
  stop("do_aggr() is needed to aggregate OPM objects")
})

setAs("OPM", "OPMD", function(from) {
  stop("do_aggr() and do_disc() are needed to discretise OPM objects")
})

setAs("OPMA", "OPMD", function(from) {
  stop("do_disc() is needed to discretise OPMA objects")
})

setAs("OPM", "OPM_DB", function(from) {
  x <- forward_OPM_to_list(from)
  new("OPM_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements)
})

setAs("OPM_DB", "OPM", function(from) {
  as(backward_OPM_to_list(from), "OPM")
})

setAs("OPMA", "OPMA_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  new("OPMA_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated)
})

setAs("OPMA_DB", "OPMA", function(from) {
  as(backward_OPMA_to_list(from), "OPMA")
})

setAs("OPMD", "OPMD_DB", function(from) {
  x <- forward_OPMA_to_list(from)
  d.sets <- settings_forward(from@disc_settings, x$plates[, "id"])
  d.data <- from@discretized
  storage.mode(d.data) <- "integer" # RMySQL might otherwise set all to FALSE
  d.data <- data.frame(id = seq_along(d.data), stringsAsFactors = FALSE,
    well_id = match(names(d.data), x$wells[, "coordinate"]),
    disc_setting_id = 1L, value = unname(d.data), check.names = FALSE)
  new("OPMD_DB", plates = x$plates, wells = x$wells,
    measurements = x$measurements, aggr_settings = x$aggr_settings,
    aggregated = x$aggregated, disc_settings = d.sets, discretized = d.data)
})

setAs("OPMD_DB", "OPMD", function(from) {
  as(backward_OPMD_to_list(from), "OPMD")
})

setAs("list", "OPM_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPM_DB"))
})

setAs("list", "OPMA_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPMA_DB"))
})

setAs("list", "OPMD_DB", function(from) {
  do.call(c, rapply(object = from, f = as, Class = "OPMD_DB"))
})

setAs("OPM_DB", "list", function(from) {
  lapply(split(from), as, "OPM")
})

setAs("OPMA_DB", "list", function(from) {
  lapply(split(from), as, "OPMA")
})

setAs("OPMD_DB", "list", function(from) {
  lapply(split(from), as, "OPMD")
})

setAs("OPMS", "OPM_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPM_DB"))
})

setAs("OPMS", "OPMA_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPMA_DB"))
})

setAs("OPMS", "OPMD_DB", function(from) {
  do.call(c, lapply(from@plates, as, "OPMD_DB"))
})

setAs("OPM_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPM_to_list), "OPMS")
})

setAs("OPMA_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPMA_to_list), "OPMS")
})

setAs("OPMD_DB", "OPMS", function(from) {
  as(lapply(split(from), backward_OPMD_to_list), "OPMS")
})

setAs("MOPMX", "OPM_DB", function(from) {
  do.call(c, lapply(unname(from), as, "OPM_DB"))
})

setAs("MOPMX", "OPMA_DB", function(from) {
  do.call(c, lapply(unname(from), as, "OPMA_DB"))
})

setAs("MOPMX", "OPMD_DB", function(from) {
  do.call(c, lapply(unname(from), as, "OPMD_DB"))
})

setAs("OPM_DB", "MOPMX", function(from) {
  do.call(opms, c(as(from, "list"),
    list(precomputed = TRUE, skip = FALSE, group = TRUE)))
})

