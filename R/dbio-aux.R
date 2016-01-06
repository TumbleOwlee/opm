settings_forward <- function(x, plate.id) {
  x$options <- toJSON(x$options)
  x <- x[c("software", "version", "method", "options")]
  data.frame(id = 1L, plate_id = plate.id, x, stringsAsFactors = FALSE,
    check.names = FALSE)
}

settings_backward <- function(x) {
  x <- x[, c("method", "options", "software", "version"), drop = TRUE]
  x$options <- fromJSON(x$options)
  x
}

forward_OPM_to_list <- function(from) {
  p <- data.frame(id = 1L, plate_type = plate_type(from),
    setup_time = csv_data(from, what = "setup_time", normalize = TRUE),
    position = csv_data(from, what = "position", normalize = TRUE),
    machine_id = opm_opt("machine.id"), stringsAsFactors = FALSE,
    csv_data = toJSON(csv_data(from, normalize = TRUE)), check.names = FALSE)
  if (length(md <- from@metadata)) {
    if (any(bad <- names(md) %in% colnames(p)))
      stop("use of forbidden metadata name: ", names(md)[bad][1L])
    p <- cbind(p, md, stringsAsFactors = FALSE)
  }
  w <- wells(from)
  w <- data.frame(id = seq_along(w), plate_id = 1L, coordinate = w,
    stringsAsFactors = FALSE, check.names = FALSE)
  m <- opm::flatten(object = from, numbers = TRUE)
  names(m) <- map_values(names(m), MEASUREMENT_COLUMN_MAP)
  m <- cbind(id = seq.int(nrow(m)), m[, MEASUREMENT_COLUMN_MAP])
  list(plates = p, wells = w, measurements = m)
}

backward_OPM_to_list <- function(from) {
  to_measurements <- function(m, w) {
    m[, "well_id"] <- w[match(m[, "well_id"], w[, "id"]), "coordinate"]
    m <- split.data.frame(m, m[, "well_id"]) # <= probably most time-consuming
    c(list(Hour = sort.int(m[[1]][, "time"])),
      lapply(m, function(x) x[order(x[, "time"]), "value"]))
  }
  if (nrow(p <- from@plates) != 1L)
    stop("object does not contain a single plate")
  list(measurements = to_measurements(from@measurements, from@wells),
    metadata = p[, setdiff(colnames(p), c("id", "plate_type", "setup_time",
      "position", "machine_id", "csv_data")), drop = TRUE],
    csv_data = unlist(fromJSON(p[, "csv_data"])))
}

forward_OPMA_to_list <- function(from) {
  aggr_forward <- function(x, coords) data.frame(id = seq_along(x),
    well_id = rep(match(colnames(x), coords), each = nrow(x)),
    aggr_setting_id = 1L, parameter = rownames(x), value = as.vector(x),
    check.names = FALSE, stringsAsFactors = FALSE)
  x <- forward_OPM_to_list(from)
  x$aggregated <- aggr_forward(from@aggregated, x$wells[, "coordinate"])
  x$aggr_settings <- settings_forward(from@aggr_settings, x$plates[, "id"])
  x
}

backward_OPMA_to_list <- function(from) {
  aggr_backward <- function(a, w) {
    a[, "well_id"] <- w[match(a[, "well_id"], w[, "id"]), "coordinate"]
    a <- split.data.frame(a, a[, "well_id"])
    lapply(a, function(x) structure(x[, "value"], names = x[, "parameter"]))
  }
  c(backward_OPM_to_list(from), list(
    aggr_settings = settings_backward(from@aggr_settings),
    aggregated = aggr_backward(from@aggregated, from@wells)))
}

backward_OPMD_to_list <- function(from) {
  disc_backward <- function(d, w) as.list(structure(as.logical(d[, "value"]),
    names = w[match(d[, "well_id"], w[, "id"]), "coordinate"]))
  c(backward_OPMA_to_list(from), list(
    disc_settings = settings_backward(from@disc_settings),
    discretized = disc_backward(from@discretized, from@wells)))
}

int2dbclass <- function(x) {
  paste0(case(x, "OPM", "OPMA", "OPMD"), "_DB")
}

