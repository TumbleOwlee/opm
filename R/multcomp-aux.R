convert_annotation_vector <- function(x, how, what, conc) {
  peptides2vector <- function(ids) {
    ids <- vapply(ids, paste0, "", collapse = "-")
    ifelse(nzchar(ids), ids, NA_character_)
  }
  create_matrix <- function(x, ids, what, conc) {
    x <- as.matrix(x)
    colnames(x) <- RESERVED_NAMES[["value"]]
    if (L(conc))
      x <- cbind(x,
        Concentration = substrate_info(rownames(x), "concentration"))
    switch(what,
      peptide = structure(cbind(x, collect(ids)),
        comment = peptides2vector(ids)),
      structure(cbind(x, collect(web_query(ids, what))), comment = ids)
    )
  }
  create_dataframe <- function(x) {
    x <- structure(as.data.frame(x), comment = comment(x))
    names(x) <- make.names(names(x))
    for (i in seq_along(x))
      if (all(x[, i] %in% c(0, 1, NA_real_)))
        x[, i] <- as.factor(as.logical(x[, i]))
    x
  }
  ids <- substrate_info(names(x), what)
  case(match.arg(how, c("ids", "values", "data.frame")),
    data.frame = create_dataframe(create_matrix(x, ids, what, conc)),
    ids = {
      switch(what, peptide = ids <- peptides2vector(ids))
      structure(x, names = ids, concentration = if (L(conc))
        unname(substrate_info(names(x), "concentration"))
      else
        NULL, comment = names(x))
    },
    values = create_matrix(x, ids, what, conc)
  )
}

check_mcp_sep <- function(sep) {
  if (!is.character(sep) || nchar(sep <- sep[[1L]]) != 1L)
    stop("'sep' must be a single character")
  sep
}

