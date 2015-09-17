#' # WORKSHOP PART 4: Estimating and visualising curve parameters
#'
#' (According sections in the tutorial: 3.5. Aggregating data by estimating
#' curve parameters, 3.8. Plotting the aggregated data)
#'
#' *****************************************************************************
#'
#' ## 4.1
#' Now we load and attach the `opm` functions. Note that if a package such as
#' `opm` has already been loaded, the `library` command does nothing. It thus
#' can be called at any time.

library(pkgutils)
library(opm)

#' *BEGIN* code we only need here for users which have no PM data of their own

# Here we check whether a data object `x` has been created by reading PM data
# files. If otherwise, we assign an existing example object to it.
#
if (!exists("x") || length(x) == 0) {
  warning("data object 'x' from part 1 is missing or empty, using 'vaas_4'")
  x <- as(vaas_4, "MOPMX") # this example object comes with the opm package
  metadata(x) <- "ID" # set unique IDs
}

#' *END* code we only need here for users which have no PM data of their own
#'
#' *****************************************************************************
#'
#' ## 4.2
#' Now we aggregate the PM measurements by calculating four parameters: maximum
#' height `A`, are under the curve `AUC`, slope `mu` and lag phase `lambda`.
#'
#' Aggregate the data under default settings and store the results by
#' overwriting the data object `x`. This is possible because aggregation does
#' not remove any of the data stored in the passed object; it only adds the
#' curve parameters and the aggregation settings.

x <- do_aggr(
  object = x,
  cores = 1 # on systems other than Windows, you can use more cores up to the
            # overall number of cores of the system, to accordingly speed up
            # the parameter estimation; set this to -1 to use the total number
            # of cores on your system
)

#' The next query should now return only `TRUE` values.

has_aggr(x)

#' *****************************************************************************
#'
#' ## 4.3
#' After inferring them, the curve parameters can be plotted in various ways. In
#' the following we assume that a metadata entry 'Strain' is present. Replace
#' that by the metadata entries of interest for you.
#'
#' The first kind of plot is a parallel plot, which shows all curve parameters
#' together.

parallelplot(
  x = x, # the data object
  data = "Strain" # the metadata to use for colouring
)

#' The argument `panel.var` for creating sub-panels can access the metadata
#' selected with `data`. If no metadata were selected (the default), only 'Well'
#' would be available as grouping variable.

parallelplot(
  x = x, # the data object as above
  data = "Strain", # selected metadata as above
  panel.var = "Strain", # create subpanels according to 'Strain'
  groups = "Strain" # assign colours to curves according to 'Strain'
)


#' *****************************************************************************
#'
#' ## 4.4
#' Radial plots can also be used for the quantitative comparison of parameter
#' values. They can plot only few wells together, however.
#'
#' Here parameter `A` from the first plate is plotted for the wells `A01` to
#' `A05` plus `A10`.

radial_plot(
  object = x[[1]][, , c(1:5, 10)], # use 1st plate type and 6 of its wells
  as.labels = "Strain", # metadata to be used for colouring
  group.col = FALSE,
  x = 150, y = 200 # placement of the legend
)

#' Now the same with one colour per group.

radial_plot(
  object = x[[1]][, , c(1:5, 10)], # use 1st plate type and 6 of its wells
  as.labels = "Strain", # metadata to be used for colouring
  group.col = TRUE,
  x = 150, y = 200 # placement of the legend
)

#' Note also that the values for positioning the upper-left corner of the legend
#' are oriented according to the axes of the plot. For positioning in the lower
#' left part of the figure, negative values for `x` and `y` were necessary.
#'
#' ### Troubleshooting
#' The code above is for several plates per plate type, make sure you have them.
#' If you run out of colours, check `?select_colors` and `?rainbow` for how to
#' create more of them or use `group.col = TRUE` to for one colour per group.
#'
#' *****************************************************************************
#'
#' ## 4.5
#' The function `ci_plot` is able to visualise point estimates and corresponding
#' 95% confidence intervals for the parameters, derived via bootstrapping during
#' aggregation of raw kinetic data into curve parameters, or, in conjunction
#' with extract, from plate groups defined by metadata.

ci_plot.legend <- ci_plot(
  object = x[[1]][, , c("A01", "A02", "A03")], # use 3 wells from 1st plate type
  as.labels = "Strain", # use these metadata
  subset = "A", # use this parameter (that's the default anyway, try others)
  legend.field = NULL, # panel relative to which legend is placed, here unused
  x = 170, y = 3 # positioning of the legend
)

#' If you want to use other parameters, note that their names are given by
#' calling `param_names`.
#'
#' As we have not conducted bootstrapping during aggregation, this shows only
#' the point estimates but no confidence intervals. We can get them from the
#' replicates, however, *if* replicates are present (not for the data set
#' `vaas_4`).

ci_plot.legend <- ci_plot(
  object = extract(
    extract(object = x[[1]][, , c("A01", "A02", "A03")], # wells as above
    dataframe = TRUE, # create confidence intervals
    as.labels = "Strain")  # by grouping according to these metadata
  ),
  legend.field = NULL,
  x = 170, y = 3 # positioning of the legend
)

#' ### Troubleshooting
#' If there are no replicates per group, confidence intervals cannot be
#' calculated in that manner. This is a statistical, not a technical
#' restriction: with a single measurement only, you can't get any idea of the
#' variance in the data. We will meet this restriction again in part 5. However,
#' because a respiration kinetic contains many values, confidence intervals for
#' the parameters could be calculated without replicates. This would need to be
#' done during aggregation by bootstrapping the curves. But the approach using
#' technical or even biological replicates is expected to be more informative.
#'
#' *****************************************************************************
#'
#' ## 4.7
#' Heat maps are false-colour level plots in which both axes are optionally
#' rearranged according to clustering results. In the context of PM data, it
#' makes most sense to apply it to the estimated curve parameters.

heat_map(
  object = x[[1]], # use only this plate type (we could combine several ones)
  as.labels = "ID", # use these metadata to colour the plot
  subset = "A", # use this parameter (that's the default anyway, try others)
  as.groups = "Strain" # use these to highlight group affiliation
)

#' ### Troubleshooting
#' The widths of the left and bottom margin are not always easy to calculate
#' automatically. They can be set by hand at any time, however, using the
#' `margins` argument.
#'
#' *****************************************************************************
#'
#' Now proceed with part 5.

