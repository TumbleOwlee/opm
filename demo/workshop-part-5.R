#' # WORKSHOP PART 5: Statistical comparisons of experimental groups
#'
#' (According section in the tutorial: 3.9. Statistical comparisons of group
#' means)
#'
#' ## 5.1
#' Load and attach the `opm` functions. Note that if `opm` has already been
#' loaded, this command does nothing. It thus can be called at any time.

library(pkgutils)
library(opm)

#' Now we load a new package, which contains additional example data we need
#' here, as we don't know yet how your own data (regarding replicates and
#' metadata) could be used in the following analyses.

library(opmdata)

#' And another one on which the analysis functions depend.

library(multcomp)

#' ## 5.2
#' Multiple Comparisons of means: The `opm_mcp` function allows the user to test
#' for differences in the means of multiple groups directly in `OPMS` objects,
#' obtaining the factors that determine the grouping structure from the stored
#' metadata or the wells.
#'
#' As we do not know yet how your data are structured, we here switch to a
#' prepared data set. First we subset this large data set `vaas_et_al`:

vg6 <- vaas_et_al[~ Experiment == "First replicate", , "G06"]

#' This yields well `G06` from all plates that belong to the first biological
#' replicate (there are many technical replicates).
#'
#' Tukey (all-against-all) comparison of different strains and the same well.
#' The research question is: Which strains show a significant difference in the
#' respiration on well `G06`, as measured using the `A` parameter? (one could
#' test the other parameters as well, of course).

tukey.g6 <- opm_mcp(vg6,
  model = ~ Strain, # design the model for model-fitting
  m.type = "aov", # which model type to be fitted
  linfct = c(Tukey = 1) # specification of the linear hypotheses to be tested
)

#' Plot point estimates and confidence intervals of differences.

old.mar <- par(mar = c(3, 15, 3, 2)) # adapt margins in the plot
plot(tukey.g6)
par(old.mar) # reset to default plotting settings

#' Show p-values of the tests.

summary(tukey.g6)

#' Compare the p-values with the plot: The significant differences can easily
#' be recognised in the plot.
#'
#' ### Troubleshooting
#' Note that you need data sets which actually provide group structures for
#' comparisons. These would be, e.g., strains measured on the same plate type in
#' several repetitions, or one strain distinctly treated and every treatment
#' measured in several repetitions. Of course, it is also possible to compare
#' the wells between each other, when the plates are comparable.
#'
#' But with a single value per group the code below will inevitably raise an
#' error.
#'
#' *****************************************************************************
#'
#' ## 5.3
#' Multiple Comparisons: Dunnett-type comparisons of wells, i.e. one against the
#' others. (Unfortunately, we have no "all for one and one for all" type of
#' comparison.)
#'
#' Prepare data set - all plates of first rep from strain `DSM30083T`.

dsm1 <- vaas_et_al[list(Experiment = "First replicate", Strain = "DSM30083T")]

#' Compare first ten wells each against negative control. The research question
#' is: Which of the first nine wells of among the `DSM30083T` measurements show
#' a significant respiration difference from the negative-control well `A01`?

mcp.a1 <- opm_mcp(dsm1[, , 1:10],
  model = ~ Well, # design the model for model-fitting
  linfct = c(`Dunnett_A01 (Negative Control)` = 1) # specify the linear
                                                  # hypotheses to be tested
                                                  # and select control group
)

#' Plot point estimates and confidence intervals of differences.

old.mar <- par(mar = c(3, 20, 3, 2)) # adapt margins in the plot
plot(mcp.a1)
par(old.mar) # reset to default plotting settings

#' Show p-values of the tests.

summary(mcp.a1)

#' *****************************************************************************
#'
#' ## 5.4
#' Special feature: Choice of reference group in Dunnett-type comparisons.
#'
#' The value for the `linfct` argument can be constructed by typing `Dunnett`,
#' plus, separated by any sign, e.g. underscore (`_`), the level name which
#' should serve as the reference group in the contrast set.
#'
#' Dunnett-type comparison with well `A03` chosen as the reference group. The
#' research question is: Which of the first nine wells of `DSM30083T`
#' measurements show a significant respiration difference from the well `A03`?

mcp.a3 <- opm_mcp(dsm1[, , 1:10],
  model = ~ Well,  # design the model for model-fitting
  linfct = c(Dunnett_A03 = 1),  # specification of the linear
                                # hypotheses to be tested
                                # and select control group
  full = FALSE # whether full substrate names shall be used
)

#' Plot point estimates and confidence intervals of differences.

old.mar <- par(mar = c(3, 5, 3, 2)) # adapt margins in the plot
plot(mcp.a3)
par(old.mar) # reset to default plotting settings

#' Show p-values of the tests.

summary(mcp.a3)

#' There are many more types of comparisons possible, but the more complicated
#' ones also need more elaborate preparation. For ways to do this in `opm` see
#' the tutorial, section 3.9.

