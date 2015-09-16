#' # WORKSHOP PART 3: Visualizing raw PM data in `opm`
#'
#' (According section in the tutorial: 3.7. Plotting functions for raw data)
#'
#' *****************************************************************************
#'
#' ## 3.1
#' Load and attach the `opm` functions. Note that if `opm` has already been
#' loaded, this command does nothing. It thus can be called at any time.

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
#' ## 3.2
#' Now we create some plots, first without metadata. We can indeed plot our PM
#' plates without considering metadata, but this apparently yields not
#' particularly informative plots.
#'
#' These functions create one plot per plate type. This avoids comparing apples
#' and oranges. Wells from distinct plate types cannot can sensibly be compared
#' in most kinds of single plots.

print(xy_plot(x)) # so-called x-y plot
print(level_plot(x)) # so-called level plot


#' ### Plotting only subsets
#'
#' Code to show the dimensions of the data for the first plate type:

dim(x[[1]])

#' This yields:
#' * (1) plates, (2) time points, (3) wells if there is > 1 plate
#' * (1) time points, (2) wells if there is 1 plate
#'
#' More subsettings examples:

## The first 8 wells of the first plate type in x if it has > 1 plate
# print(xy_plot(x[[1]][, , 1:8]))

## The first 8 wells of the first plate type in x if it has 1 plate
# print(xy_plot(x[[1]][, 1:8]))

## The first 50 time points of the first plate type in x if it has > 1 plate
# print(xy_plot(x[[1]][, 1:50]))

## The first 50 time points of the first plate type in x if it has > 1 plate
# print(xy_plot(x[[1]][1:50]))

#' ### Troubleshooting
#' You can create subsets at any time, but what is allowed depends on the
#' dimensions of the data. If you select beyond the real range, an error
#' results.
#'
#' *****************************************************************************
#'
#' ## 3.3
#' A way more interesting approach is to make use of the stored metadata, if
#' any.
#'
#' You could check whether 'Strain' occurs in the metadata keys of all of the
#' plates as follows. Otherwise the plotting functions would crash if 'Strain'
#' is within the `include` argument, as below.

## if (all(unlist("Strain" %k% x))) {
## # the code would come here
## }

#' Here the proper plotting code is as in 3.3 but note the argument `include`.

print(xy_plot(x, include = "Strain"))

#' Next we make use of *two* metadata entries.

print(level_plot(x, include = list("Strain", "ID")))

#' Please watch the generated plots carefully and determine how the selected
#' metadata entry has been used. If you have other metadata entries in your own
#' data, use those.
#'
#' *****************************************************************************
#'
#' Now proceed with part 4.
