#' # WORKSHOP PART 2: Adding and manipulating metadata
#'
#' (According section in the tutorial: 3.4. Integration and manipulation of
#' metadata)
#'
#' *****************************************************************************
#'
#' ## 2.1
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
#' ## 2.2
#' We here check for the availability of metadata in our data object.
#'
#' A raw representation of the metadata:

metadata(x)

#' A nicer display as data frame, which might contain gaps:

to_metadata(x)

#' The set of all metadata *entries*:

metadata_chars(x, values = TRUE)

#' The set of all metadata *keys*:

metadata_chars(x, values = FALSE)

#' ### Notes
#' Metadata are not set automatically after reading CSV files. The reason is
#' that metainformation from CSV files is usually limited and potentially
#' inconsistent or erroneous, depending on what has been entered at an OmniLog
#' instrument. It is possible with one line of code, which we show below to set
#' everything in `csv_data` as metadata, but whether this makes sense depends on
#' the data. The default mechanism adds metadata later on. Note that LIMS format
#' yielded more metadata from the beginning.
#'
#' *****************************************************************************
#'
#' ## 2.3
#' The most general approach for entering metadata is to generate a template
#' (file), add information within `R` or with an external editor, and then
#' assign this information into the data object, using the plate identifiers
#' stored within the template (file).
#'
#' As an example, we target the metadata entry 'Strain' to either be added
#' manually or computed from `csv_data` entries. We do nothing if it is already
#' contained.
#'
#' This queries the metadata for the presence of a key named 'Strain'. You not
#' normally need that in you code, but we don't know your metadata yet.

if (!all(unlist("Strain" %k% x))) { # Metadata of all plates contain 'Strain'?
  # This creates a metadata template file with selected entries.
  collect_template(
    object = x, # take the 'CSV data' from our data object 'x'
    outfile = "template.csv", # place them in that file
    previous = NULL, # ignore existing file, if any
    selection = c(
      opm_opt("csv.selection"), # include the plate identifiers
      grep(pattern = "Strain", # and all 'CSV data' entries with 'Strain'
        x = colnames(csv_data(x)), value = TRUE)
    ),
    add.cols = "Strain" # add column named 'Strain' with empty values to fill in
  )
}

#' You should now edit the file 'template.csv' with an editor or a spreadsheet
#' program, enter the values of interest and save it again, without destroying
#' format. In Excel, you might need to use `Data -> Text to columns`, using
#' tabulator as column separator and setting all columns to data type "Text".

if (!all(unlist("Strain" %k% x))) { # Metadata of all plates contain 'Strain'?
  # Now we add the information from that file. This has not much use as long as
  # we have not filled in our target column 'Strain'.
  x <- include_metadata(
    object = x, # overwrite existing data object: nothing gets lost
    md = "template.csv" # read from that file
  )
}

#' ### Troubleshooting
#' `include_metadata` must correctly and uniquely identify plates to correctly
#' assign the metadata. This cannot work if the identifiers get modified after
#' exporting them! Potential causes for key-value mismatches mismatches are:
#'
#' * Excel etc. re-interpreting date-time entries; you must set the data type of
#'   all columns to text in Excel to prevent this.
#' * Leading spaces in plate identifiers; the current default for the
#'   `strip.white` has several values that are tried in turn as a remedy.
#' * Excel etc. saving the file with another column separator; the current
#'   default for the `sep` argument tries several values in turn as a remedy.
#'
#' Instead of using external software, you could also edit the metadata directly
#' in `R`. Try:

# x <- edit(x)

#' If 'Strain' still has not been set but is represented as `NA` (the
#' placeholder in `R` for data that are Not Available), we set it to 'unknown'.
#' The next code snippet shows metadata manipulation using formulas that contain
#' instructions to be applied to metadata entries. Symbols in these formulas
#' refer to keys in the metadata.

metadata(x) <- Strain ~ if (is.na(Strain)) "unknown" else Strain

#' In this way you can conduct any kinds of computations within the metadata
#' themselves.
#'
#' Adding just a plate ID to each plate that is unique for the current session
#' is easy using the following shortcut:

if (!any(unlist(x %k% "ID"))) { # if there is no ID entry at all, we set it
  metadata(x) <- "ID"
}

#' Using data frames, it is also possible to add metadata *if* the data frame
#' has as many rows as we have plates in the PM data object. Note that in
#' contrast to `include_metadata` here plates are identified just by their
#' position in the data object.
#'
#' Setting certain `csv_data` entries as metadata would work as follows:

## appending
# metadata(x, 1) <- as.data.frame(csv_data(x,
#   c("Strain Name", "Strain Number")))
## prepending
# metadata(x, -1) <- as.data.frame(csv_data(x,
#   c("Strain Name", "Strain Number")))
## replacing
# metadata(x) <- as.data.frame(csv_data(x, c("Strain Name", "Strain Number")))

#' (This will not work with your data unless you have 'Strain Name' and 'Strain
#' Number' in your `csv_data`.)
#'
#' But there is a much easier shortcut:

# metadata(x) <- TRUE # sets the default csv_data() components as metadata
# metadata(x) <- FALSE # removes the default csv_data() components from the
#                      # metadata

#' *****************************************************************************
#'
#' ## 2.4
#' Here we check again for the availability of metadata in our data object. Some
#' should be present by now.
#'
#' A raw representation of the metadata:

metadata(x)

#' A nicer display as data frame, which might contain gaps:

to_metadata(x)

#' The set of all metadata *entries*:

metadata_chars(x, values = TRUE)

#' The set of all metadata *keys*:

metadata_chars(x, values = FALSE)

#' Do we have 'Strain' entries and, if so, where? Note that `%k%` stands for
#' 'key' -- here we only search for keys.

x %k% "Strain"
x %k% "A long key that is quite unlikely to be there!"
x %k% "ID"
x %k% list(ID = 9)

#' Note that the last query has *ignored* 9 because for `%k%` only the keys are
#' relevant.
#'
#' Where do we have 'Strain' entries with a certain value? Here `%q%` stands for
#' 'query' -- here we search for keys that have a certain value.

x %q% list(Strain = "DSM 917")

#' Where do we have 'Strain' entries with several possible values?

x %q% list(Strain = c("unknown", "DSM 1003"))

#' Where do we have 'Strain' entries and certain 'ID' entries?

x %q% list(Strain = c("unknown", "DSM 917"), ID = 5)

#' Such queries are very useful for selecting plates to specifically analyse
#' them, which we will deal with in a later part of this workshop.
#'
#' *****************************************************************************
#'
#' ## 2.5
#' An alternative formula syntax can be used to search in metadata. We suggest
#' to skip this at the beginning. The following examples do the same as their
#' counterparts above:

x %k% ~ Strain

x %q% ~ Strain == "DSM 917"

x %q% ~ Strain %in% c("unknown", "DSM 917")

x %q% ~ Strain %in% c("unknown", "DSM 917") & ID == 5

#' ### Troubleshooting
#' It is an error to apply `%q%` with a formula to metadata keys that are not
#' present. These errors can be avoided by using a list on the right-hand side
#' and by checking with `%k%` beforehand which keys are there. Also note that
#' metadata can be nested.
#'
#' *****************************************************************************
#'
#' Now proceed with part 3.


