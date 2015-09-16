#' # WORKSHOP PART 1: Inputting PM data
#'
#' (According section in the tutorial: 3.2. Data import)
#'
#' *****************************************************************************
#'
#' ## 1.1
#' How does `R` work? By excuting code, i.e. by calling functions, mostly with
#' some arguments. Code can be commented, e.g. *this line* is a comment and will
#' not be executed. You can use this to (de-)activate code, too:
#'
#' This will be printed:

message("I like R and opm!")

#' This will *not* be printed:

#message("I don't like R and opm!")


#' ### Some calculations

5 + 19 # result will be printed to the screen
result <- 5 + 19 # will not print
result # will print: 'result' now contains 24, we have assigned it using `<-`

5 + 15:20 # what does this do?

result # still there
rm(result) # deleting it

#' Entering `result` would now yield "Error: object 'result' not found" or so.
#'
#' ### Character strings

"I am a character string and will be printed to the screen"

#' ### Boolean (logical) values

TRUE # this means, well, true
FALSE # guess what
NA # this means 'NOT AVAILABLE' (missing data)

#' ### Formulas
#'
#' With left-hand side:

x ~ a * y1 + b * y2 - c * y3 ^ 2

#' Without left-hand side:

~ a * y1 + b * y2 - c * y3 ^ 2

#' Formulas are unevaluated code for storing symbolic representations to be
#' interpreted later on in a specific way.
#'
#' ### Troubleshooting
#' If anything fails, read the error message!
#'
#' *****************************************************************************
#'
#' ## 1.2
#' Now we load and attach the `opm` functions. Note that if `opm` has already
#' been loaded, this command does nothing. It thus can be called at any time.

library(pkgutils)
library(opm)

message("Welcome to R and opm!")

#' ### Troubleshooting
#' Note that you can call

#  ?ITEM

#' at any time to get the *documentation* for the topic *ITEM* (replace *ITEM*
#' by what you are interested in). The following command would list the content
#' of the entire `opm` documentation:

# help(package = "opm")

#' *****************************************************************************
#'
#' ## 1.3
#' Now please make sure you are in the right directory! You can call the `getwd`
#' command to check that. To change the working directory in Rstudio use:
#' `Session > Set Working Directory > To Source File Location`.
#'
#' Let's issue a warning about the working directory:

warning("the working directory is ", getwd(), " -- correct?")

#' *****************************************************************************
#'
#' ## 1.4
#' We assume that only the CSV files within the working directory should be
#' input and that the data read should be grouped by plate type (see below or
#' consult the `opm` for what that means). This code fails if unreadable CSV
#' files are there and not deselected (the `include` and/or `exclude` argument
#' would need to be adapted).

x <- read_opm(
  names = getwd(), # search in the working directory for files of interest
  convert = "grp", # group the plates according to the plate type
  include = list("csv"), # search only for CSV files
  exclude = "*template*", # but exclude CSV files that match this pattern
                          # (we later on generate metadata template CSV files)
  demo = FALSE # read the files, do not just show the file names
)


#' ### Troubleshooting
#' If this fails, read the error message! The most usual error we know about is
#' to try to input CSV files with *several* plates per file, but these would
#' need to be split beforehand, which you can do with opm itself. This works as
#' follows:

# split_files(filename, '^("Data File",|Data File)', getwd())

#' where `filename` is the name of the file to be split, provided as character
#' string. The newly generated files are numbered accordingly. (They are not
#' named after any metainformation entry because there is no guarantee that it
#' is present.)
#'
#' The second frequent kind of error is that you attempt to read files that are
#' CSV but do not contain PM data. The name of the file that fails is shown in
#' the error message, look at it! Use `demo = TRUE` to first show the files you
#' would read, and if this list contains names of files that expectedly cannot
#' be read by opm, modify the inclusion or exclusion settings. Alternatively,
#' modify your folder structure. Finally, note that you can always use a
#' character vector of specific file names collected by hand as `names`
#' argument. This gives you most control about the files read.
#'
#' The `opm` package understands several styles of CSV and the new LIMS format.
#'
#' *****************************************************************************
#'
#' ## 1.5
#' We now have a look at the resulting object. You do not routinely need all of
#' these, some calls are just for beginners who are curious. Others tell you
#' important features of the generated data object.
#'
#' That code yields "MOPMX"; if you are curious what that means, consult figure
#' 4 in the tutorial.

class(x)

#' As often in `R`, `summary` is a most useful function...

summary(x)

#' ...but in `opm` you can just enter the name of the object, which also shows
#' the summary:

x

#' Show the distinct plate types:

names(x)

#' Names could be missing, however, but the next function always works to get
#' the plate type:

plate_type(x)

#' This checks for aggregated data (a.k.a. curve parameters); must all be
#' `FALSE`:

has_aggr(x)

#' This show the metainformation that was directly found in the CSV files:

csv_data(x)

#' The next functions shows the proper metadata (none there yet!):

metadata(x)

#' It makes no sense to proceed if we have not input any data. This happens if
#' no suitable files are found.

length(x) > 0

#' *****************************************************************************
#'
#' ## 1.6
#' If you have only one plate type, you could simplify the object:

# x <- x[[1]] # fetches the 1st element, which belongs to the sole plate type
# class(x) # yielded "OPMS" or "OPM"; if you are curious, consult figure 4 in
#          # the tutorial

#' But this is not normally necessary.
#'
#' *****************************************************************************
#'
#' Now proceed with part 2.

