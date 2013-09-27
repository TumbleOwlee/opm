

################################################################################
################################################################################
#
# Programming constants
#


# Error messages
#
NOT_YET <- "not yet implemented"
BUG_MSG <- "a bug -- this should not happen"


# Class names
#
WMD <- "WMD"
OPM <- "OPM"
OPMA <- "OPMA"
OPMD <- "OPMD"
OPMS <- "OPMS"
OPMX <- "OPMX"
YAML_VIA_LIST <- "YAML_VIA_LIST"
MOA <- "MOA"
FOE <- "FOE"
CMAT <- "CMAT"


# Used in all S4 method definitions. The idea is to keep it FALSE during
# programming but set it to TRUE when building the package. The line contains a
# special  comment used by pkgutils and should not be changed.
#
SEALED <- TRUE #|| SEALED <- FALSE


################################################################################
################################################################################
#
# Input constants
#


# Names used in CSV data
#
CSV_NAMES <- c(FILE = "File", PLATE_TYPE = "Plate Type", POS = "Position",
  SETUP = "Setup Time")


# Expected name of time points in input data
#
HOUR <- "Hour"


# The known special plates
#
SPECIAL_PLATES <- c("Gen III", "ECO", "SF-N2", "SF-P2", "AN2", "FF", "YT")
names(SPECIAL_PLATES) <- c("gen.iii", "eco", "sf.n2", "sf.p2")

# Pattern used for matching them
#
SP_PATTERN <- sub("^SF", "G", SPECIAL_PLATES, TRUE, TRUE)
SP_PATTERN <- unique(c(SP_PATTERN, SPECIAL_PLATES))
SP_PATTERN <- toupper(gsub("\\W", "", SP_PATTERN, FALSE, TRUE))
SP_PATTERN <- sprintf("^(%s)([A-Z]*)$", paste0(SP_PATTERN, collapse = "|"))


# Theoretically expected range of the OmniLog measurements (Bochner, pers.
# comm.)
#
THEOR_RANGE <- c(0, 400)


################################################################################
################################################################################
#
# Output constants
#


# Curve parameters
# N.B.: The order must be kept in sync with map_param_names() and with the
# functions that fetch the name of lambda using CURVE_PARAMS[2L]. So this should
# not be changed unless great care is taken.
#
CURVE_PARAMS <- c("mu", "lambda", "A", "AUC")

# An addtion: the name used to select the discretized values.
#
DISC_PARAM <- "disc"


# Reserved metadata names (they CAN be used by in the metadata but might yield
# problems).
#
RESERVED_NAMES <- c("Plate", "Well", "Time", "Value", "Parameter")
names(RESERVED_NAMES) <- tolower(RESERVED_NAMES)


# Names used in aggregation/discretization settings.
#
SOFTWARE <- "software"
VERSION <- "version"
UNKNOWN_VERSION <- "0.0.0"
PROGRAM <- "program" # from the old style, synonym of METHOD in new style
METHOD <- "method"
OPTIONS <- "options"
KNOWN_METHODS <- list(
  aggregation = c("grofit", "opm-fast", "shortcut", "splines"),
  discretization = c("direct", "kmeans", "best-cutoff")
)

# Optionally used in collect_template() output
INSTRUMENT <- "Instrument"

# Used by several functions in the headers of HTML output.
#
HTML_DOCTYPE <- paste('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"',
  '"http://www.w3.org/TR/html4/strict.dtd">', collapse = " ")


################################################################################
################################################################################
#
# Storage of precomputed values
#

MEMOIZED <- new.env(parent = emptyenv())


################################################################################
################################################################################
#
# Default opm options.
#

OPM_OPTIONS <- new.env(parent = emptyenv())
OPM_OPTIONS$color.borders <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E",
  "#993404")
OPM_OPTIONS$colors <- "w3c"
OPM_OPTIONS$comb.key.join <- "."
OPM_OPTIONS$comb.value.join <- "/"
OPM_OPTIONS$contrast.type <- "Tukey"
OPM_OPTIONS$css.file <- ""
OPM_OPTIONS$csv.keys <- unname(CSV_NAMES[c("SETUP", "POS")])
OPM_OPTIONS$csv.selection <- unname(CSV_NAMES[c("SETUP", "POS", "FILE")])
OPM_OPTIONS$curve.param <- "A"
OPM_OPTIONS$digits <- 4L
OPM_OPTIONS$disc.param <- "A"
OPM_OPTIONS$file.encoding <- ""
OPM_OPTIONS$file.split.tmpl <- "%s-%05i.%s"
OPM_OPTIONS$gen.iii <- ""
OPM_OPTIONS$group.name <- "Group"
OPM_OPTIONS$heatmap.colors <- topo.colors(120L)
OPM_OPTIONS$html.class <- "section-level-%i"
OPM_OPTIONS$input.try.order <- c(1L, 2L, 3L)
OPM_OPTIONS$key.join <- "."
OPM_OPTIONS$machine.id <- 1L
OPM_OPTIONS$max.chars <- 100L
OPM_OPTIONS$min.mode <- 0.5
OPM_OPTIONS$phylo.fmt <- "epf"
OPM_OPTIONS$split <- "/.-_"
OPM_OPTIONS$strict.OPMD <- FALSE
OPM_OPTIONS$threshold <- 0
OPM_OPTIONS$time.fmt <- c("%m/%d/%Y %I:%M:%S %p", "%b %d %Y %I:%M %p",
  "%d.%m.%Y %H:%M:%S", "%b %d %Y %H:%M")
OPM_OPTIONS$time.zone <- ""


################################################################################
################################################################################
#
# Constants related to the phylogeny functions
#


# These must be kepot in sync between phylogeny and discretization functions.
#
CHARACTER_STATES <- c(0L:9L, LETTERS)[1L:32L]
MISSING_CHAR <- "?"

# Used by several functions via match.arg().
#
PHYLO_FORMATS <- c("epf", "nexus", "phylip", "hennig", "html")


################################################################################
################################################################################
#
# Constants related to the processing of substrate names
#


# We consider only those Greek letters that are likely to occur in substrate
# names, and deliberately not their uppercase versions.
GREEK_LETTERS <- c("alpha", "beta", "gamma", "delta", "epsilon")
names(GREEK_LETTERS) <- substr(GREEK_LETTERS, 1L, 1L)


COMPOUND_NAME_HTML_MAP <- c(
  # stereochemistry and configuration
  cis = "i", o = "i", m = "i", p = "i", meso = "i", tert = "i", exo = "i",
  threo = "i", iso = "i", cyclo = "i", R = "i", S = "i", E = "i", Z = "i",
  # chemical elements (for "S" see above)
  N = "i", O = "i", P = "i",
  # configuration of sugars or amino acids
  D = "small", L = "small"
)

COMPOUND_NAME_HTML_MAP <- (function(x)
    structure(sprintf("<%s>%s</%s>", x, names(x), x), names = names(x))
  )(COMPOUND_NAME_HTML_MAP)

COMPOUND_NAME_HTML_MAP <- c(
  COMPOUND_NAME_HTML_MAP,
  structure(sprintf("&%s;", GREEK_LETTERS), names = names(GREEK_LETTERS))
)


SUBSTRATE_PATTERN <- (function() {
  # we prepare for paired parentheses or paired brackets in substrate names
  x <- c(paren = "\\(((?:[^()]+|\\([^()]+\\))+)\\)",
    bracket = "\\[((?:[^\\[\\]]+|\\[[^\\[\\]]+\\])+)\\]")
  # because 'paren.sep' may be anything, we cannot be too strict here
  x <- c(x, either = paste0(".*(", x[1L], "|", x[2L], ")", collapse = ""))
  x <- c(x, any = paste0("(?:.*(?:", x[1L], "|", x[2L], "))?", collapse = ""))
  x[1L:2L] <- sprintf(".*%s", x[1L:2L])
  x <- c(x, plain = "")
  structure(sprintf("^[A-Z]\\d{2}%s$", x), names = names(x))
})()


