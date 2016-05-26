NOT_YET <- "not yet implemented"
BUG_MSG <- "a bug -- this should not happen"

SEALED <- TRUE #|| SEALED <- FALSE

CSV_NAMES <- c(FILE = "File", PLATE_TYPE = "Plate Type", POS = "Position",
  SETUP = "Setup Time")

HOUR <- "Hour"

SPECIAL_PLATES <- c("Gen III", "ECO", "SF-N2", "SF-P2", "AN2", "FF", "YT")
names(SPECIAL_PLATES) <- c("gen.iii", "eco", "sf.n2", "sf.p2", "an2", "ff",
  "yt")

SP_PATTERN <- sub("^SF", "G", SPECIAL_PLATES, TRUE, TRUE)
SP_PATTERN <- unique(c(SP_PATTERN, SPECIAL_PLATES))
SP_PATTERN <- toupper(gsub("\\W", "", SP_PATTERN, FALSE, TRUE))
SP_PATTERN <- sprintf("^(%s)([A-Z]*)$", paste0(SP_PATTERN, collapse = "|"))

THEOR_RANGE <- c(0, 400)

CURVE_PARAMS <- c("mu", "lambda", "A", "AUC")

DISC_PARAM <- "disc"

RESERVED_NAMES <- c("Plate", "Well", "Time", "Value", "Parameter")
names(RESERVED_NAMES) <- tolower(RESERVED_NAMES)

MEASUREMENT_COLUMN_MAP <- c(Well = "well_id", Time = "time", Value = "value")

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

INSTRUMENT <- "Instrument"

HTML_DOCTYPE <- paste('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"',
  '"http://www.w3.org/TR/html4/strict.dtd">', collapse = " ")

STR_NOTE <- paste0("\n",
  "IMPORTANT NOTE: DO NOT RELY ON THE INTERNAL STRUCTURE OF THIS KIND OF\n",
  "OBJECT, WHICH MIGHT CHANGE IN FUTURE VERSIONS OF THE PACKAGE. USE THE\n",
  "ACCESSOR METHODS FOR THIS KIND OF OBJECT INSTEAD OF THE `@` OPERATOR.\n")

MEMOIZED <- new.env(parent = emptyenv())

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
OPM_OPTIONS$md.duration <- "Duration"
OPM_OPTIONS$md.id.name <- "ID"
OPM_OPTIONS$md.id.start <- 1L
OPM_OPTIONS$min.mode <- 0.5
OPM_OPTIONS$phylo.fmt <- "epf"
OPM_OPTIONS$series.key <- "Concentration"
OPM_OPTIONS$split <- "/.-_"
OPM_OPTIONS$strict.OPMD <- FALSE
OPM_OPTIONS$threshold <- 0
OPM_OPTIONS$time.fmt <- c("%m/%d/%Y %I:%M:%S %p", "%b %d %Y %I:%M %p",
  "%d.%m.%Y %H:%M:%S", "%b %d %Y %H:%M")
OPM_OPTIONS$time.zone <- ""
OPM_OPTIONS$warn.mult <- FALSE

COLORS <- list(
  # Basic colour keywords from http://www.w3.org/TR/css3-color/ (accessed on
  # 29-8-2011), sorted darkest-first.
  W3C = c(
    black = "#000000", navy = "#000080",
    green = "#008000", maroon = "#800000", blue = "#0000FF", lime = "#00FF00",
    red = "#FF0000", teal = "#008080", purple = "#800080", olive = "#808000",
    gray = "#808080", aqua = "#00FFFF", fuchsia = "#FF00FF",
    yellow = "#FFFF00", silver = "#C0C0C0", white = "#FFFFFF"
    ),
  # Colours manually selected and sorted by Nora Buddruhs for maximum contrast.
  NORA = c(
    "midnightblue", "darkred", "darkgreen", "orange",
    "lightslateblue", "seashell4", "saddlebrown", "firebrick2",
    "palevioletred3", "purple4"
    ),
  # Shades of pink...
  ROSEO = c(
    "maroon1", "palevioletred3", "hotpink1",
    "mediumvioletred", "violetred3", "deeppink3", "lightcoral", "pink1",
    "indianred3", "magenta1"
    ),
  # Colours from the two qualitative ColorBrewer (http://colorbrewer2.org/)
  # palettes, in order.
  BREWER = c(
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
    "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928",
    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
    "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"
  ),
  # from http://godsnotwheregodsnot.blogspot.de/2012/09/
  # color-distribution-methodology.html (converted from RGB)
  SSNOT = c(
    "#000000", "#010067", "#D5FF00", "#FF0056", "#9E008E", "#0E4CA1",
    "#FFE502", "#005F39", "#00FF00", "#95003A", "#FF937E", "#A42400",
    "#001544", "#91D0CB", "#620E00", "#6B6882", "#0000FF", "#007DB5",
    "#6A826C", "#00AE7E", "#C28C9F", "#BE9970", "#008F9C", "#5FAD4E",
    "#FF0000", "#FF00F6", "#FF029D", "#683D3B", "#FF74A3", "#968AE8",
    "#98FF52", "#A75740", "#01FFFE", "#FFEEE8", "#FE8900", "#BDC6FF",
    "#01D0FF", "#BB8800", "#7544B1", "#A5FFD2", "#FFA6FE", "#774D00",
    "#7A4782", "#263400", "#004754", "#43002C", "#B500FF", "#FFB167",
    "#FFDB66", "#90FB92", "#7E2DD2", "#BDD393", "#E56FFE", "#DEFF74",
    "#00FF78", "#009BFF", "#006401", "#0076FF", "#85A900", "#00B917",
    "#788231", "#00FFC6", "#FF6E41", "#E85EBE"
  ),
  # from http://phrogz.net/css/distinct-colors.html
  # selected by Jan Meier-Kolthoff, 2016-05-24
  PHROGZ = c(
    "#c10000", "#81ba67", "#0085ff", "#f056ff", "#e60000", "#005000",
    "#81aee5", "#7d006d", "#ff706e", "#00cf00", "#005ddb", "#ff78e6",
    "#f2936b", "#00791a", "#0031a2", "#d20095", "#984624", "#00a43f",
    "#003bdb", "#a4006e", "#6d2200", "#005137", "#989dff", "#ff0084",
    "#ff6f28", "#007a59", "#6f60ff", "#b80d5e", "#9c4600", "#00a57e",
    "#a487ff", "#8b003b", "#ca6b00", "#00a7b9", "#5b00dc", "#ff73aa",
    "#bfaa69", "#007c91", "#5c00a3", "#662439", "#6d5d21", "#00daff",
    "#b355f8", "#ff004d", "#473b00", "#00536b", "#51296c", "#8f485c",
    "#9d8200", "#00c2ff", "#784c93", "#e395a9", "#ababa7", "#00afff",
    "#c999e5", "#bf0128", "#5e5f5b", "#0080cb", "#c013ff", "#ff1730",
    "#3b3c38", "#003f6c", "#e07fff", "#900007", "#669100", "#0057a1",
    "#8e00a3"
  ),
  # From six ColorBrewer palettes.
  GROUPS = c(
    "#c6dbef", "#c7e9c0", "#d9d9d9", "#fdd0a2", "#dadaeb", "#fcbba1",
    "#2171b5", "#238b45", "#525252", "#d94801", "#6a51a3", "#cb181d",
    "#6baed6", "#74c476", "#969696", "#fd8d3c", "#9e9ac8", "#fb6a4a",
    "#4292c6", "#41ab5d", "#737373", "#f16913", "#807dba", "#ef3b2c",
    "#9ecae1", "#a1d99b", "#bdbdbd", "#fdae6b", "#bcbddc", "#fc9272",
    "#deebf7", "#e5f5e0", "#f0f0f0", "#fee6ce", "#efedf5", "#fee0d2",
    "#08519c", "#006d2c", "#252525", "#a63603", "#54278f", "#a50f15",
    "#f7fbff", "#f7fcf5", "#fffffe", "#fff5eb", "#fcfbfd", "#fff5f0",
    "#08306b", "#00441b", "#000000", "#7f2704", "#3f007d", "#67000d"
  )
)

COLORS$W3C <- COLORS$W3C[c("teal", "purple", "olive", "black", "silver",
  "blue", "lime", "red", "aqua", "fuchsia", "yellow", "navy", "green",
  "maroon", "gray")]

COLORS <- as.environment(COLORS)

CHARACTER_STATES <- c(0L:9L, LETTERS)[1L:32L]
MISSING_CHAR <- "?"

PHYLO_FORMATS <- c("epf", "nexus", "phylip", "hennig", "html")

`_GROUPING` <- "?"

AMINO_ACIDS <- c(
  # proteinogenic amino acids
  Alanine = "Ala", Cysteine = "Cys", Glycine = "Gly", Isoleucine = "Ile",
  Leucine = "Leu", Methionine = "Met", Proline = "Pro", Valine = "Val",
  Serine = "Ser", Threonine = "Thr", `Aspartic Acid` = "Asp",
  `Glutamic Acid` = "Glu", Histidine = "His", Arginine = "Arg", Lysine = "Lys",
  Asparagine = "Asn", Glutamine = "Gln", Phenylalanine = "Phe",
  Tryptophan = "Trp", Tyrosine = "Tyr",
  # modified proteinogenic amino acids and selection of uncommon ones
  # three-letter abbreviations are from CAS search engine
  `Diamino-Pimelic Acid` = "Dpm", Homoarginine = "Har", Homocysteine = "Hcy",
  Homohistidine = "Hhs", Homoserine = "Hse", Hydroxyproline = "Hyp",
  Isovaline = "Iva", Norleucine = "Nle", Nortyrosine = "Nty", Norvaline = "Nva",
  Ornithine = "Orn", Penicillamine = "Pen", `Pyroglutamic Acid` = "Glp",
  Pyrrolysine = "Pyl", Sarcosine = "Sar", Selenocysteine = "Scy",
  Statine = "Sta"
)

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
    structure(.Data = sprintf("<%s>%s</%s>", x, names(x), x), names = names(x))
  )(COMPOUND_NAME_HTML_MAP)

COMPOUND_NAME_HTML_MAP <- c(
  COMPOUND_NAME_HTML_MAP,
  structure(.Data = sprintf("&%s;", GREEK_LETTERS),
    names = names(GREEK_LETTERS)),
  structure(.Data = sprintf("&%s;", GREEK_LETTERS), names = GREEK_LETTERS)
)

SUBSTRATE_PATTERN <- (function() {
  # we prepare for paired parentheses or paired brackets in substrate names
  x <- c(paren = "\\(((?:[^()]+|\\([^()]+\\))+)\\)",
    bracket = "\\[((?:[^\\[\\]]+|\\[[^\\[\\]]+\\])+)\\]")
  x <- c(x, either = paste0("\\s*(", x[1L], "|", x[2L], ")", collapse = ""))
  x <- c(x, any = paste0("(?:\\s*(?:", x[1L], "|", x[2L], "))?", collapse = ""))
  x[1L:2L] <- sprintf("\\s*%s", x[1L:2L])
  x <- c(x, plain = "")
  structure(.Data = sprintf("^[A-Z]\\d{2}%s$", x), names = names(x))
})()

