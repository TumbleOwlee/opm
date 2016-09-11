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
OPM_OPTIONS$colors <- ""
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
  ),
  JP01 = "#1b9e77",
  JP02 = c("#1b9e77", "#d95f02"),
  JP03 = c("#1b9e77", "#d95f02", "#7570b3"),
  JP04 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"),
  JP05 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
    "#fb9a99"),
  JP06 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
    "#fb9a99", "#e31a1c"),
  JP07 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
    "#fb9a99", "#e31a1c", "#fdbf6f"),
  JP08 = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
    "#66a61e", "#e6ab02", "#a6761d", "#666666"),
  JP09 = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
    "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"),
  JP10 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
    "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
  JP11 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
    "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
    "#ffff99"),
  JP12 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
    "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
    "#ffff99", "#b15928"),
  JP13 = c("#8393c7", "#8ad256", "#6a49c5", "#d2b351",
    "#cb55c3", "#4d4040", "#c4527c", "#57743d", "#d85439", "#7accb1",
    "#925136", "#ceb2ab", "#512f67"),
  JP14 = c("#a2d1cd", "#5d39a8", "#71d14c", "#cb56c7",
    "#7ed094", "#4d4040", "#7077b8", "#c28b4c", "#cd9dae", "#c64a34",
    "#55868c", "#cccb51", "#b2436e", "#567137"),
  JP15 = c("#92d4ad", "#6842c1", "#6ecf58", "#cb4ec2",
    "#55733d", "#4d4040", "#c99447", "#9083cb", "#c9d14f", "#4d2c63",
    "#cea4a2", "#d54f38", "#71a6bd", "#ca507f", "#823f33"),
  JP16 = c("#76a5bd", "#bfdf44", "#cf4bab", "#66c95b",
    "#7c42c5", "#4d4040", "#7279ca", "#c27837", "#4b2a62", "#c7b956",
    "#cc8cb5", "#536e3b", "#d74746", "#84d3ae", "#893b42", "#cdb19a"),
  JP17 = c("#823f35", "#77d952", "#6d44c4", "#78d5a1",
    "#cf4a70", "#4d4040", "#ca53bd", "#69923c", "#6d7fc4", "#d1d04e",
    "#532b63", "#d64d31", "#4b623d", "#ca96b7", "#78b5c2", "#ccbf9b",
    "#c58741"),
  JP18 = c("#697bc5", "#5e9742", "#6641c0", "#7bdc57",
    "#c954c9", "#4d4040", "#4d2b62", "#73d6ac", "#d6493d", "#75adbe",
    "#c54883", "#526339", "#caca9b", "#7b332e", "#cfcf49", "#c89dc8",
    "#c58738", "#c78980"),
  JP19 = c("#9e693f", "#9147d5", "#c9d747", "#9482d3",
    "#61913d", "#4d4040", "#6dd85e", "#d049a4", "#76d0b6", "#d5493c",
    "#6897bb", "#d7993d", "#553291", "#c7cb8a", "#472f5b", "#cd7993",
    "#496340", "#ccb8bc", "#7f2c3a"),
  JP20 = c("#7295c1", "#d44b38", "#6ad14f", "#6a3bc0",
    "#cedb44", "#4d4040", "#77d192", "#cb4fc3", "#b1b85f", "#7772cc",
    "#d9973b", "#4f2b62", "#79d1cf", "#cc497b", "#4a6c2e", "#c990b5",
    "#752e30", "#d1c5ac", "#a26f47", "#537e71"),
  JP21 = c("#90b5d9", "#d6532d", "#c84ccc", "#74d147",
    "#512d79", "#4d4040", "#6740c8", "#cace49", "#6b79d1", "#6ccc84",
    "#c8478c", "#74c4b8", "#cc4458", "#4f6781", "#cb9142", "#552443",
    "#c6cb97", "#82442d", "#c489c5", "#546d37", "#cb9896"),
  JP22 = c("#392c51", "#4d4040", "#642c79", "#792d3b",
    "#6a3ec6", "#875b30", "#4f7231", "#547f72", "#d24637", "#6d71ce",
    "#d2497e", "#cd4fc8", "#6a8fbc", "#d88742", "#c78dc6", "#cc9795",
    "#c7af40", "#68cd55", "#72d4a6", "#9ecfd6", "#c9cb8f", "#c3de48"),
  JP23 = c("#8ad93f", "#c749c4", "#5e8f3d", "#6639be",
    "#73d979", "#4d4040", "#d4ca4a", "#6c6ccc", "#d78c3b", "#6485b9",
    "#d24635", "#70d4ae", "#cc4279", "#cbcb99", "#4c295f", "#ce867e",
    "#793130", "#84cbd7", "#896c35", "#c27bbb", "#364e27", "#cab2cb",
    "#5b837b"),
  JP24 = c("#ccc79a", "#6a42c7", "#d0a540", "#cc49c9",
    "#6dd755", "#4d4040", "#de5a26", "#7cc7d0", "#cc3f47", "#78d8a5",
    "#5e2d78", "#c9da51", "#6679d0", "#bf7348", "#c6b7d8", "#5f903c",
    "#c47ec5", "#6a5b29", "#ce4684", "#497359", "#772d38", "#c3858c",
    "#352444", "#5b7a9e"),
  JP25 = c("#6ba43c", "#c74ace", "#cbe14b", "#6847cd",
    "#6ede53", "#4d4040", "#cbb248", "#592e82", "#d6842f", "#5e78c1",
    "#76dd99", "#c6438e", "#4b8047", "#cf4c67", "#7acdc4", "#d2472f",
    "#7ba5c4", "#79322f", "#c388cf", "#78662f", "#45294d", "#c8cd9d",
    "#3e5d4a", "#d08c6c", "#c698a9"),
  JP26 = c("#73d991", "#b44adb", "#71d94d", "#cf4cb4",
    "#ccde4d", "#4d4040", "#ceae44", "#5a41c2", "#cdd09c", "#652e7a",
    "#83d7ce", "#dc4338", "#536e83", "#d34a79", "#5d9073", "#c68dc7",
    "#619339", "#85b1d7", "#da8340", "#6978cb", "#9d4533", "#34284e",
    "#d09e9e", "#732d41", "#364e25", "#866a38"),
  JP27 = c("#363258", "#6ed853", "#5b3fc7", "#c9de43",
    "#b54ad9", "#4d4040", "#5c2c7e", "#b7d17b", "#cf4a83", "#6ed9a4",
    "#cd4450", "#8fd3d5", "#d74527", "#769ac1", "#d27d3f", "#6d75cf",
    "#d4af42", "#4f8c3b", "#d14eba", "#568778", "#c692c8", "#344625",
    "#d4c7a6", "#722e4c", "#c88988", "#7a3a25", "#86783a"),
  JP28 = c("#7f3a27", "#71da53", "#c14bd4", "#55933d",
    "#626ad0", "#4d4040", "#623ac4", "#cbd943", "#542c79", "#c1d483",
    "#bc7fd0", "#6ad7a3", "#d84330", "#71bec7", "#ce7537", "#6f99d8",
    "#d5aa43", "#546586", "#7c7233", "#ce429f", "#3e6344", "#ce7d9f",
    "#2d1d38", "#c6b3ce", "#793151", "#bfcbae", "#d24566", "#c8927d"),
  JP29 = c("#cdc2c2", "#663dc8", "#76dd51", "#c64ece",
    "#cfda49", "#4d4040", "#549e3f", "#7577da", "#d3522e", "#7cd6ce",
    "#d4425b", "#77de9a", "#542a7e", "#d1d395", "#321e3d", "#d74a98",
    "#95963d", "#586095", "#db9a3e", "#77abd9", "#8b3c67", "#639575",
    "#d08982", "#456129", "#ca92cc", "#896134", "#597984", "#742c28",
    "#283a28"),
  JP30 = c("#31223c", "#bbe141", "#c94edb", "#65d559",
    "#8b3899", "#4d4040", "#613ec8", "#df9b36", "#6e75d5", "#c16c39",
    "#402a74", "#cfc248", "#da47a4", "#63d6ad", "#d94330", "#6abccd",
    "#c58181", "#617fae", "#7f2f2c", "#b5cfb8", "#833b65", "#b5d888",
    "#cc88cb", "#4e8a3b", "#d6466a", "#476d58", "#d2b284", "#544320",
    "#c9b6d0", "#867c36"),
  JP31 = c("#913d83", "#ced242", "#6643d0", "#79d949",
    "#c249d4", "#4d4040", "#db45a4", "#68dc88", "#3a1f4f", "#c3d483",
    "#532e8e", "#da983e", "#6d79d5", "#9b4b29", "#d085d5", "#8b7d3b",
    "#c9a0c0", "#54913d", "#dc4b32", "#72d4b1", "#8f3e58", "#90d0d8",
    "#592720", "#d2c7a9", "#21262c", "#d64769", "#3b4f25", "#6ea2cf",
    "#cd887a", "#5c6089", "#568477"),
  JP32 = c("#8f8b38", "#663cc8", "#6bd546", "#c74cce",
    "#b1d773", "#4d4040", "#c6e03a", "#59287c", "#5edb86", "#d14592",
    "#7ad9b1", "#da4627", "#719cd8", "#dc973a", "#6e71d7", "#dbc348",
    "#ca84c8", "#4c8b3a", "#d5445a", "#84ccd6", "#7f3353", "#d3c99f",
    "#2e1c38", "#ca7442", "#5a558b", "#803325", "#537286", "#cc8585",
    "#314826", "#cab3cc", "#7e6136", "#618d75"),
  JP33 = c("#d64e9e", "#6cd54c", "#dd49d1", "#c8dd41",
    "a#152dd", "#4d4040", "#5139c2", "#ceaa3b", "#432d7c", "#c6d179",
    "#8f379a", "#70d68c", "#d9432f", "#6ad5be", "#d5416a", "#76c2d7",
    "#d87a71", "#6a75d5", "#836834", "#c988d1", "#598939", "#7a3260",
    "#bed3b3", "#8f372e", "#6082b3", "#d47c35", "#312749", "#d4ac8b",
    "#314825", "#cab9d7", "#4b211f", "#ad788b", "#568275"),
  JP34 = c("#d8436c", "#653cc7", "#b4dc41", "#d143d0",
    "#5fd857", "#4d4040", "#a4db84", "#c64496", "#6adcad", "#de4830",
    "#6aa3d9", "#d98731", "#6271d1", "#dec841", "#b062cd", "#528e36",
    "#c28acd", "#675b2c", "#cbb7d3", "#a53332", "#528089", "#532878",
    "#d9d393", "#2a1e3c", "#8ed4d3", "#834629", "#5e5e8a", "#a08e3c",
    "#2b482a", "#d78763", "#619470", "#c87b8d", "#702944", "#c3a994"),
  JP35 = c("#72d4cf", "#ccdf3e", "#5533c1", "#70d951",
    "#ac42d6", "#4d4040", "#6d66dc", "#b9c866", "#562a84", "#71da99",
    "#db43c7", "#518f39", "#d04497", "#314826", "#bc6cc9", "#5d8b74",
    "#d2416d", "#72abd3", "#dd461f", "#6078c6", "#d7ab3b", "#c49ad6",
    "#7d6b2f", "#cab8c4", "#3c1a20", "#c8ddb6", "#312652", "#cfb182",
    "#7c3463", "#c98271", "#576782", "#d24243", "#cb7a99", "#82372d",
    "#cf7734"),
  JP36 = c("#6ade4b", "#6344d3", "#7bdc86", "#b746d4",
    "#65a234", "#4d4040", "#dbc941", "#552c93", "#bee148", "#dc3fb4",
    "#62d7b4", "#903a7e", "#4a8245", "#cf74d0", "#da993a", "#3e255f",
    "#c0d3b2", "#291d2d", "#cdce7e", "#752c41", "#7dcbd6", "#c43c44",
    "#669bcf", "#de4e28", "#5b5e83", "#c97449", "#bd92d0", "#847933",
    "#d7417a", "#558279", "#d07d92", "#364525", "#ceb9d0", "#763d23",
    "#6872d2", "#be9880"),
  JP37 = c("#645b8e", "#80dc40", "#4f2ea4", "#69dc7b",
    "#d848cd", "#4d4040", "#8548da", "#c7d84e", "#96368e", "#afd995",
    "#d54227", "#61d9b9", "#db4187", "#4a9339", "#cd83d6", "#7a8431",
    "#6870d5", "#e3bc3b", "#6b9bd7", "#d87935", "#6fbfcf", "#cd3e50",
    "#c3d8c8", "#772e29", "#dbc38b", "#3f2267", "#bf9340", "#cab1d6",
    "#304726", "#b2918d", "#2a1f35", "#d5816f", "#5e8c6b", "#c77192",
    "#497080", "#7d592d", "#732d52"),
  JP38 = c("#cf8ad0", "#74e042", "#b946da", "#5be080",
    "#5834c1", "#4d4040", "#d248bb", "#59a434", "#8064d4", "#b4dc4e",
    "#893876", "#96db99", "#d9478a", "#499052", "#627bcf", "#dfd238",
    "#47277a", "#908f39", "#79a2d8", "#d79234", "#4c7788", "#df502c",
    "#625984", "#d7d27b", "#2e1d3b", "#6bdac4", "#d34557", "#6a8b73",
    "#9e4427", "#cfb5cd", "#78562e", "#7cc6d5", "#26392b", "#cdcfb2",
    "#702735", "#bd7984", "#405924", "#d59571"),
  JP39 = c("#8b308f", "#74dd41", "#6939ca", "#cce346",
    "#d545d2", "#4d4040", "#b271dd", "#e39b39", "#5050bc", "#cabc46",
    "#3a1f64", "#5cde7e", "#d9428e", "#57a56d", "#d63949", "#76dfc2",
    "#7e3052", "#b7e28f", "#d286c6", "#66a234", "#6d83d8", "#d65629",
    "#76c3d2", "#843326", "#6aa0d5", "#9c762c", "#5f5488", "#d48e70",
    "#4a6a81", "#d36778", "#466b2c", "#b28491", "#273825", "#c1b47a",
    "#301b31", "#d0d2bd", "#6c552d", "#c9b8d8", "#5f8675"),
  JP40 = c("#3c2b5d", "#dee032", "#ab48d5", "#5bd749",
    "#db49c6", "#4d4040", "#5c42d0", "#a4e040", "#462687", "#d8b136",
    "#8d3989", "#60d076", "#d7468f", "#63d8b5", "#de4528", "#77c7d6",
    "#d13a55", "#5f8c7b", "#ce88d5", "#759b31", "#696ecd", "#de8739",
    "#6f9ad6", "#b75738", "#aadc90", "#946d89", "#d0dc6a", "#2c1a25",
    "#c6d8bc", "#782849", "#ceb977", "#283f27", "#d9798c", "#447c3d",
    "#ceb8d4", "#635b2d", "#c79783", "#733426", "#476682", "#98762e")
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

