# Regenerates the dataset fixture package used by the analyzer contract test.
#
# The output is committed, so the suite needs only the analyzer binary and none
# of the packages below. This script exists so the inputs stay reproducible and
# reviewable: it is run by hand, from the repository root, when a new shape has
# to be covered.
#
#   Rscript tests/testthat/fixtures/dataset-contract/make.R
#
# Every object here is chosen to make the analyzer report one family of fields.
# Keep them tiny: the point is which fields come back, never how much data.

root <- file.path("tests", "testthat", "fixtures", "dataset-contract", "pkg")
d <- file.path(root, "data")
unlink(d, recursive = TRUE)
dir.create(d, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "R"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "man"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "inst", "extdata"), recursive = TRUE, showWarnings = FALSE)

sv <- function(obj, name, version = 3, compress = "gzip") {
  assign(name, obj)
  save(list = name, file = file.path(d, paste0(name, ".rda")), version = version,
       compress = compress)
}

# ---- rectangles -----------------------------------------------------------
# One frame carrying every column type the profiler has a statistics block for:
# numeric, integer, character, logical, factor, ordered factor, Date, POSIXct.
set.seed(1)
sv(data.frame(
  num  = c(1.5, 2.5, 100.0, 3.5, NA, 4.5, 0, 0),
  int  = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
  chr  = c("alpha", "beta", "", "beta", "gamma", "delta", "beta", "e"),
  lgl  = c(TRUE, FALSE, TRUE, NA, TRUE, FALSE, TRUE, TRUE),
  fac  = factor(c("a", "b", "a", "c", "b", "a", "c", "b")),
  ord  = factor(c("lo", "hi", "mid", "hi", "lo", "mid", "hi", "lo"),
                levels = c("lo", "mid", "hi"), ordered = TRUE),
  date = as.Date("2020-01-01") + 0:7,
  time = as.POSIXct("2020-01-01 10:00", tz = "America/Chicago") + (0:7) * 3600,
  stringsAsFactors = FALSE
), "every_type")

rn <- data.frame(a = 1:3, b = c(2.0, 4.0, 6.0))
rownames(rn) <- c("one", "two", "three")
sv(rn, "named_rows")

sv(data.frame(a = integer(0), b = character(0)), "no_rows")

# The awkward numbers again, this time as a column: NaN and each infinity are
# counted per column and never for the object as a whole.
sv(data.frame(v = c(1, NaN, Inf, -Inf, NA, 0, 2), w = c(1, 2, 3, 4, 5, 6, 7)), "awkward_column")

if (requireNamespace("tibble", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)) {
  tb <- tibble::tibble(g = c("x", "x", "y"), v = c(1, 2, 3))
  sv(dplyr::group_by(tb, g), "grouped_tbl")
  sv(dplyr::rowwise(tb), "rowwise_tbl")
}
if (requireNamespace("data.table", quietly = TRUE)) {
  dt <- data.table::data.table(k = c("a", "b", "c"), v = 1:3)
  data.table::setkey(dt, k)
  data.table::setindexv(dt, "v")
  sv(dt, "keyed_dt")
}

# ---- vectors --------------------------------------------------------------
# Missing runs, both infinities, NaN, zeros and a sort order, in one vector.
sv(c(NA, NA, 0, 1, NaN, Inf, -Inf, 0, 5, NA), "awkward_numbers")
sv(sort(c(3L, 1L, 2L, 5L, 4L)), "sorted_ints")
sv(c("aa", "b", "", "cccc", "b"), "words")
sv(c(TRUE, TRUE, FALSE, NA), "flags")
sv(factor(c("a", "b", "a", "c")), "just_a_factor")
sv(factor(c("lo", "hi", "mid"), levels = c("lo", "mid", "hi"), ordered = TRUE), "ranked_factor")
# A factor with more levels than the reader will list. Past fifty it writes a
# window and marks the field it cut, and every factor above was under the line,
# so nothing here reached the marker at all. Sixty levels, each used twice, is
# the narrowest object that cuts both the declared list and the counts.
many <- sprintf("L%02d", 1:60)
sv(factor(rep(many, each = 2L), levels = many), "many_levels")
sv(as.Date("2020-01-01") + c(0, 1, 2, 40), "gappy_dates")
# A broken-down time, kept broken down. Arithmetic on a POSIXlt returns a
# POSIXct, so this has to be written without any, or the object saved here is
# an instant like any other and the field count and year range never come back.
# Two calendar years apart, so the range has two ends.
sv(as.POSIXlt(c("2021-06-01 12:00:00", "2022-07-05 03:00:00"), tz = "UTC"),
   "broken_down_time")

lab <- 1:5
attr(lab, "label") <- "a labelled vector"
comment(lab) <- "and a comment"
attr(lab, "something_else") <- "carried in attrs_other"
sv(lab, "labelled_vec")

if (requireNamespace("units", quietly = TRUE)) {
  sv(units::set_units(c(1, 2, 3), "m"), "with_units")
}

# ---- grids ----------------------------------------------------------------
sv(matrix(c(1, 2, 3, 4, 5, 60), nrow = 2,
          dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))), "named_matrix")
sv(array(1:24, dim = c(2, 3, 4)), "cube")
# A margin with more labels than the reader will list, which is the same cut as
# the level list and is marked inside the dimnames array rather than beside it.
sv(matrix(seq_len(120), nrow = 60,
          dimnames = list(sprintf("r%02d", 1:60), c("left", "right"))),
   "many_labels")
# The awkward numbers a third time, now in something grid shaped. A grid has no
# columns to hang a summary on, so its values are summarised as one set of
# figures for the object, and NaN and the two signed infinities are counted
# separately there and nowhere else.
sv(matrix(c(1, NaN, Inf, -Inf, 0, 2), nrow = 2), "awkward_matrix")
if (requireNamespace("Matrix", quietly = TRUE)) {
  sv(Matrix::sparseMatrix(i = c(1, 2, 3), j = c(3, 1, 2), x = c(1, 2, 3), dims = c(4, 4)), "sparse_cols")
  sv(Matrix::triu(Matrix::Matrix(matrix(1:16, 4), sparse = FALSE)), "upper_triangle")
  # A symmetric sparse matrix stores one triangle and implies the other, so the
  # count of non-zeros is not the count of stored values.
  sv(Matrix::forceSymmetric(Matrix::sparseMatrix(i = c(1, 2, 3), j = c(1, 1, 2),
                                                 x = c(1, 2, 3), dims = c(3, 3))), "sparse_symmetric")
}

# ---- series ---------------------------------------------------------------
sv(ts(1:24, start = c(2000, 1), frequency = 12), "monthly_series")
sv(ts(matrix(1:20, ncol = 2), start = c(2000, 1), frequency = 4), "quarterly_pair")
if (requireNamespace("zoo", quietly = TRUE)) {
  sv(zoo::zoo(1:5, as.Date("2020-01-01") + c(0, 1, 2, 9, 10)), "irregular_zoo")
  # An index that stands still: two observations claim the same instant, so the
  # series has fewer moments than rows and any lookup by time is ambiguous.
  sv(suppressWarnings(zoo::zoo(1:4, as.Date("2020-01-01") + c(0, 1, 1, 2))),
     "repeated_index_zoo")
  # An index that runs backwards. zoo sorts whatever order.by it is handed, so
  # the only way to save an out-of-order series is to write the index on
  # afterwards, which is also how one ends up in the wild: something rewrote
  # the attribute and the ordering the class promises no longer holds.
  backwards <- zoo::zoo(1:4, as.Date("2020-01-01") + 0:3)
  attr(backwards, "index") <- rev(unclass(as.Date("2020-01-01") + 0:3))
  sv(backwards, "backwards_zoo")
}
if (requireNamespace("xts", quietly = TRUE)) {
  sv(xts::xts(1:4, as.POSIXct("2020-01-01", tz = "UTC") + (0:3) * 3600), "hourly_xts")
}

# ---- lists ----------------------------------------------------------------
sv(list(a = 1, b = "two", c = TRUE), "named_list")
sv(list(1, 2, 3), "bare_list")
sv(list(a = 1:3, b = NULL, c = integer(0), d = 1:2), "list_with_gaps")
sv(list(one = data.frame(x = 1:3, y = 4:6), two = data.frame(x = 1:2, y = 3:4)), "list_of_frames")
sv(list(one = data.frame(x = 1:3), two = data.frame(x = 1:2, extra = 3:4)), "list_schema_varies")
sv(list(outer = list(inner = data.frame(x = 1:4))), "nested_frames")

# ---- objects --------------------------------------------------------------
if (requireNamespace("sf", quietly = TRUE)) {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  sv(nc[1:3, c("NAME", "AREA")], "sf_counties")
  pts <- sf::st_sf(id = 1:2,
                   geometry = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326))
  sv(pts, "sf_points")
}
if (requireNamespace("sp", quietly = TRUE)) {
  sp_pts <- sp::SpatialPointsDataFrame(
    coords = cbind(c(1, 2), c(3, 4)), data = data.frame(v = 1:2),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  sv(sp_pts, "sp_points")
}
if (requireNamespace("terra", quietly = TRUE)) {
  r <- terra::rast(nrows = 4, ncols = 4, nlyrs = 2, vals = 1:32)
  sv(terra::wrap(r), "wrapped_raster")
}
if (requireNamespace("raster", quietly = TRUE)) {
  rr <- raster::raster(nrows = 4, ncols = 4, xmn = 0, xmx = 4, ymn = 0, ymx = 4, crs = "EPSG:4326")
  rr[] <- seq_len(16)
  sv(rr, "a_raster")
  # Named layers, a declared no-data sentinel and a computed range, which are
  # what let a reader judge whether the cell figures above mean anything.
  bb <- raster::brick(rr, rr * 2)
  names(bb) <- c("elev", "depth")
  raster::NAvalue(bb) <- -9999
  sv(bb, "named_brick")
}
if (requireNamespace("igraph", quietly = TRUE)) {
  # The one fixture that does not come back byte for byte: an igraph object
  # carries state that differs between sessions. Re-running this script will
  # show it as changed with nothing about the graph having changed, so leave it
  # alone unless the graph itself needs to be different.
  sv(igraph::make_ring(5, directed = TRUE), "ring_graph")
}
setClass("FixtureThing", representation(x = "numeric"))
sv(new("FixtureThing", x = c(1, 2, 3)), "an_s4_object")

# ---- how deeply a table's columns get described ----------------------------
# Past 512 columns the reader stops writing a profile per column, and what it
# writes instead depends on whether the columns are alike. Past its cell cap it
# stops reading values at all and keeps only the structure. Each of the three
# is a different record shape, so all three have to be reachable or the
# writer's handling of them is untested.
#
# Kept as small as the rule allows: 600 columns is the narrowest thing over the
# 512 the reader draws the line at, and eight rows is enough to summarise.
wide <- as.data.frame(matrix(as.numeric(seq_len(600 * 8)), nrow = 8, ncol = 600))
sv(wide, "wide_one_type")

mixed <- wide
for (i in seq(1, 600, by = 3)) mixed[[i]] <- paste0("s", mixed[[i]])
sv(mixed, "wide_mixed_types")

# Past the reader's cell cap, twice over, by the two routes that get there.
#
# A column with bytes: 8,000,001 doubles, materialised, so the reader reads
# their length, hashes the bytes on the way past and skips the values. It comes
# back structural WITH content_fp, schema_fp and shape_fp over those digests,
# which is what lets it take the ordinary path into the contents table. Two
# alternating values compress to about 14 KB under xz, which is what makes an
# eight million row frame something a repository can hold.
sv(data.frame(v = rep(c(0.5, 1.5), length.out = 8000001),
              g = rep(1L, 8000001)),
   "tall_values", compress = "xz")

# A column with no bytes at all: R writes 1:n as a compact sequence, and 9
# million of them are 202 bytes on disk. There is nothing to hash, so this one
# comes back structural with no fingerprint of any kind, and the writer has to
# keep it in the catalog anyway. Nothing here is ever materialised.
sv(data.frame(a = 1:9000000, b = 1:9000000), "unread_values")

# ---- text files under data/ -----------------------------------------------
# data() loads a .csv or a .txt as readily as an .rda, and the extension is a
# promise about the separator that the contents need not keep. The reader says
# which separator would actually work, which is a property of this file.
wl <- function(lines, name) writeLines(lines, file.path(d, name))
wl(c("height,weight,sex", "1.7,65,F", "1.8,80,M"), "comma_sep.csv")
wl(c("height;weight;sex", "1.7;65;F", "1.8;80;M"), "semicolon_sep.csv")
wl(c("grade   sex   score", "    6   M        43", "    7   F        88"), "spaced.txt")

# data() also runs a .R file to build the object it names, which needs R and so
# is something the reader can only report the existence of. The record carries
# no fingerprint, which makes it one of the shapes that keeps its place in the
# catalog with nothing behind it.
wl(c("script_built <- data.frame(a = 1:3, b = c(\"x\", \"y\", \"z\"))"),
   "script_built.R")

# ---- internal, and files that are not data/ -------------------------------
internal_lookup <- data.frame(code = c("a", "b"), value = c(1, 2), stringsAsFactors = FALSE)
save(internal_lookup, file = file.path(root, "R", "sysdata.rda"), version = 3)

writeLines(c("a,b,c", "1,2,3", "4,5,6"), file.path(root, "inst", "extdata", "commas.csv"))
writeLines(c("a\tb", "1\t2"), file.path(root, "inst", "extdata", "tabs.tsv"))
saveRDS(data.frame(x = 1:3), file.path(root, "inst", "extdata", "an_object.rds"), version = 3)

# ---- the package itself ---------------------------------------------------
# A DESCRIPTION is what makes the directory a package the analyzer will read,
# and the help pages are where dataset titles come from.
writeLines(c(
  "Package: bcmfixtures",
  "Version: 0.1.0",
  "Title: Dataset shapes the contract test measures",
  "Description: Not a real package. Every object exists to make the analyzer",
  "    report one family of fields."
), file.path(root, "DESCRIPTION"))

writeLines(c(
  "\\name{every_type}",
  "\\alias{every_type}",
  "\\docType{data}",
  "\\title{One column of every profiled type}",
  "\\description{A frame carrying each column type the profiler reports on.}",
  "\\keyword{datasets}"
), file.path(root, "man", "every_type.Rd"))

cat("fixtures written:", length(list.files(d)), "under data/\n")
