# tests/testthat/test-upsert-versions.R: replacing a package's rows release by
# release when some of its releases are not read.

.UV_RELEASED <- c("1.0" = "2026-01-01", "1.1" = "2026-02-01", "1.2" = "2026-03-01",
                  "1.3" = "2026-04-01", "2.0" = "2025-01-01")

# Cross-version columns as add_cross_version_metrics leaves them over a listing:
# the package-level ones on its newest row only, the detail marker on every row.
.uv_xv <- function(v) {
  n <- length(v)
  last <- seq_len(n) == n
  data.frame(
    bump_type = c("initial", rep("minor", n - 1L)), exports_added_n = 1L,
    exports_removed_n = 0L, is_breaking = FALSE, bump_fidelity_ok = c(NA, rep(TRUE, n - 1L)),
    n_versions = ifelse(last, n, NA_integer_),
    first_release_date = ifelse(last, .UV_RELEASED[[v[[1L]]]], NA_character_),
    latest_release_date = ifelse(last, .UV_RELEASED[[v[[n]]]], NA_character_),
    release_cadence_days = ifelse(last, 30, NA_real_),
    dependency_drift = ifelse(last, 0L, NA_integer_),
    authors_added_later = ifelse(last, 0L, NA_integer_), cold_removal_rate = NA_real_,
    deprecation_infrastructure_maturity = ifelse(last, 0L, NA_integer_),
    detail_scanned = TRUE, datasets_scanned = ifelse(last, TRUE, NA),
    stringsAsFactors = FALSE)
}

.uv_summary <- function(pkg, v, xv, assessed = "2026-09-01", loc = NULL) {
  loc <- loc %||% as.integer(round(as.numeric(v) * 10))
  cbind(data.frame(package = pkg, version = v, released = unname(.UV_RELEASED[v]),
                   loc_r = as.integer(loc), n_fns_r = 3L, assessed_at = assessed,
                   stringsAsFactors = FALSE), xv)
}

.uv_churn <- function(v, file, added, pkg = "p") {
  data.frame(package = pkg, version = v, file = file, added = as.integer(added),
             deleted = 0L, stringsAsFactors = FALSE)
}

.uv_api <- function(v, added, n, pkg = "p") {
  data.frame(package = pkg, version = v, exports_added = added, exports_removed = "[]",
             n_exports = as.integer(n), stringsAsFactors = FALSE)
}

.uv_functions <- function(v, name, pkg = "p") {
  data.frame(package = pkg, version = v, lang = "R", name = name, exported = TRUE,
             file = "R/a.R", line = 1L, loc = 5L, n_params = 1L, cyclocomp = 1L,
             stringsAsFactors = FALSE)
}

.uv_edges <- function(v, from, pkg = "p") {
  data.frame(package = pkg, version = v, graph = "calls", from = from, to = "f0",
             stringsAsFactors = FALSE)
}

.uv_state <- function(v, commit, prev_version, prev_commit, read_at, generation = 1L,
                      pkg = "p") {
  data.frame(package = pkg, version = v, commit_sha = commit, tree_sha = sub("c", "t", commit),
             prev_version = prev_version, prev_commit = prev_commit, deprecated = "[]",
             uses_lifecycle = 0L, read_at = read_at, rows_generation = as.integer(generation),
             stringsAsFactors = FALSE)
}

.uv_description <- function(v, value, pkg = "p") {
  data.frame(package = pkg, version = v, field = "Title", value = value,
             value_truncated = 0L, stringsAsFactors = FALSE)
}

.uv_notes <- function(v, pkg = "p") {
  data.frame(package = pkg, version = v, package_version = v, news_file = "NEWS.md",
             release_notes_source = "news", release_notes = paste("notes", v),
             release_notes_truncated = 0L, stringsAsFactors = FALSE)
}

.uv_datasets <- function(v, content, current, pkg = "p") {
  data.frame(package = pkg, name = "d1", version = v, file = "data/d1.rda", internal = 0L,
             format = "rda", compression = "gzip", confidence = "high",
             class = "data.frame", kind = "table", nrow = 10L, ncol = 2L,
             n_missing_total = 0L, content_fp = content, schema_fp = "sf1",
             fp_algo_version = 1L, columns = '["a","b"]', row_sketch = '{"a":[1]}',
             is_current = as.integer(current), stringsAsFactors = FALSE)
}

.uv_keys <- function(v, pkg = "p") data.frame(package = rep(pkg, length(v)), version = v,
                                              stringsAsFactors = FALSE)

# Each of `pkgs` stored at 1.0, 1.1 and 1.2, with detail for every release, and
# package q at 2.0, all written by upsert_shard, in a fresh code and data
# database under one directory. Returns an environment holding the connections
# and readers of what they hold.
.uv_fixture <- function(pkgs = "p", env = parent.frame()) {
  fx <- new.env()
  fx$out_dir <- withr::local_tempdir(.local_envir = env)
  fx$con  <- open_or_init_db(file.path(fx$out_dir, DB_FILENAME))
  fx$dcon <- open_or_init_data_db(file.path(fx$out_dir, DATA_DB_FILENAME))
  withr::defer({
    DBI::dbDisconnect(fx$con)
    DBI::dbDisconnect(fx$dcon)
  }, envir = env)

  v <- c("1.0", "1.1", "1.2")
  seed <- function(pkg) list(
    summary = .uv_summary(pkg, v, .uv_xv(v)),
    churn = .uv_churn(v, c("R/a.R", "R/a.R", "R/b.R"), c(10, 1, 2), pkg),
    api = .uv_api(v, c('["a"]', '["b"]', "[]"), c(1, 2, 2), pkg),
    functions = .uv_functions(v, c("f0", "f1", "f2"), pkg),
    edges = .uv_edges(v, c("f0", "f1", "f2"), pkg),
    description = .uv_description("1.2", "Old", pkg),
    notes = .uv_notes("1.2", pkg),
    state = .uv_state(v, c("c1", "c2", "c3"), c(NA, "1.0", "1.1"), c(NA, "c1", "c2"),
                      "2026-09-01T00:00:00Z", pkg = pkg),
    datasets = .uv_datasets(v, c("cf1", "cf2", "cf3"), c(0, 0, 1), pkg))
  frames <- lapply(pkgs, seed)
  bind <- function(f) do.call(rbind, lapply(frames, `[[`, f))
  q <- .uv_summary("q", "2.0", .uv_xv("2.0"))
  upsert_shard(fx$con, rbind(bind("summary"), q),
               churn_df = bind("churn"), api_df = bind("api"),
               functions_df = bind("functions"), edges_df = bind("edges"),
               description_df = bind("description"), release_notes_df = bind("notes"),
               state_df = rbind(bind("state"),
                                .uv_state("2.0", "c9", NA, NA, "2026-09-01T00:00:00Z",
                                          pkg = "q")))
  upsert_datasets(fx$dcon, bind("datasets"), pkgs)

  query <- function(sql, ...) DBI::dbGetQuery(fx$con, sql, params = list(...))
  fx$summary_versions <- function() {
    query("SELECT version FROM bioc_code_summary WHERE package = ? ORDER BY version", "p")$version
  }
  fx$row <- function(v) query("SELECT * FROM bioc_code_summary WHERE package = ? AND version = ?", "p", v)
  fx$api_versions <- function() {
    query("SELECT version FROM bioc_api_history WHERE package = ? ORDER BY version", "p")$version
  }
  fx$api_row <- function(v) query("SELECT * FROM bioc_api_history WHERE package = ? AND version = ?", "p", v)
  fx$state_versions <- function() {
    query("SELECT version FROM bioc_version_state WHERE package = ? ORDER BY version", "p")$version
  }
  fx$state_row <- function(v) query("SELECT * FROM bioc_version_state WHERE package = ? AND version = ?", "p", v)
  fx$churn_rows <- function() {
    query("SELECT package, version, file, added, deleted FROM bioc_code_churn
         WHERE package = ? ORDER BY version, file", "p")
  }
  fx$detail_rows <- function(table) {
    query(sprintf('SELECT * FROM "%s" WHERE package = ? ORDER BY version', table), "p")
  }
  fx$desc_fields <- function(pkg) {
    query(sprintf('SELECT * FROM "%s" WHERE package = ?', DESCRIPTION_FIELDS_TABLE), pkg)
  }
  fx$notes_row <- function(pkg) {
    query(sprintf('SELECT * FROM "%s" WHERE package = ?', RELEASE_NOTES_TABLE), pkg)
  }
  dquery <- function(sql) DBI::dbGetQuery(fx$dcon, sql, params = list("p"))
  fx$links <- function() {
    dquery("SELECT l.version, l.is_current, c.content_fp FROM bioc_dataset_versions l
          LEFT JOIN bioc_dataset_contents c USING (content_id)
          WHERE l.package = ? ORDER BY l.version")
  }
  fx$current_link_versions <- function() {
    dquery("SELECT version FROM bioc_dataset_versions WHERE package = ? AND is_current = 1
          ORDER BY version")$version
  }
  fx$identity <- function() {
    dquery("SELECT d.*, c.content_fp FROM bioc_datasets d
          LEFT JOIN bioc_dataset_contents c ON c.content_id = d.current_content_id
          WHERE d.package = ?")
  }
  # Every table of the code database, its schema and all its rows.
  fx$all_rows <- function() {
    out <- list(schema = DBI::dbGetQuery(fx$con,
      "SELECT type, name, sql FROM sqlite_master ORDER BY type, name"))
    for (tbl in sort(DBI::dbListTables(fx$con))) {
      cols <- DBI::dbListFields(fx$con, tbl)
      out[[tbl]] <- DBI::dbGetQuery(fx$con, sprintf('SELECT * FROM "%s" ORDER BY %s', tbl,
                                                    paste(sprintf('"%s"', cols), collapse = ", ")))
    }
    out
  }
  # The dataset database by content rather than by the ids it happens to assign.
  fx$data_rows <- function() {
    rows <- function(sql) DBI::dbGetQuery(fx$dcon, sql)
    list(
      links = rows("SELECT l.package, l.name, l.version, l.is_current, c.content_fp
                   FROM bioc_dataset_versions l LEFT JOIN bioc_dataset_contents c USING (content_id)
                   ORDER BY l.package, l.name, l.version"),
      identity = rows("SELECT d.package, d.name, d.current_version, c.content_fp
                      FROM bioc_datasets d
                      LEFT JOIN bioc_dataset_contents c ON c.content_id = d.current_content_id
                      ORDER BY d.package, d.name"),
      contents = rows("SELECT content_fp FROM bioc_dataset_contents ORDER BY content_fp"))
  }
  fx
}

# 1.3 read, 1.0 gone, 1.1 and 1.2 skipped, 1.1's diff redone (its predecessor
# 1.0 is gone): the listing is 1.1, 1.2, 1.3. The frames hold what the walk
# gives: the read release's rows, and the redone diff's API and churn rows.
.uv_newest_read <- function(pkg = "p") {
  v <- c("1.1", "1.2", "1.3")
  xv <- .uv_xv(v)
  xv$exports_added_n[[1L]] <- 2L
  list(
    summary_df = .uv_summary(pkg, "1.3", xv[3L, ], assessed = "2026-10-20"),
    derived_df = data.frame(package = pkg, version = v[1:2], xv[1:2, ],
                            stringsAsFactors = FALSE, row.names = NULL),
    churn_df = .uv_churn(c("1.1", "1.1", "1.3"), c("R/a.R", "R/b.R", "R/c.R"),
                         c(11, 2, 7), pkg),
    api_df = .uv_api(c("1.1", "1.3"), c('["a","b"]', '["c"]'), c(2, 3), pkg),
    functions_df = .uv_functions("1.3", "f3", pkg),
    edges_df = .uv_edges("1.3", "f3", pkg),
    state_df = rbind(.uv_state("1.3", "c4", "1.2", "c3", "2026-10-20T00:00:00Z",
                               ROWS_GENERATION, pkg),
                     .uv_state("1.1", "c2", NA, NA, "2026-09-01T00:00:00Z", pkg = pkg)),
    row_keys = .uv_keys(c("1.3", "1.0"), pkg),
    api_keys = .uv_keys(c("1.3", "1.0", "1.1"), pkg),
    newest = stats::setNames("1.3", pkg),
    latest_pkgs = pkg,
    description_df = .uv_description("1.3", "New", pkg),
    release_notes_df = .uv_notes("1.3", pkg))
}

# 1.0 gone, 1.1's diff redone, the newest 1.2 skipped: nothing read, and the
# frames hold no row of the newest.
.uv_newest_skipped <- function(pkg = "p") {
  v <- c("1.1", "1.2")
  xv <- .uv_xv(v)
  xv$exports_added_n[[1L]] <- 2L
  list(
    summary_df = .uv_summary(pkg, "1.3", .uv_xv("1.3"))[0L, ],
    derived_df = data.frame(package = pkg, version = v, xv, stringsAsFactors = FALSE),
    churn_df = .uv_churn(c("1.1", "1.1"), c("R/a.R", "R/b.R"), c(11, 2), pkg),
    api_df = .uv_api("1.1", '["a","b"]', 2, pkg),
    functions_df = .empty_functions_df(),
    edges_df = .empty_edges_df(),
    state_df = .uv_state("1.1", "c2", NA, NA, "2026-09-01T00:00:00Z", pkg = pkg),
    row_keys = .uv_keys("1.0", pkg),
    api_keys = .uv_keys(c("1.0", "1.1"), pkg),
    newest = stats::setNames("1.2", pkg),
    latest_pkgs = character(0L),
    description_df = .uv_description("1.2", "Not this one", pkg),
    release_notes_df = .uv_notes("1.2", pkg))
}

# 1.1 moved and read again, 1.2's diff redone from the moved tip, 1.0 and the
# newest 1.2 skipped.
.uv_moved <- function() {
  v <- c("1.0", "1.1", "1.2")
  xv <- .uv_xv(v)
  list(
    summary_df = .uv_summary("p", "1.1", xv[2L, ], assessed = "2026-10-20", loc = 99L),
    derived_df = data.frame(package = "p", version = v[c(1L, 3L)], xv[c(1L, 3L), ],
                            stringsAsFactors = FALSE, row.names = NULL),
    churn_df = .uv_churn(c("1.1", "1.2"), c("R/c.R", "R/d.R"), c(4, 6)),
    api_df = .uv_api(c("1.1", "1.2"), c('["e"]', "[]"), c(3, 3)),
    functions_df = .uv_functions("1.1", "f1b"),
    edges_df = .uv_edges("1.1", "f1b"),
    state_df = rbind(.uv_state("1.1", "c2b", "1.0", "c1", "2026-10-20T00:00:00Z",
                               ROWS_GENERATION),
                     .uv_state("1.2", "c3", "1.1", "c2b", "2026-09-01T00:00:00Z")),
    row_keys = .uv_keys("1.1"),
    api_keys = .uv_keys(c("1.1", "1.2")),
    newest = c(p = "1.2"),
    latest_pkgs = character(0L),
    description_df = .uv_description("1.2", "Not this one"),
    release_notes_df = .uv_notes("1.2"))
}

.uv_write <- function(fx, args) do.call(upsert_versions, c(list(fx$con), args))

.uv_no_rownames <- function(df) {
  rownames(df) <- NULL
  df
}

test_that("upsert_versions replaces the summary rows of the releases read and gone, and only those", {
  fx <- .uv_fixture()
  before <- fx$all_rows()$bioc_code_summary
  .uv_write(fx, .uv_newest_read())
  expect_identical(fx$summary_versions(), c("1.1", "1.2", "1.3"))
  expect_identical(fx$row("1.3")$loc_r, 13L)
  expect_identical(fx$row("1.3")$assessed_at, "2026-10-20")
  # Another package's rows are not touched.
  after <- fx$all_rows()$bioc_code_summary
  expect_identical(.uv_no_rownames(after[after$package == "q", ]),
                   .uv_no_rownames(before[before$package == "q", ]))
})

test_that("skipped rows get their cross-version columns in place and keep every other column", {
  fx <- .uv_fixture()
  args <- .uv_newest_read()
  before <- list("1.1" = fx$row("1.1"), "1.2" = fx$row("1.2"))
  .uv_write(fx, args)
  r <- fx$row("1.2")
  expect_true(is.na(r$latest_release_date)); expect_identical(r$assessed_at, "2026-09-01")
  expect_identical(r$loc_r, before[["1.2"]]$loc_r)
  derived <- .coerce_logicals(args$derived_df)
  for (v in c("1.1", "1.2")) {
    got  <- fx$row(v)
    keep <- setdiff(names(got), .CROSS_VERSION_COLS)
    expect_identical(got[keep], before[[v]][keep], info = v)
    want <- .uv_no_rownames(derived[derived$version == v, .CROSS_VERSION_COLS])
    expect_identical(got[.CROSS_VERSION_COLS], want, info = v)
  }
  expect_identical(fx$row("1.1")$bump_type, "initial")
  expect_identical(fx$row("1.1")$exports_added_n, 2L)
})

test_that("API and state rows are replaced for the releases read, gone or with a redone diff", {
  fx <- .uv_fixture()
  before_api_12   <- fx$api_row("1.2")
  before_state_12 <- fx$state_row("1.2")
  .uv_write(fx, .uv_newest_read())
  expect_identical(fx$api_versions(), c("1.1", "1.2", "1.3"))
  expect_identical(fx$api_row("1.1")$exports_added, '["a","b"]')       # the redone diff, first release now
  expect_identical(fx$api_row("1.3")$exports_added, '["c"]')
  expect_identical(fx$api_row("1.2"), before_api_12)
  expect_identical(fx$state_versions(), c("1.1", "1.2", "1.3"))
  expect_identical(fx$state_row("1.1")$prev_version, NA_character_)
  expect_identical(fx$state_row("1.1")$commit_sha, "c2")
  expect_identical(fx$state_row("1.2"), before_state_12)
  expect_identical(fx$state_row("1.2")$read_at, "2026-09-01T00:00:00Z")
  expect_identical(fx$state_row("1.3")$rows_generation, ROWS_GENERATION)
  expect_identical(fx$state_row("1.3")$read_at, "2026-10-20T00:00:00Z")
})

test_that("churn, functions and edges are replaced for the releases read or gone, and churn also for a redone diff", {
  fx <- .uv_fixture()
  args <- .uv_newest_read()
  before_churn <- fx$churn_rows()
  before_fns   <- fx$detail_rows("bioc_functions")
  before_edges <- fx$detail_rows("bioc_call_edges")
  .uv_write(fx, args)
  # 1.0 gone, 1.1 redone and 1.3 read take the write's churn; skipped 1.2 keeps its own.
  want <- rbind(args$churn_df, before_churn[before_churn$version == "1.2", ])
  expect_identical(fx$churn_rows(), .uv_no_rownames(want[order(want$version, want$file), ]))
  # 1.0 gone and 1.3 read; 1.1 and 1.2 skipped, a redone diff included, keep theirs.
  for (tbl in c("bioc_functions", "bioc_call_edges")) {
    before <- if (tbl == "bioc_functions") before_fns else before_edges
    got <- fx$detail_rows(tbl)
    expect_identical(got$version, c("1.1", "1.2", "1.3"), info = tbl)
    expect_identical(.uv_no_rownames(got[got$version %in% c("1.1", "1.2"), ]),
                     .uv_no_rownames(before[before$version %in% c("1.1", "1.2"), ]), info = tbl)
  }
  expect_identical(fx$detail_rows("bioc_functions")$name[[3L]], "f3")
  expect_identical(fx$detail_rows("bioc_call_edges")$from[[3L]], "f3")

  # A stored release read again: its rows are replaced, not added to.
  fx <- .uv_fixture()
  args <- .uv_moved()
  before_churn <- fx$churn_rows()
  before_fns   <- fx$detail_rows("bioc_functions")
  before_edges <- fx$detail_rows("bioc_call_edges")
  .uv_write(fx, args)
  want <- rbind(before_churn[before_churn$version == "1.0", ], args$churn_df)
  expect_identical(fx$churn_rows(), .uv_no_rownames(want[order(want$version, want$file), ]))
  expect_identical(fx$detail_rows("bioc_functions"),
                   .uv_no_rownames(rbind(before_fns[before_fns$version == "1.0", ],
                                         .coerce_logicals(args$functions_df),
                                         before_fns[before_fns$version == "1.2", ])))
  expect_identical(fx$detail_rows("bioc_call_edges"),
                   .uv_no_rownames(rbind(before_edges[before_edges$version == "1.0", ],
                                         args$edges_df,
                                         before_edges[before_edges$version == "1.2", ])))
})

test_that("the latest-only text is replaced only where the newest was read", {
  fx <- .uv_fixture()
  .uv_write(fx, .uv_newest_read())
  expect_identical(unique(fx$desc_fields("p")$version), "1.3")
  expect_identical(fx$desc_fields("p")$value, "New")
  expect_identical(fx$notes_row("p")$version, "1.3")

  fx <- .uv_fixture()
  before_desc  <- fx$desc_fields("p")
  before_notes <- fx$notes_row("p")
  .uv_write(fx, .uv_newest_skipped())
  expect_identical(fx$desc_fields("p"), before_desc)
  expect_identical(fx$notes_row("p"), before_notes)
  expect_identical(before_desc$value, "Old")
})

test_that("a per-version write needs its state rows", {
  fx <- .uv_fixture()
  before <- fx$all_rows()
  args <- .uv_newest_read()
  args$state_df <- NULL
  expect_error(.uv_write(fx, args), "state_df")
  expect_error(.uv_write(fx, c(args, list(state_df = NULL))), "state_df")
  expect_identical(fx$all_rows(), before)
})

test_that("a per-version write refuses keys that are not frames and changes nothing", {
  fx <- .uv_fixture()
  before <- fx$all_rows()
  before_data <- fx$data_rows()
  args <- .uv_newest_read()
  expect_error(.uv_write(fx, utils::modifyList(args, list(row_keys = NULL), keep.null = TRUE)),
               "row_keys")
  expect_error(.uv_write(fx, utils::modifyList(args, list(api_keys = NULL), keep.null = TRUE)),
               "api_keys")
  expect_identical(fx$all_rows(), before)
  # NULL keys would replace each package's links whole.
  expect_error(upsert_dataset_versions(fx$dcon, .uv_datasets("1.3", "cf4", 1), row_keys = NULL,
                                       newest = c(p = "1.3"), latest_pkgs = "p"),
               "row_keys")
  expect_identical(fx$data_rows(), before_data)
})

test_that("a per-version write that fails leaves every table as it was", {
  fx <- .uv_fixture()
  before <- fx$all_rows()
  args <- .uv_newest_read()
  # A state row for 1.2, whose stored state row the write keeps: the last
  # append clashes with the table's primary key, after every other step ran.
  args$state_df <- rbind(args$state_df,
                         .uv_state("1.2", "c3", "1.1", "c2", "2026-10-20T00:00:00Z"))
  expect_error(.uv_write(fx, args), "UNIQUE constraint failed")
  expect_identical(fx$all_rows(), before)
})

test_that("a release read without a state row is left with none, so the next plan reads it as unrecorded", {
  fx <- .uv_fixture()
  args <- .uv_moved()
  # What the walk gives when 1.1's churn diff did not finish: no state row for it.
  args$state_df <- args$state_df[args$state_df$version != "1.1", ]
  .uv_write(fx, args)
  expect_identical(fx$row("1.1")$loc_r, 99L)
  expect_identical(fx$state_versions(), c("1.0", "1.2"))
  expect_identical(fx$state_row("1.2")$prev_commit, "c2b")
  prior <- .prior_state(fx$con, fx$con, "p", NA_character_)[["p"]]
  listing <- data.frame(version = c("1.0", "1.1", "1.2"), commit = c("c1", "c2b", "c3"),
                        stringsAsFactors = FALSE)
  plan <- .read_plan(listing, c("t1", "t2b", "t3"), prior)
  expect_identical(plan$reason, c(NA, "unrecorded", NA))
  expect_identical(plan$read, c(FALSE, TRUE, FALSE))
})

test_that("upsert_dataset_versions replaces the links of the releases read or gone and keeps the identity unless the newest was read", {
  # The newest read with 1.1 read again: 1.3's link is the current one and the
  # identity follows it, not the older release read beside it.
  fx <- .uv_fixture()
  upsert_dataset_versions(fx$dcon, rbind(.uv_datasets("1.1", "cf2b", 0),
                                         .uv_datasets("1.3", "cf4", 1)),
                          row_keys = .uv_keys(c("1.3", "1.1", "1.0")), newest = c(p = "1.3"),
                          latest_pkgs = "p")
  expect_identical(fx$links()$version, c("1.1", "1.2", "1.3"))
  expect_identical(fx$links()$content_fp, c("cf2b", "cf3", "cf4"))
  expect_identical(fx$current_link_versions(), "1.3")
  expect_identical(fx$identity()$current_version, "1.3")
  expect_identical(fx$identity()$content_fp, "cf4")
  # 1.0's and the old 1.1's profiles are referenced by nothing now.
  expect_identical(fx$data_rows()$contents$content_fp, c("cf2b", "cf3", "cf4"))

  # The newest skipped, 1.1 read again and 1.0 gone: the identity stays as it was.
  fx <- .uv_fixture()
  before_identity <- fx$identity()
  upsert_dataset_versions(fx$dcon, .uv_datasets("1.1", "cf2b", 0),
                          row_keys = .uv_keys(c("1.1", "1.0")), newest = c(p = "1.2"),
                          latest_pkgs = character(0L))
  expect_identical(fx$links()$version, c("1.1", "1.2"))
  expect_identical(fx$links()$content_fp, c("cf2b", "cf3"))
  expect_identical(fx$current_link_versions(), "1.2")
  expect_identical(fx$identity(), before_identity)
  expect_identical(before_identity$current_version, "1.2")
})

test_that("a write of two packages at once equals the two written apart", {
  p <- .uv_newest_read("p")
  r <- .uv_newest_skipped("r")
  both <- lapply(stats::setNames(nm = names(p)), function(f) {
    if (f %in% c("newest", "latest_pkgs")) c(p[[f]], r[[f]]) else rbind(p[[f]], r[[f]])
  })
  ds_p <- .uv_datasets("1.3", "cf4", 1, "p")
  # Nothing of r is read, so it brings no dataset rows.
  ds_r <- .uv_datasets("1.1", "cf2b", 0, "r")[0L, ]

  together <- .uv_fixture(c("p", "r"))
  expect_identical(both$newest, c(p = "1.3", r = "1.2"))
  expect_identical(both$latest_pkgs, "p")
  upsert_dataset_versions(together$dcon, rbind(ds_p, ds_r), both$row_keys, both$newest,
                          both$latest_pkgs)
  .uv_write(together, both)

  apart <- .uv_fixture(c("p", "r"))
  upsert_dataset_versions(apart$dcon, ds_p, p$row_keys, p$newest, p$latest_pkgs)
  upsert_dataset_versions(apart$dcon, ds_r, r$row_keys, r$newest, r$latest_pkgs)
  .uv_write(apart, p)
  .uv_write(apart, r)

  expect_identical(together$all_rows(), apart$all_rows())
  expect_identical(together$data_rows(), apart$data_rows())
  # Each package took its own write: p's newest is 1.3, r keeps 1.2's text.
  rows <- together$all_rows()$bioc_code_summary
  expect_identical(rows$version[rows$package == "p"], c("1.1", "1.2", "1.3"))
  expect_identical(rows$version[rows$package == "r"], c("1.1", "1.2"))
  expect_identical(together$desc_fields("r")$value, "Old")
  links <- together$data_rows()$links
  expect_identical(links$version[links$is_current == 1L], c("1.3", "1.2"))
})

test_that("the version helpers do nothing on a missing table or no keys", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_silent(.delete_by_version(con, "absent", .uv_keys("1.0")))
  expect_silent(.mark_current_versions(con, "absent", c(p = "1.0")))
  DBI::dbWriteTable(con, "t", data.frame(package = "p", version = c("1.0", "1.1"),
                                         is_current = c(1L, 1L)))
  .delete_by_version(con, "t", .uv_keys(character(0L)))
  .mark_current_versions(con, "t", character(0L))
  expect_identical(DBI::dbReadTable(con, "t")$is_current, c(1L, 1L))
  .mark_current_versions(con, "t", c(p = "1.1"))
  .delete_by_version(con, "t", .uv_keys("1.1"))
  expect_identical(DBI::dbReadTable(con, "t"),
                   data.frame(package = "p", version = "1.0", is_current = 0L))
})
