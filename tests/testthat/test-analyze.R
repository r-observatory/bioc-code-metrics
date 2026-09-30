test_that("analyze_version returns flat named list with structure columns", {
  file_map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/foo.R"     = "foo <- function() 1\n"
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")

  result <- analyze_version(ctx)

  expect_true(is.list(result))
  # Structure metrics must be present
  expect_true("n_files"       %in% names(result))
  expect_true("loc_total"     %in% names(result))
  expect_true("loc_r"         %in% names(result))
  expect_true("has_src"       %in% names(result))
  expect_true("compiled_share" %in% names(result))
  expect_true("lang_breakdown" %in% names(result))
  # All values are scalar
  expect_true(all(vapply(result, length, integer(1L)) == 1L))
})

test_that("analyze_version: a failing group emits warning, yields NA sentinel, does not abort", {
  file_map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "R/a.R"       = "a <- 1\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")

  # Register a deliberately failing group in a local copy of the registry
  old_groups <- METRIC_GROUPS
  METRIC_GROUPS[["fail_test"]] <<- function(ctx) stop("deliberate failure")
  on.exit(METRIC_GROUPS[["fail_test"]] <<- NULL, add = TRUE)

  result <- withCallingHandlers(
    analyze_version(ctx),
    warning = function(w) {
      expect_true(grepl("fail_test", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )

  # Must return a list (not abort)
  expect_true(is.list(result))

  # Sentinel NA entry for the failed group
  sentinel_name <- ".error.fail_test"
  expect_true(sentinel_name %in% names(result))
  expect_true(is.na(result[[sentinel_name]]))

  # Successful groups still present
  expect_true("n_files" %in% names(result))
})

test_that("analyze_version: unknown/empty group registry still returns list", {
  file_map <- list("R/a.R" = "a <- 1\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")

  old_groups <- METRIC_GROUPS
  METRIC_GROUPS <<- list()
  on.exit(METRIC_GROUPS <<- old_groups, add = TRUE)

  result <- analyze_version(ctx)
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})

test_that("analyze_package produces summary/churn/api data.frames from a local repo", {
  repo <- tempfile("bcm_ap_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "config", "user.email", "t@t.test"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "config", "user.name", "T"),
          stdout = FALSE, stderr = FALSE)
  writeLines("# readme", file.path(repo, "README"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "init"),
          stdout = FALSE, stderr = FALSE)

  # RELEASE_1_0 branch
  system2("git", c("-C", repo, "checkout", "-b", "RELEASE_1_0"),
          stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines(c("foo <- function() 1"), file.path(repo, "R", "foo.R"))
  writeLines("Package: mypkg\nVersion: 1.0\n", file.path(repo, "DESCRIPTION"))
  writeLines("export(foo)\n", file.path(repo, "NAMESPACE"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "release-1.0"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "checkout", "-"),
          stdout = FALSE, stderr = FALSE)

  # RELEASE_1_1 branch
  system2("git", c("-C", repo, "checkout", "-b", "RELEASE_1_1"),
          stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines(c("foo <- function() 1", "bar <- function() 2"),
             file.path(repo, "R", "foo.R"))
  writeLines("Package: mypkg\nVersion: 1.1\n", file.path(repo, "DESCRIPTION"))
  writeLines("export(foo)\nexport(bar)\n", file.path(repo, "NAMESPACE"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "release-1.1"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "checkout", "-"),
          stdout = FALSE, stderr = FALSE)

  result <- analyze_package(repo, "mypkg")

  # Three output components
  expect_true(all(c("summary", "churn", "api") %in% names(result)))

  # summary: two version rows
  expect_s3_class(result$summary, "data.frame")
  expect_equal(nrow(result$summary), 2L)
  expect_true("package" %in% colnames(result$summary))
  expect_true("version" %in% colnames(result$summary))

  # api: two rows, exports_added is JSON
  expect_s3_class(result$api, "data.frame")
  expect_equal(nrow(result$api), 2L)
  # First version: foo added; second: bar added (foo already present)
  v1_api <- result$api[result$api$version == "1.0", ]
  v2_api <- result$api[result$api$version == "1.1", ]
  expect_true(grepl("foo", v1_api$exports_added))
  expect_true(grepl("bar", v2_api$exports_added))

  # churn data.frame has package column
  expect_s3_class(result$churn, "data.frame")
  expect_true("package" %in% colnames(result$churn))
})

test_that("a cap inside one R-fallback group runs the group again rather than NA", {
  file_map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nImports: stats\n",
    "NAMESPACE"   = "export(f)\n",
    "R/f.R"       = "f <- function(x) x + 1\n"
  )
  mk  <- function() build_context("p", "1.0", "1.0", "2024-01-01",
                                  names(file_map), function(p) file_map[[p]] %||% "")
  want <- analyze_version(mk())
  groups <- METRIC_GROUPS
  groups$meta <- .fires_cap_once(groups$meta)
  .local_global("METRIC_GROUPS", groups)
  expect_identical(analyze_version(mk()), want)
})

# ---------------------------------------------------------------------------
# A cap on the first file a group reads, for every R-fallback group
# ---------------------------------------------------------------------------

.sweep_map <- list(
  "DESCRIPTION" = paste0(
    "Package: p\nVersion: 1.0\nTitle: P\nDescription: A package.\n",
    "Depends: R (>= 4.1)\nImports: stats\nSuggests: testthat\n",
    "Config/testthat/edition: 3\nLicense: MIT\nSystemRequirements: zlib\n"),
  "NAMESPACE"   = "export(f)\nexportPattern(\"^g\")\n",
  "R/f.R"       = paste0("#' F\n#' @export\nf <- function(x) {\n  eval(substitute(x))\n}\n",
                         "g1 <- function() stats:::median.default(1)\n"),
  "tests/testthat/test-f.R" = "test_that('f', expect_equal(f(1), 1))\n",
  "README.md"   = "# p\n\nInstall with `install.packages('p')`.\n",
  "NEWS.md"     = "# p 1.0\n\n* First.\n",
  "src/Makevars" = "PKG_CFLAGS = -O3\n",
  "vignettes/intro.Rmd" = paste0("---\ntitle: Intro\nvignette: >\n",
                                 "  %\\VignetteEngine{knitr::rmarkdown}\n---\n```{r}\n1\n```\n"))

.sweep_ctx <- function() {
  build_context("p", "1.0", "1.0", "2024-01-01", names(.sweep_map),
                function(p) .sweep_map[[p]] %||% "")
}

for (nm in c("structure", "functions", "docs", "tests", "security", "health", "portability")) {
  test_that(sprintf("a cap on the first file the %s group reads leaves its metrics unchanged", nm), {
    .local_global("METRIC_GROUPS", METRIC_GROUPS[nm])
    want <- analyze_version(.sweep_ctx())
    ctx <- .sweep_ctx()
    fired <- FALSE
    cap_first <- function(f) {
      force(f)
      function(p) {
        if (!fired) {
          fired <<- TRUE
          stop(.cap_error())
        }
        f(p)
      }
    }
    ctx$read  <- cap_first(ctx$read)
    ctx$lines <- cap_first(ctx$lines)
    expect_identical(analyze_version(ctx), want)
    expect_true(fired)
  })
}
