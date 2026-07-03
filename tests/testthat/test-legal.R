# Helper: build a context from an in-memory path -> content map.
make_ctx <- function(map, ...) {
  build_context(
    "testpkg", "1.0", "v1.0", "2024-01-01",
    names(map),
    function(p) map[[p]] %||% "",
    ...
  )
}

# ---------------------------------------------------------------------------
# license
# ---------------------------------------------------------------------------

test_that("metrics_legal: license returns the raw DESCRIPTION License string", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n")
  m   <- metrics_legal(make_ctx(map))
  expect_equal(m$license, "MIT + file LICENSE")
})

test_that("metrics_legal: license is NA when DESCRIPTION has no License field", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  m   <- metrics_legal(make_ctx(map))
  expect_equal(m$license, NA_character_)
})

test_that("metrics_legal: license is NA when License field is blank", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense:   \n")
  m   <- metrics_legal(make_ctx(map))
  expect_equal(m$license, NA_character_)
})

test_that("metrics_legal: license value is trimmed", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense:  GPL-2  \n")
  m   <- metrics_legal(make_ctx(map))
  expect_equal(m$license, "GPL-2")
})

# ---------------------------------------------------------------------------
# spdx_valid
# ---------------------------------------------------------------------------

test_that("metrics_legal: spdx_valid TRUE for a standard SPDX identifier", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: GPL-2\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)
})

test_that("metrics_legal: spdx_valid TRUE for MIT + file LICENSE (strips suffix)", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)
})

test_that("metrics_legal: spdx_valid TRUE for pipe-separated alternatives", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: GPL-2 | GPL-3\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)
})

test_that("metrics_legal: spdx_valid TRUE for GPL (>= 2) version-range form", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: GPL (>= 2)\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)
})

test_that("metrics_legal: spdx_valid TRUE for standalone file LICENSE", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: file LICENSE\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)
})

test_that("metrics_legal: spdx_valid FALSE for an unrecognized license", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: Custom License\n")
  m   <- metrics_legal(make_ctx(map))
  expect_false(m$spdx_valid)
})

test_that("metrics_legal: spdx_valid FALSE when only one alternative is unknown", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: GPL-2 | Custom\n")
  m   <- metrics_legal(make_ctx(map))
  expect_false(m$spdx_valid)
})

test_that("metrics_legal: spdx_valid NA when License is absent", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(is.na(m$spdx_valid))
})

# ---------------------------------------------------------------------------
# osi_approved
# ---------------------------------------------------------------------------

test_that("metrics_legal: osi_approved TRUE for MIT", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$osi_approved)
})

test_that("metrics_legal: osi_approved TRUE for GPL-2 | GPL-3", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: GPL-2 | GPL-3\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$osi_approved)
})

test_that("metrics_legal: osi_approved FALSE for CC0 (SPDX valid but not OSI approved)", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: CC0\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)   # CC0 is in SPDX allowlist
  expect_false(m$osi_approved) # but is not OSI-approved
})

test_that("metrics_legal: osi_approved FALSE for Unlimited", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: Unlimited\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)
  expect_false(m$osi_approved)
})

test_that("metrics_legal: osi_approved FALSE when one alternative is file LICENSE", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: GPL-2 | file LICENSE\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(m$spdx_valid)
  expect_false(m$osi_approved)
})

test_that("metrics_legal: osi_approved NA when License is absent", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(is.na(m$osi_approved))
})

# ---------------------------------------------------------------------------
# license_file_completeness
# ---------------------------------------------------------------------------

test_that("metrics_legal: license_file_completeness NA when license has no file reference", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: GPL-2\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(is.na(m$license_file_completeness))
})

test_that("metrics_legal: license_file_completeness NA when license is absent", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(is.na(m$license_file_completeness))
})

test_that("metrics_legal: license_file_completeness FALSE when LICENSE file is missing", {
  # MIT + file LICENSE declared but no LICENSE file in the tree
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n")
  m   <- metrics_legal(make_ctx(map))
  expect_false(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness FALSE when LICENSE file is empty", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n",
    "LICENSE"      = ""
  )
  m <- metrics_legal(make_ctx(map))
  expect_false(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness FALSE when MIT template has unfilled YEAR", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n",
    "LICENSE"      = "YEAR COPYRIGHT HOLDER\nMIT License\nPermission is hereby granted...\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_false(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness FALSE when MIT template has YEAR placeholder only", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n",
    "LICENSE"      = "YEAR Jane Doe\nMIT License\nPermission is hereby granted...\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_false(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness FALSE when MIT template has COPYRIGHT HOLDER only", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n",
    "LICENSE"      = "2024 COPYRIGHT HOLDER\nMIT License\nPermission is hereby granted...\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_false(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness TRUE when MIT template is properly filled", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n",
    "LICENSE"      = "2024 Jane Doe\nMIT License\nPermission is hereby granted, free of charge...\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_true(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness TRUE for non-template license with file", {
  # LGPL is not a template; any non-empty LICENSE file is acceptable
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: LGPL-2.1 + file LICENSE\n",
    "LICENSE"      = "This library is licensed under the GNU LGPL v2.1.\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_true(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness TRUE with LICENCE (British spelling)", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT + file LICENCE\n",
    "LICENCE"      = "2024 Jane Doe\nMIT License text here.\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_true(m$license_file_completeness)
})

test_that("metrics_legal: license_file_completeness TRUE for BSD_2_clause properly filled", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: BSD_2_clause + file LICENSE\n",
    "LICENSE"      = "Copyright (c) 2024, Jane Doe. All rights reserved.\nRedistribution and use...\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_true(m$license_file_completeness)
})

# ---------------------------------------------------------------------------
# copyright_holder_declared
# ---------------------------------------------------------------------------

test_that("metrics_legal: copyright_holder_declared TRUE when Authors@R has cph role", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: p\nVersion: 1.0\nLicense: MIT\n",
      'Authors@R: person("Jane", "Doe", role = c("aut", "cph"))\n'
    )
  )
  m <- metrics_legal(make_ctx(map))
  expect_true(m$copyright_holder_declared)
})

test_that("metrics_legal: copyright_holder_declared FALSE when Authors@R lacks cph", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: p\nVersion: 1.0\nLicense: MIT\n",
      'Authors@R: person("Jane", "Doe", role = c("aut", "cre"))\n'
    )
  )
  m <- metrics_legal(make_ctx(map))
  expect_false(m$copyright_holder_declared)
})

test_that("metrics_legal: copyright_holder_declared TRUE for single-quoted 'cph'", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: p\nVersion: 1.0\nLicense: MIT\n",
      "Authors@R: person('Jane', 'Doe', role = 'cph')\n"
    )
  )
  m <- metrics_legal(make_ctx(map))
  expect_true(m$copyright_holder_declared)
})

test_that("metrics_legal: copyright_holder_declared TRUE when Author field is non-empty (no Authors@R)", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT\nAuthor: Jane Doe\n"
  )
  m <- metrics_legal(make_ctx(map))
  expect_true(m$copyright_holder_declared)
})

test_that("metrics_legal: copyright_holder_declared NA when both Authors@R and Author are absent", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nLicense: MIT\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(is.na(m$copyright_holder_declared))
})

test_that("metrics_legal: copyright_holder_declared NA when DESCRIPTION is absent entirely", {
  map <- list("R/foo.R" = "foo <- function() NULL\n")
  m   <- metrics_legal(make_ctx(map))
  expect_true(is.na(m$copyright_holder_declared))
})

# ---------------------------------------------------------------------------
# Return structure
# ---------------------------------------------------------------------------

test_that("metrics_legal: always returns a named list with the five expected metrics", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  m   <- metrics_legal(make_ctx(map))
  expect_setequal(
    names(m),
    c("license", "spdx_valid", "osi_approved",
      "license_file_completeness", "copyright_holder_declared")
  )
})

test_that("metrics_legal: all metrics are scalar (length 1)", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: p\nVersion: 1.0\nLicense: MIT + file LICENSE\n",
      'Authors@R: person("Jane", "Doe", role = c("aut", "cph"))\n'
    ),
    "LICENSE" = "2024 Jane Doe\nMIT License text.\n"
  )
  m <- metrics_legal(make_ctx(map))
  for (nm in names(m)) {
    expect_true(length(m[[nm]]) == 1L, label = nm)
  }
})
