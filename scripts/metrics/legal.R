# scripts/metrics/legal.R: legal / licence metrics.
# Dependency: config.R, context.R must be sourced first.

# Static allowlist of recognized R-canonical / SPDX license identifiers.
# Matched against normalized tokens (trimmed; "+ file LICENSE" suffix stripped).
.LEGAL_SPDX_TOKENS <- c(
  # GPL family
  "GPL-2", "GPL-3",
  "GPL (>= 2)", "GPL (>= 3)",
  # LGPL family
  "LGPL-2", "LGPL-2.1", "LGPL-3",
  "LGPL (>= 2)", "LGPL (>= 2.1)",
  # MIT
  "MIT",
  # BSD
  "BSD_2_clause", "BSD_3_clause",
  # Apache
  "Apache License 2.0", "Apache License (>= 2)",
  # Creative Commons
  "CC0", "CC BY 4.0", "CC-BY-4.0",
  # Mozilla
  "MPL-2.0",
  # Artistic
  "Artistic-2.0",
  # AGPL
  "AGPL-3", "AGPL (>= 3)",
  # Other common R licenses
  "Unlimited",
  # Standalone file reference (full license text is in a file)
  "file LICENSE", "file LICENCE"
)

# OSI-approved subset of the allowlist above.
# CC0, CC-BY-4.0, Unlimited, and "file *" are excluded as they are not
# OSI-approved (or cannot be verified from the field alone).
.LEGAL_OSI_TOKENS <- c(
  "GPL-2", "GPL-3",
  "GPL (>= 2)", "GPL (>= 3)",
  "LGPL-2", "LGPL-2.1", "LGPL-3",
  "LGPL (>= 2)", "LGPL (>= 2.1)",
  "MIT",
  "BSD_2_clause", "BSD_3_clause",
  "Apache License 2.0", "Apache License (>= 2)",
  "MPL-2.0",
  "Artistic-2.0",
  "AGPL-3", "AGPL (>= 3)"
)

# Template licenses that use YEAR / COPYRIGHT HOLDER placeholder text
# and must be checked for completion when accompanied by a file reference.
.LEGAL_TEMPLATE_TOKENS <- c("MIT", "BSD_2_clause", "BSD_3_clause")

# Split a DESCRIPTION License string into canonical tokens.
#
# Rules:
#   "|" separates alternatives -> separate tokens
#   "+ file LICENSE[CE]" suffix is stripped from each alternative
#   A bare "file LICENSE[CE]" alternative (no preceding name) is kept as-is
#
# Returns character(0L) for NULL / blank / only-delimiter input.
.legal_tokenize <- function(lic) {
  if (is.null(lic) || !nzchar(trimws(lic))) return(character(0L))

  parts <- strsplit(lic, "|", fixed = TRUE)[[1L]]
  # Normalize internal whitespace then trim
  parts <- trimws(gsub("[ \t]+", " ", parts))
  # Drop completely empty parts (e.g. leading/trailing "|")
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(character(0L))

  # Strip "+ file LICENSE[CE]" suffix from each alternative
  stripped <- sub(
    "\\s*\\+\\s*file\\s+LICEN[SC]E\\s*$", "",
    parts, perl = TRUE
  )
  stripped <- trimws(stripped)
  # A part that became empty after stripping was a standalone file reference
  # (e.g. the whole token was "+ file LICENSE" without a preceding name)
  stripped[!nzchar(stripped)] <- "file LICENSE"
  stripped
}

# TRUE when the License string contains any reference to a LICENSE/LICENCE file.
.legal_has_file_ref <- function(lic) {
  grepl("\\bfile\\s+LICEN[SC]E\\b", lic %||% "", perl = TRUE)
}

# Check whether a license file is present and (for template licenses) complete.
#
# Returns:
#   NA    - license does not reference a file
#   FALSE - file is missing, empty, or contains unfilled template placeholders
#   TRUE  - file exists, non-empty, and placeholders are filled (or not a template)
.legal_file_completeness <- function(ctx, lic, tokens) {
  if (!.legal_has_file_ref(lic)) return(NA)

  lic_path <- if (ctx$exists("LICENSE")) "LICENSE"
              else if (ctx$exists("LICENCE")) "LICENCE"
              else NULL
  if (is.null(lic_path)) return(FALSE)

  content <- ctx$read(lic_path)
  if (!nzchar(trimws(content))) return(FALSE)

  # For MIT / BSD templates, verify YEAR and COPYRIGHT HOLDER placeholders
  # have been replaced by real values.
  if (any(tokens %in% .LEGAL_TEMPLATE_TOKENS)) {
    if (grepl("\\bYEAR\\b",             content, perl = TRUE) ||
        grepl("\\bCOPYRIGHT HOLDER\\b", content, perl = TRUE)) {
      return(FALSE)
    }
  }

  TRUE
}

# Determine whether a copyright holder is declared.
#
# Priority:
#   1. Authors@R field present: TRUE iff it contains a quoted "cph" role string.
#   2. Authors@R absent/empty: TRUE iff the Author field is non-empty.
#   3. Both fields absent: NA.
.legal_copyright_holder <- function(ctx) {
  authors_r <- ctx$desc[["Authors@R"]]
  if (!is.null(authors_r) && nzchar(trimws(authors_r))) {
    return(grepl('"cph"|\'cph\'', authors_r, perl = TRUE))
  }

  author <- ctx$desc[["Author"]]
  if (!is.null(author) && nzchar(trimws(author))) return(TRUE)

  NA
}

#' Compute legal / licence metrics for a package version.
#'
#' All metrics are NA-safe: absent/empty/malformed input yields NA rather than
#' an error or warning.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars (character / logical / NA):
#'   license                   character  raw DESCRIPTION License string
#'   spdx_valid                logical    every token is in the R-canonical/SPDX allowlist
#'   osi_approved              logical    every token is OSI-approved
#'   license_file_completeness logical    file reference resolved + template placeholders filled
#'   copyright_holder_declared logical    a "cph" role or Author field is present
metrics_legal <- function(ctx) {
  lic_raw <- ctx$desc$License
  license <- if (is.null(lic_raw) || !nzchar(trimws(lic_raw %||% ""))) {
    NA_character_
  } else {
    trimws(lic_raw)
  }

  tokens <- if (is.na(license)) character(0L) else .legal_tokenize(license)

  spdx_valid   <- if (length(tokens) == 0L) NA else all(tokens %in% .LEGAL_SPDX_TOKENS)
  osi_approved <- if (length(tokens) == 0L) NA else all(tokens %in% .LEGAL_OSI_TOKENS)

  license_file_completeness <- if (is.na(license)) {
    NA
  } else {
    .legal_file_completeness(ctx, license, tokens)
  }

  list(
    license                   = license,
    spdx_valid                = spdx_valid,
    osi_approved              = osi_approved,
    license_file_completeness = license_file_completeness,
    copyright_holder_declared = .legal_copyright_holder(ctx)
  )
}
