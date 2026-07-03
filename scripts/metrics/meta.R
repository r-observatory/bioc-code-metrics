# scripts/metrics/meta.R: package metadata metrics.
# Dependency: config.R, context.R must be sourced first.

# ---- internal helpers --------------------------------------------------

# Extract the inner content of each person() call in an Authors@R string.
# Uses balanced-paren tracking so nested c() calls are handled correctly.
# Returns a character vector of inner-paren contents (one per person() call).
.meta_person_inners <- function(text) {
  if (!nzchar(trimws(text %||% ""))) return(character(0L))
  result    <- character(0L)
  remaining <- text
  repeat {
    m <- regexpr("\\bperson\\s*\\(", remaining, perl = TRUE)
    if (m == -1L) break
    open_pos <- m + attr(m, "match.length") - 1L  # index of the "("
    start    <- open_pos + 1L                       # first char inside "("
    depth    <- 1L
    pos      <- start
    nch      <- nchar(remaining)
    in_dq    <- FALSE  # inside a double-quoted string
    in_sq    <- FALSE  # inside a single-quoted string
    while (pos <= nch && depth > 0L) {
      ch <- substr(remaining, pos, pos)
      if      (!in_sq && ch == '"')  in_dq <- !in_dq
      else if (!in_dq && ch == "'") in_sq <- !in_sq
      else if (!in_dq && !in_sq) {
        if      (ch == "(") depth <- depth + 1L
        else if (ch == ")") depth <- depth - 1L
      }
      pos <- pos + 1L
    }
    # inner = everything from start to just before the matching closing ")"
    inner  <- substr(remaining, start, pos - 2L)
    result <- c(result, inner)
    remaining <- substr(remaining, pos, nch)
  }
  result
}

# Extract the value of the first capture group from the first match of `pattern`.
# Returns NA_character_ when there is no match or the group did not participate.
.cap1 <- function(pattern, text) {
  m <- regexpr(pattern, text, perl = TRUE)
  if (m == -1L) return(NA_character_)
  s <- attr(m, "capture.start")
  l <- attr(m, "capture.length")
  if (is.null(s) || length(s) == 0L) return(NA_character_)
  s1 <- s[[1L]]; l1 <- l[[1L]]
  if (is.na(s1) || s1 < 1L) return(NA_character_)
  substring(text, s1, s1 + l1 - 1L)
}

# Extract all quoted strings (single or double) from `text`.
# Uses alternation so "foo'bar" and 'foo"bar' are handled correctly.
# Returns a character vector of the string contents (outer quotes stripped).
.extract_quoted <- function(text) {
  ms <- regmatches(text, gregexpr('"[^"]*"|\'[^\']*\'', text, perl = TRUE))[[1L]]
  if (length(ms) == 0L) return(character(0L))
  substr(ms, 2L, nchar(ms) - 1L)
}

# Parse the inner content of a single person() call.
# Tries named arguments first; falls back to positional strings.
# Returns list(given, family, roles) where roles is always an I()-wrapped array.
.meta_parse_person <- function(inner) {
  given  <- NA_character_
  family <- NA_character_
  roles  <- character(0L)

  # Named given / first (try double-quote then single-quote form)
  gd <- .cap1('(?:given|first)\\s*=\\s*"([^"]*)"', inner)
  gs <- .cap1("(?:given|first)\\s*=\\s*'([^']*)'", inner)
  g  <- if (!is.na(gd)) gd else if (!is.na(gs)) gs else NA_character_
  if (!is.na(g)) given <- g

  # Named family / last
  fd <- .cap1('(?:family|last)\\s*=\\s*"([^"]*)"', inner)
  fs <- .cap1("(?:family|last)\\s*=\\s*'([^']*)'", inner)
  f  <- if (!is.na(fd)) fd else if (!is.na(fs)) fs else NA_character_
  if (!is.na(f)) family <- f

  # Roles: role = c(...) or role = "..." or role = '...'
  role_c_m <- regexpr('role\\s*=\\s*c\\(([^)]*)\\)', inner, perl = TRUE)
  if (role_c_m != -1L) {
    s <- attr(role_c_m, "capture.start")[[1L]]
    l <- attr(role_c_m, "capture.length")[[1L]]
    if (!is.na(s) && s >= 1L) {
      role_content <- substring(inner, s, s + l - 1L)
      roles <- .extract_quoted(role_content)
    }
  } else {
    rd <- .cap1('role\\s*=\\s*"([^"]*)"', inner)
    rs <- .cap1("role\\s*=\\s*'([^']*)'", inner)
    r  <- if (!is.na(rd)) rd else if (!is.na(rs)) rs else NA_character_
    if (!is.na(r)) roles <- r
  }

  # Positional args: strip named args then extract remaining quoted strings
  if (is.na(given) || is.na(family)) {
    cleaned <- inner
    # Remove: name = c(...)
    cleaned <- gsub('[A-Za-z_.][A-Za-z0-9_.]*\\s*=\\s*c\\([^)]*\\)', "",
                    cleaned, perl = TRUE)
    # Remove: name = "..." or name = '...'
    cleaned <- gsub('[A-Za-z_.][A-Za-z0-9_.]*\\s*=\\s*(?:"[^"]*"|\'[^\']*\')', "",
                    cleaned, perl = TRUE)
    pos_strs <- .extract_quoted(cleaned)
    pos_strs <- pos_strs[nzchar(pos_strs)]
    if (is.na(given)  && length(pos_strs) >= 1L) given  <- pos_strs[[1L]]
    if (is.na(family) && length(pos_strs) >= 2L) family <- pos_strs[[2L]]
  }

  list(given  = given  %||% NA_character_,
       family = family %||% NA_character_,
       roles  = I(roles))
}

# Parse a DCF Imports/Depends field text into a character vector of package names.
# Strips version constraints and excludes "R" itself.
.meta_parse_deps <- function(text) {
  if (!nzchar(trimws(text %||% ""))) return(character(0L))
  parts <- strsplit(text, ",", fixed = TRUE)[[1L]]
  pkgs  <- sub("\\s*\\(.*", "", trimws(parts), perl = TRUE)
  pkgs  <- trimws(pkgs)
  pkgs  <- pkgs[nzchar(pkgs)]
  pkgs[!grepl("^R$", pkgs, perl = TRUE)]
}

# Parse the Author free-text field into a list of {given, family, roles}.
# Splits on commas and "and"; extracts [role] brackets best-effort.
# Commas inside [...] brackets are protected before splitting so "Jane [aut, cre]"
# is treated as a single entry.
.meta_parse_author_text <- function(text) {
  if (!nzchar(trimws(text %||% ""))) return(list())
  text <- trimws(text)

  # Protect commas inside [...] from the comma-split by replacing them with \x01
  bm <- gregexpr("\\[[^\\]]+\\]", text, perl = TRUE)
  bracket_strs <- regmatches(text, bm)[[1L]]
  if (length(bracket_strs) > 0L) {
    protected <- gsub(",", "\x01", bracket_strs, fixed = TRUE)
    regmatches(text, bm) <- list(protected)
  }

  parts <- strsplit(text, "\\s*,\\s*|\\s+and\\s+", perl = TRUE)[[1L]]
  # Restore placeholders back to commas in each part (for role parsing below)
  parts <- gsub("\x01", ",", trimws(parts), fixed = TRUE)
  parts <- parts[nzchar(parts)]
  lapply(parts, function(entry) {
    # Strip email address
    entry <- sub("\\s*<[^>]*>", "", entry, perl = TRUE)
    # Extract [roles] bracket
    roles <- character(0L)
    rm    <- regexpr("\\[([^\\]]+)\\]", entry, perl = TRUE)
    if (rm != -1L) {
      rs    <- attr(rm, "capture.start")[[1L]]
      rl    <- attr(rm, "capture.length")[[1L]]
      if (!is.na(rs) && rs >= 1L) {
        role_str <- substring(entry, rs, rs + rl - 1L)
        roles    <- trimws(strsplit(role_str, ",", fixed = TRUE)[[1L]])
        roles    <- roles[nzchar(roles)]
      }
      entry <- trimws(sub("\\s*\\[[^\\]]*\\]", "", entry, perl = TRUE))
    }
    name_parts <- strsplit(trimws(entry), "\\s+", perl = TRUE)[[1L]]
    name_parts <- name_parts[nzchar(name_parts)]
    n <- length(name_parts)
    if (n == 0L) {
      list(given = NA_character_, family = NA_character_, roles = I(roles))
    } else if (n == 1L) {
      list(given = NA_character_, family = name_parts[[1L]], roles = I(roles))
    } else {
      list(given  = paste(name_parts[-n], collapse = " "),
           family = name_parts[[n]],
           roles  = I(roles))
    }
  })
}

# ---- main metric function ----------------------------------------------

#' Compute metadata metrics for a package version.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars:
#'   n_deps_direct    integer   Imports + Depends package count, excluding R itself
#'   dep_list         character JSON array of direct dependency package names
#'   maintainer       character Maintainer name (before the email bracket), NA if absent
#'   maintainer_email character Email extracted from Maintainer <...>, NA if absent
#'   n_authors        integer   person() count from Authors@R; falls back to
#'                              comma/and-separated name count from Author field
#'   authors          character JSON array of {given, family, roles} objects
metrics_meta <- function(ctx) {
  desc         <- ctx$desc
  desc_present <- ctx$exists("DESCRIPTION")

  # ---- direct dependencies ---------------------------------------------
  if (!desc_present) {
    n_deps_direct <- NA_integer_
    dep_list      <- NA_character_
  } else {
    imports_text <- desc[["Imports"]] %||% ""
    depends_text <- desc[["Depends"]] %||% ""
    combined     <- paste(
      Filter(nzchar, trimws(c(imports_text, depends_text))),
      collapse = ","
    )
    dep_pkgs      <- .meta_parse_deps(combined)
    n_deps_direct <- length(dep_pkgs)
    dep_list      <- as.character(jsonlite::toJSON(dep_pkgs, auto_unbox = FALSE))
  }

  # ---- maintainer ------------------------------------------------------
  if (!desc_present) {
    maintainer       <- NA_character_
    maintainer_email <- NA_character_
  } else {
    maint_raw <- trimws(desc[["Maintainer"]] %||% "")
    if (!nzchar(maint_raw)) {
      maintainer       <- NA_character_
      maintainer_email <- NA_character_
    } else {
      em <- regexpr("<([^>]+)>", maint_raw, perl = TRUE)
      if (em != -1L) {
        es <- attr(em, "capture.start")[[1L]]
        el <- attr(em, "capture.length")[[1L]]
        maintainer_email <- substring(maint_raw, es, es + el - 1L)
        name_part        <- trimws(sub("\\s*<[^>]*>.*", "", maint_raw, perl = TRUE))
        maintainer       <- if (nzchar(name_part)) name_part else NA_character_
      } else {
        maintainer       <- maint_raw
        maintainer_email <- NA_character_
      }
    }
  }

  # ---- authors ---------------------------------------------------------
  if (!desc_present) {
    n_authors <- NA_integer_
    authors   <- NA_character_
  } else {
    ar_text <- trimws(desc[["Authors@R"]] %||% "")
    if (nzchar(ar_text)) {
      inners <- .meta_person_inners(ar_text)
      if (length(inners) == 0L) {
        n_authors <- NA_integer_
        authors   <- NA_character_
      } else {
        n_authors <- length(inners)
        parsed    <- lapply(inners, .meta_parse_person)
        authors   <- as.character(jsonlite::toJSON(parsed, auto_unbox = TRUE))
      }
    } else {
      au_text <- trimws(desc[["Author"]] %||% "")
      if (!nzchar(au_text)) {
        n_authors <- NA_integer_
        authors   <- NA_character_
      } else {
        parsed    <- .meta_parse_author_text(au_text)
        n_authors <- length(parsed)
        if (n_authors == 0L) {
          n_authors <- NA_integer_
          authors   <- NA_character_
        } else {
          authors <- as.character(jsonlite::toJSON(parsed, auto_unbox = TRUE))
        }
      }
    }
  }

  list(
    n_deps_direct    = n_deps_direct,
    dep_list         = dep_list,
    maintainer       = maintainer,
    maintainer_email = maintainer_email,
    n_authors        = n_authors,
    authors          = authors
  )
}
