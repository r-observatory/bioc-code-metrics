# scripts/metrics/docs.R: documentation quality metrics.
# Dependency: config.R, context.R must be sourced first.

#' Compute documentation quality metrics for a package version.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars (numeric / logical / integer / NA).
metrics_docs <- function(ctx) {

  # ---- Internal helpers -------------------------------------------------------

  # Brace-balanced content starting right after a known opening '{'.
  # Handles Rd escaped braces (\{ and \}).
  # Returns list(content=chr, end=int) where end = position of closing '}'
  # in `text`, or NULL when braces are unbalanced.
  .bc <- function(text, after_open) {
    if (after_open > nchar(text)) return(NULL)
    rest  <- substring(text, after_open)
    chars <- strsplit(rest, "")[[1L]]
    if (length(chars) == 0L) return(NULL)
    depth <- 1L
    i     <- 1L
    while (i <= length(chars)) {
      ch <- chars[[i]]
      # Skip Rd escape sequences \{ and \}
      if (ch == "\\" && i < length(chars) &&
          chars[[i + 1L]] %in% c("{", "}")) {
        i <- i + 2L
        next
      }
      if      (ch == "{") depth <- depth + 1L
      else if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L)
          return(list(
            content = paste(chars[seq_len(i - 1L)], collapse = ""),
            end     = after_open + i - 1L
          ))
      }
      i <- i + 1L
    }
    NULL
  }

  # Find the first \cmd{...} block in text.
  # Returns list(content, end) or NULL.
  .fb <- function(text, cmd) {
    m <- regexpr(paste0("\\\\", cmd, "\\s*\\{"), text, perl = TRUE)
    if (m == -1L) return(NULL)
    .bc(text, m + attr(m, "match.length"))
  }

  # Find all \cmd{...} block contents in text.
  # Returns a character vector of contents (only successful parses).
  .ab <- function(text, cmd) {
    ms   <- gregexpr(paste0("\\\\", cmd, "\\s*\\{"), text, perl = TRUE)[[1L]]
    lens <- attr(ms, "match.length")
    if (ms[1L] == -1L) return(character(0L))
    out <- character(0L)
    for (i in seq_along(ms)) {
      b <- .bc(text, ms[i] + lens[i])
      if (!is.null(b)) out <- c(out, b$content)
    }
    out
  }

  # Check whether text contains at least one \cmd{ marker.
  .hc <- function(text, cmd)
    grepl(paste0("\\\\", cmd, "\\s*\\{"), text, perl = TRUE)

  # Extract parameter names from \usage block content (approximate).
  # Finds identifier(...) call signatures and splits at top-level commas to
  # avoid counting named arguments inside default values as parameters.
  .uparams <- function(u) {
    if (!nzchar(trimws(u))) return(character(0L))
    u <- gsub("%[^\n]*",          "",    u, perl = TRUE) # strip Rd comments
    u <- gsub("\\\\dots|\\\\ldots", "...", u, perl = TRUE)
    ms   <- gregexpr("[A-Za-z_.][A-Za-z0-9_.]*\\s*\\(", u, perl = TRUE)[[1L]]
    lens <- attr(ms, "match.length")
    if (ms[1L] == -1L) return(character(0L))
    params <- character(0L)
    for (k in seq_along(ms)) {
      after <- ms[k] + lens[k]
      rest  <- substring(u, after)
      if (!nzchar(rest)) next
      chars <- strsplit(rest, "")[[1L]]
      depth <- 1L; end_i <- NA_integer_
      for (j in seq_along(chars)) {
        if      (chars[j] == "(") depth <- depth + 1L
        else if (chars[j] == ")") {
          depth <- depth - 1L
          if (depth == 0L) { end_i <- j; break }
        }
      }
      if (is.na(end_i)) next
      sig <- paste(chars[seq_len(end_i - 1L)], collapse = "")
      # Split by top-level commas only (respects nested parentheses)
      sc <- strsplit(sig, "")[[1L]]
      d2 <- 0L; ts <- 1L; toks <- character(0L)
      for (jj in seq_along(sc)) {
        if      (sc[jj] == "(") d2 <- d2 + 1L
        else if (sc[jj] == ")") d2 <- d2 - 1L
        else if (sc[jj] == "," && d2 == 0L) {
          toks <- c(toks, paste(sc[ts:(jj - 1L)], collapse = ""))
          ts   <- jj + 1L
        }
      }
      if (ts <= length(sc)) toks <- c(toks, paste(sc[ts:length(sc)], collapse = ""))
      for (tok in toks) {
        tok   <- trimws(tok)
        pname <- trimws(sub("\\s*=.*$", "", tok, perl = TRUE))
        if (!nzchar(pname) || pname == "..." ||
            !grepl("^[A-Za-z_.]", pname, perl = TRUE)) next
        params <- c(params, pname)
      }
    }
    unique(params)
  }

  # Extract parameter names documented via \item{name}{} in \arguments content.
  .inames <- function(args_text) {
    hits <- regmatches(
      args_text,
      gregexpr("\\\\item\\s*\\{([^{}]*)\\}", args_text, perl = TRUE)
    )[[1L]]
    if (length(hits) == 0L) return(character(0L))
    trimws(sub("^\\\\item\\s*\\{([^{}]*)\\}.*$", "\\1", hits, perl = TRUE))
  }

  # ---- Setup -----------------------------------------------------------------
  rd_files <- ctx$find("^man/.*\\.Rd$")
  n_rd     <- length(rd_files)

  raw_exports <- ctx$namespace$exports
  if (is.null(raw_exports)) raw_exports <- character(0L)
  exports <- raw_exports[!startsWith(raw_exports, "pattern:")]

  # Symbols declared per Rd file (\name + \alias)
  rd_syms <- setNames(
    lapply(rd_files, function(f) {
      text <- ctx$read(f)
      trimws(c(.ab(text, "name"), .ab(text, "alias")))
    }),
    rd_files
  )

  # Rd files that document at least one exported symbol
  ex_fn_rd <- rd_files[vapply(rd_files, function(f) {
    length(exports) > 0L && any(rd_syms[[f]] %in% exports)
  }, logical(1L))]

  # ---- 1. dontrun_example_ratio ----------------------------------------------
  # Fraction of man/*.Rd with \examples where the entire examples block is
  # wrapped in a single \dontrun{} or \donttest{} (no runnable code exposed).
  dontrun_example_ratio <- {
    rd_ex <- rd_files[vapply(rd_files, function(f) {
      .hc(ctx$read(f), "examples")
    }, logical(1L))]
    n_ex <- length(rd_ex)
    if (n_ex == 0L) {
      NA_real_
    } else {
      n_wrap <- sum(vapply(rd_ex, function(f) {
        text <- ctx$read(f)
        blk  <- .fb(text, "examples")
        if (is.null(blk)) return(FALSE)
        body <- trimws(blk$content)
        m2   <- regexpr("^\\\\don(trun|ttest)\\s*\\{", body, perl = TRUE)
        if (m2 == -1L) return(FALSE)
        inner <- .bc(body, m2 + attr(m2, "match.length"))
        if (is.null(inner)) return(FALSE)
        # Nothing (or only whitespace) should remain after the wrapper closes
        !nzchar(trimws(substring(body, inner$end + 1L)))
      }, logical(1L)))
      n_wrap / n_ex
    }
  }

  # ---- 2. undocumented_params_rate -------------------------------------------
  # Across exported-function Rd files: mean fraction of \usage parameters that
  # have no matching \item in \arguments.  NA when no exported-function Rd or
  # no exports.
  undocumented_params_rate <- {
    if (length(ex_fn_rd) == 0L || length(exports) == 0L) {
      NA_real_
    } else {
      rates <- vapply(ex_fn_rd, function(f) {
        text  <- ctx$read(f)
        ublk  <- .fb(text, "usage")
        if (is.null(ublk)) return(NA_real_)
        params <- .uparams(ublk$content)
        if (length(params) == 0L) return(NA_real_)
        ablk   <- .fb(text, "arguments")
        dnames <- if (!is.null(ablk)) .inames(ablk$content) else character(0L)
        sum(!params %in% dnames) / length(params)
      }, numeric(1L))
      valid <- rates[!is.na(rates)]
      if (length(valid) == 0L) NA_real_ else mean(valid)
    }
  }

  # ---- 3. value_doc_rate -----------------------------------------------------
  # Fraction of exported-function Rd files that contain a \value{} section.
  value_doc_rate <- {
    if (length(ex_fn_rd) == 0L) NA_real_
    else mean(vapply(ex_fn_rd, function(f) .hc(ctx$read(f), "value"), logical(1L)))
  }

  # ---- 4. references_coverage ------------------------------------------------
  # Fraction of all man/*.Rd files that contain a \references{} section.
  references_coverage <- {
    if (n_rd == 0L) NA_real_
    else mean(vapply(rd_files, function(f) .hc(ctx$read(f), "references"), logical(1L)))
  }

  # ---- 5. roxygen_doc_coverage -----------------------------------------------
  # Fraction of exported symbols (non-pattern) that appear as \name or \alias
  # in at least one man/*.Rd file.  NA when there are no exports.
  roxygen_doc_coverage <- {
    if (length(exports) == 0L) {
      NA_real_
    } else {
      all_syms <- unique(unlist(rd_syms, use.names = FALSE))
      sum(exports %in% all_syms) / length(exports)
    }
  }

  # ---- 6. has_readme ---------------------------------------------------------
  has_readme <- ctx$exists("README.md") || ctx$exists("README.Rmd")

  # ---- 7. readme_prose_length ------------------------------------------------
  # Word count of README after stripping fenced code blocks and badge/image lines.
  # NA when no README is present; 0 when README contains only stripped content.
  readme_prose_length <- {
    rpath <- if      (ctx$exists("README.md"))  "README.md"
              else if (ctx$exists("README.Rmd")) "README.Rmd"
              else NULL
    if (is.null(rpath)) {
      NA_integer_
    } else {
      text <- ctx$read(rpath)
      if (!nzchar(text)) {
        0L
      } else {
        # Strip fenced code blocks (``` ... ```)
        text <- gsub("(?s)```[^\n]*\n.*?```", "", text, perl = TRUE)
        # Strip badge / inline-image lines
        lns  <- strsplit(text, "\n", fixed = TRUE)[[1L]]
        lns  <- lns[!grepl("^\\s*(\\[!\\[|<img\\s|\\[\\[img)", lns, perl = TRUE)]
        text2 <- paste(lns, collapse = " ")
        words <- strsplit(trimws(text2), "\\s+")[[1L]]
        length(words[nzchar(words)])
      }
    }
  }

  # ---- 8. has_pkgdown --------------------------------------------------------
  has_pkgdown <- ctx$exists("_pkgdown.yml") || ctx$exists("pkgdown/_pkgdown.yml")

  # ---- 9. news_present -------------------------------------------------------
  news_present <- ctx$exists("NEWS") || ctx$exists("NEWS.md")

  # ---- 10. news_structure_quality --------------------------------------------
  # 0..1 score based on three criteria (each worth 1/3):
  #   (a) has per-version headings containing a version number,
  #   (b) has bullet-point entries,
  #   (c) version headings appear in descending version order (requires >= 2).
  news_structure_quality <- {
    npath <- if      (ctx$exists("NEWS.md")) "NEWS.md"
              else if (ctx$exists("NEWS"))    "NEWS"
              else NULL
    if (is.null(npath)) {
      NA_real_
    } else {
      text <- ctx$read(npath)
      if (!nzchar(trimws(text))) {
        0.0
      } else {
        lns   <- strsplit(text, "\n", fixed = TRUE)[[1L]]
        n_met <- 0L

        # Criterion (a): per-version headings
        ver_hd_pat <- paste0(
          "^(#{1,4}\\s[^\n]*\\d+\\.\\d+",
          "|[Vv]ersion\\s+\\d+\\.\\d+",
          "|[Cc]hanges?\\s+(in|for)\\s+(version\\s+)?\\d+\\.\\d+",
          "|\\d+\\.\\d+(?:\\.\\d+)?\\s*([-_(]|$))"
        )
        hd_lines <- grep(ver_hd_pat, lns, value = TRUE, perl = TRUE)
        if (length(hd_lines) > 0L) n_met <- n_met + 1L

        # Criterion (b): bullet entries
        if (any(grepl("^\\s*[-*+]\\s+\\S", lns, perl = TRUE))) n_met <- n_met + 1L

        # Criterion (c): version headings in descending order (need >= 2 headings)
        if (length(hd_lines) >= 2L) {
          ver_strs <- unlist(regmatches(
            hd_lines,
            gregexpr("\\d+\\.\\d+(?:\\.\\d+)*", hd_lines, perl = TRUE)
          ))
          if (length(ver_strs) >= 2L) {
            vers <- tryCatch(numeric_version(ver_strs), error = function(e) NULL)
            if (!is.null(vers)) {
              diffs <- vapply(seq_len(length(vers) - 1L),
                              function(i) vers[i] >= vers[i + 1L],
                              logical(1L))
              if (all(diffs)) n_met <- n_met + 1L
            }
          }
        }

        n_met / 3L
      }
    }
  }

  # ---- Return ----------------------------------------------------------------
  list(
    dontrun_example_ratio    = dontrun_example_ratio,
    undocumented_params_rate = undocumented_params_rate,
    value_doc_rate           = value_doc_rate,
    references_coverage      = references_coverage,
    roxygen_doc_coverage     = roxygen_doc_coverage,
    has_readme               = has_readme,
    readme_prose_length      = as.integer(readme_prose_length),
    has_pkgdown              = has_pkgdown,
    news_present             = news_present,
    news_structure_quality   = news_structure_quality
  )
}
