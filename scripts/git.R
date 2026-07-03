# scripts/git.R: git plumbing wrappers for the bioc-code-metrics pipeline.
#
# All functions communicate with git via system2("git", ...).
# clone_package() never throws on failure; it returns FALSE instead.
# Dependency: config.R must be sourced first (BIOC_GIT_BASE, %||%).

#' Clone a Bioconductor repo from github.com/bioc.
#'
#' A plain `git clone` fetches all remote branches as `origin/*` refs, so
#' every RELEASE_X_Y branch becomes accessible as `origin/RELEASE_X_Y`
#' immediately after the clone without any extra fetch.
#'
#' @param pkg   Package name (exact case as it appears in Bioconductor).
#' @param dest  Local path for the clone (passed as <directory> to git clone).
#' @param base  Base URL; defaults to BIOC_GIT_BASE.
#' @param token Optional GitHub personal access token. When supplied the clone
#'   URL is rewritten to inject the token so the request authenticates without
#'   a credential helper or .netrc.
#' @return TRUE on success, FALSE on any failure (404, network, etc.).
clone_package <- function(pkg, dest, base = BIOC_GIT_BASE, token = NULL) {
  if (!is.null(token) && nzchar(token)) {
    # Convert "https://github.com/bioc" -> "https://x-access-token:<tok>@github.com/bioc"
    url <- sub("^https://", paste0("https://x-access-token:", token, "@"), base)
    url <- paste0(url, "/", pkg, ".git")
  } else {
    url <- paste0(base, "/", pkg, ".git")
  }
  rc <- suppressWarnings(
    system2("git", c("clone", "--quiet", url, dest),
            stdout = FALSE, stderr = FALSE, timeout = GIT_TIMEOUT)
  )
  identical(rc, 0L)
}

#' List Bioconductor RELEASE_X_Y branches, ordered by release version ascending.
#'
#' Scans for branches matching the pattern RELEASE_<major>_<minor> in both
#' remote-tracking refs (`refs/remotes/origin/RELEASE_*`, present after
#' `git clone`) and local branch refs (`refs/heads/RELEASE_*`, used in
#' offline tests). Converts "RELEASE_3_18" to version "3.18". Sorts numerically
#' so that 3.9 < 3.10 (not lexicographically). De-duplicates by commit SHA,
#' keeping the lowest-version occurrence per SHA.
#'
#' Only SOFTWARE and WORKFLOW packages have github.com/bioc repos with RELEASE
#' branches. Annotation/experiment data packages do not; their repos either
#' do not exist or have no RELEASE branches. In both cases the empty frame is
#' returned.
#'
#' @param repo  Path to a local git repository directory.
#' @return data.frame(version, ref, date, commit) ordered by version ascending.
#'   version - "X.Y" string (e.g. "3.18")
#'   ref     - usable git ref (e.g. "origin/RELEASE_3_18" or "RELEASE_3_18")
#'   date    - branch-tip author date YYYY-MM-DD
#'   commit  - branch-tip SHA
#'   All columns are character. Returns a zero-row frame when no RELEASE
#'   branches are found.
list_versions <- function(repo) {
  empty <- data.frame(
    version = character(0L), ref    = character(0L),
    date    = character(0L), commit = character(0L),
    stringsAsFactors = FALSE
  )

  # For branch refs (commit objects) %(objectname) IS the commit SHA.
  # Use %09 (not %x09): Apple git 2.50+ does not expand the %xNN hex escape
  # form in for-each-ref --format, but does expand the decimal %09 form.
  # Check both remote-tracking refs (production: after git clone) and local
  # branch refs (offline tests).
  fmt <- paste0(
    "%(refname:short)%09",
    "%(objectname)%09",
    "%(authordate:short)"
  )
  # system2 with stdout=TRUE pipes through /bin/sh; shQuote the format argument.
  raw <- suppressWarnings(
    system2("git", c("-C", repo, "for-each-ref",
                     shQuote(paste0("--format=", fmt)),
                     "refs/remotes/origin/RELEASE_*",
                     "refs/heads/RELEASE_*"),
            stdout = TRUE, stderr = FALSE, timeout = GIT_TIMEOUT)
  )
  if (length(raw) == 0L || identical(raw, character(0L))) return(empty)
  if (!is.null(attr(raw, "status")) && attr(raw, "status") != 0L) return(empty)

  rows <- lapply(raw, function(line) {
    p <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    if (length(p) < 3L) return(NULL)
    short_ref <- p[1L]   # e.g. "origin/RELEASE_3_18" or "RELEASE_3_18"
    sha       <- p[2L]
    adate     <- p[3L]

    # Extract the bare branch name by stripping the "origin/" remote prefix.
    branch <- sub("^origin/", "", short_ref)
    # Require exactly RELEASE_<digits>_<digits> (e.g. RELEASE_3_18).
    if (!grepl("^RELEASE_[0-9]+_[0-9]+$", branch)) return(NULL)

    # Convert "RELEASE_3_18" -> "3.18"
    ver_raw <- sub("^RELEASE_", "", branch)
    ver     <- gsub("_", ".", ver_raw, fixed = TRUE)

    list(ref = short_ref, commit = sha, date = adate, version = ver)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(empty)

  versions <- vapply(rows, `[[`, character(1L), "version")
  refs     <- vapply(rows, `[[`, character(1L), "ref")
  commits  <- vapply(rows, `[[`, character(1L), "commit")
  dates    <- vapply(rows, `[[`, character(1L), "date")

  # Sort numerically by Bioconductor release version: X*1000 + Y.
  # This ensures 3.9 < 3.10 (not 3.10 < 3.9 as lexicographic sort would give).
  .ver_num <- function(v) {
    p <- suppressWarnings(as.integer(strsplit(v, ".", fixed = TRUE)[[1L]]))
    if (length(p) < 2L || any(is.na(p))) return(NA_real_)
    p[1L] * 1000L + p[2L]
  }
  nums <- vapply(versions, .ver_num, numeric(1L))
  ord  <- order(nums, na.last = TRUE)
  versions <- versions[ord]
  refs     <- refs[ord]
  commits  <- commits[ord]
  dates    <- dates[ord]

  # De-duplicate by commit SHA: keep first (lowest-version) occurrence.
  # Uses a hashed environment for O(1) membership tests.
  seen <- new.env(hash = TRUE, parent = emptyenv())
  keep <- logical(length(commits))
  for (i in seq_along(commits)) {
    if (!exists(commits[i], envir = seen, inherits = FALSE)) {
      keep[i] <- TRUE
      assign(commits[i], TRUE, envir = seen)
    }
  }

  data.frame(
    version = versions[keep],
    ref     = refs[keep],
    date    = dates[keep],
    commit  = commits[keep],
    stringsAsFactors = FALSE
  )
}

#' Extract a version tree from a git archive into a directory.
#'
#' Runs `git archive <ref> | tar -x -C <dest>` so <dest> holds that version's
#' file tree exactly.  Creates <dest> if it does not exist.
#'
#' @param repo  Path to a local git repository.
#' @param ref   Tag name, branch, or commit SHA to archive.
#'   For Bioconductor RELEASE branches, use the ref from list_versions()
#'   (e.g. "origin/RELEASE_3_18" or a commit SHA).
#' @param dest  Destination directory for the extracted tree.
#' @return Character vector of extracted file paths, relative to dest.
#'   Returns character(0) when the archive or extraction fails.
extract_version <- function(repo, ref, dest) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  # Write the archive to a temp file so each stage gets its own timeout.
  archive_file <- tempfile("git_archive_", fileext = ".tar")
  on.exit(unlink(archive_file), add = TRUE)
  rc1 <- suppressWarnings(
    system2("git", c("-C", repo, "archive", ref),
            stdout = archive_file, stderr = FALSE, timeout = GIT_TIMEOUT)
  )
  if (!identical(rc1, 0L)) return(character(0L))
  rc2 <- suppressWarnings(
    system2("tar", c("-x", "-C", dest),
            stdin = archive_file, stdout = FALSE, stderr = FALSE,
            timeout = GIT_TIMEOUT)
  )
  if (!identical(rc2, 0L)) return(character(0L))
  list.files(dest, recursive = TRUE, all.files = TRUE,
             include.dirs = FALSE, no.. = TRUE)
}

# Resolve a git numstat path that contains a rename arrow ( => ).
# git emits renames as either:
#   brace form:  "pre/{old => new}/post"
#   plain form:  "old/path => new/path"
# Returns the new (right-hand) path in both cases.
.resolve_rename_path <- function(path) {
  if (!grepl(" => ", path, fixed = TRUE)) return(path)
  # Brace form: pre/{a => b}/post
  m <- regmatches(path, regexec("^(.*?)\\{([^}]*) => ([^}]*)\\}(.*)$", path))[[1L]]
  if (length(m) == 5L) {
    pre    <- m[2L]
    b      <- m[4L]   # new name (right-hand side)
    post   <- m[5L]
    result <- paste0(pre, b, post)
    # Collapse double slashes that arise when b is empty
    result <- gsub("//+", "/", result, perl = TRUE)
    # Strip a leading or trailing slash left by empty b
    result <- sub("^/", "", result)
    result <- sub("/$", "", result)
    return(result)
  }
  # Plain form: "old => new" -- take the right-hand side
  trimws(sub("^.* => ", "", path))
}

#' Compute per-file churn between consecutive Bioconductor RELEASE branches.
#'
#' For each consecutive pair of RELEASE_X_Y releases (ordered by version
#' ascending), runs `git diff --numstat <prev_tip>..<curr_tip>` and attributes
#' the per-file added/deleted line counts to the CURR release's version string.
#' For the FIRST release, diffs against the empty tree (git's well-known
#' 4b825dc... SHA) so the first release's churn equals its full file content.
#'
#' Binary files for which numstat reports "-" are recorded with NA added/deleted.
#' Rename paths are resolved to the new (right-hand) path via .resolve_rename_path.
#'
#' @param repo  Path to a local git repository.
#' @return data.frame(commit, version, file, added, deleted).
#'   commit  - tip SHA of the release branch (character)
#'   version - Bioconductor release version string, e.g. "3.18" (character)
#'   file    - repository-relative file path (character)
#'   added   - integer lines added, NA for binary files
#'   deleted - integer lines deleted, NA for binary files
#'   Returns the zero-row frame when no RELEASE branches are found.
package_churn <- function(repo) {
  empty <- data.frame(
    commit  = character(0L), version = character(0L),
    file    = character(0L), added   = integer(0L),  deleted = integer(0L),
    stringsAsFactors = FALSE
  )

  vers <- list_versions(repo)
  if (nrow(vers) == 0L) return(empty)

  # Git's empty-tree SHA (the canonical SHA of an empty tree object).
  # Used to diff the first release against nothing so all its content counts
  # as added lines rather than being skipped.
  empty_tree <- suppressWarnings(
    system2("git", c("-C", repo, "hash-object", "-t", "tree", "/dev/null"),
            stdout = TRUE, stderr = FALSE, timeout = GIT_TIMEOUT)
  )
  if (length(empty_tree) == 0L || !nzchar(trimws(empty_tree[1L])) ||
      (!is.null(attr(empty_tree, "status")) && attr(empty_tree, "status") != 0L)) {
    # Fall back to the well-known SHA when /dev/null is not available or on timeout.
    empty_tree <- "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  } else {
    empty_tree <- trimws(empty_tree[1L])
  }

  results   <- vector("list", nrow(vers) * 20L)
  n_results <- 0L

  for (i in seq_len(nrow(vers))) {
    curr_ver    <- vers$version[i]
    curr_commit <- vers$commit[i]
    prev_ref    <- if (i == 1L) empty_tree else vers$commit[i - 1L]

    raw <- suppressWarnings(
      system2("git", c("-C", repo, "diff", "--numstat",
                       paste0(prev_ref, "..", curr_commit)),
              stdout = TRUE, stderr = FALSE, timeout = GIT_TIMEOUT)
    )
    # On timeout the status attribute is non-zero; skip this release's diff
    # rather than recording partial or absent churn data.
    if (!is.null(attr(raw, "status")) && attr(raw, "status") != 0L) next

    for (line in raw) {
      line <- trimws(line)
      if (!nzchar(line)) next
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      if (length(parts) < 3L) next
      add_s <- parts[1L]
      del_s <- parts[2L]
      fpath <- .resolve_rename_path(parts[3L])
      # Binary files: numstat shows "-" for both counts.
      added   <- if (add_s == "-") NA_integer_ else suppressWarnings(as.integer(add_s))
      deleted <- if (del_s == "-") NA_integer_ else suppressWarnings(as.integer(del_s))
      n_results <- n_results + 1L
      results[[n_results]] <- list(
        commit  = curr_commit,
        version = curr_ver,
        file    = fpath,
        added   = added,
        deleted = deleted
      )
    }
  }

  if (n_results == 0L) return(empty)
  results <- results[seq_len(n_results)]

  data.frame(
    commit  = vapply(results, `[[`, character(1L), "commit"),
    version = vapply(results, `[[`, character(1L), "version"),
    file    = vapply(results, `[[`, character(1L), "file"),
    added   = vapply(results, `[[`, integer(1L),   "added"),
    deleted = vapply(results, `[[`, integer(1L),   "deleted"),
    stringsAsFactors = FALSE
  )
}

#' Read the content of a file at a specific git ref.
#'
#' @param repo  Path to a local git repository.
#' @param ref   Tag, branch, or commit SHA.
#'   For Bioconductor RELEASE branches, pass a ref from list_versions()
#'   (e.g. "origin/RELEASE_3_18").
#' @param path  Repository-relative file path (e.g. "DESCRIPTION").
#' @return Content as a single character string.  Returns "" when the path does
#'   not exist at the given ref (git exits non-zero; no error is thrown).
read_at <- function(repo, ref, path) {
  spec <- paste0(ref, ":", path)
  out  <- suppressWarnings(
    system2("git", c("-C", repo, "show", spec),
            stdout = TRUE, stderr = FALSE, timeout = GIT_TIMEOUT)
  )
  if (length(out) == 0L || identical(out, character(0L))) return("")
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) return("")
  paste(out, collapse = "\n")
}
