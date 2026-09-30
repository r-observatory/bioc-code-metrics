# tests/testthat/helper-handler-inventory.R: every error handler in a file,
# read with getParseData, so a new one cannot slip into worker code unseen.

.EXPR_TOKENS <- c("expr", "equal_assign", "expr_or_assign_or_help")

# One row per tryCatch or try call in `path`: its line, the call, the innermost
# named function around it, the text of the expression it guards, and whether
# it catches errors (a try, or a tryCatch with an error or condition handler).
# With `within`, only calls inside the function assigned to that name count.
# Attribute "n_retry" counts the .retry_after_time_limit calls in the same span.
.handler_inventory <- function(path, label = basename(path), within = NULL) {
  pd <- utils::getParseData(parse(path, keep.source = TRUE), includeText = TRUE)
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  kids      <- function(id) pd[pd$parent == id, , drop = FALSE]
  parent_of <- function(id) pd$parent[pd$id == id][1L]
  # The expr a name is bound to by `name <- function(...)`, or NULL.
  assigned_name <- function(fn_id) {
    pk <- kids(parent_of(fn_id))
    if (!any(pk$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))) return(NULL)
    lhs <- pk[pk$token %in% .EXPR_TOKENS, , drop = FALSE]
    if (!nrow(lhs) || identical(lhs$id[[1L]], fn_id)) return(NULL)
    sym <- kids(lhs$id[[1L]])
    if (nrow(sym) == 1L && sym$token == "SYMBOL") sym$text else NULL
  }
  enclosing_fn <- function(id) {
    while (!is.na(id) && id > 0L) {
      if ("FUNCTION" %in% kids(id)$token) {
        nm <- assigned_name(id)
        if (!is.null(nm)) return(nm)
      }
      id <- parent_of(id)
    }
    ""
  }
  span <- c(-Inf, Inf)
  if (!is.null(within)) {
    for (sid in pd$id[pd$token == "SYMBOL" & pd$text == within]) {
      assign_id <- parent_of(parent_of(sid))
      rhs <- kids(assign_id)
      rhs <- rhs[rhs$token %in% .EXPR_TOKENS, , drop = FALSE]
      if (nrow(rhs) == 2L && "FUNCTION" %in% kids(rhs$id[[2L]])$token) {
        span <- c(rhs$line1[[2L]], rhs$line2[[2L]])
      }
    }
    if (!is.finite(span[[1L]])) stop(sprintf("no function %s in %s", within, path))
  }
  in_span <- function(rows) rows$line1 >= span[[1L]] & rows$line2 <= span[[2L]]
  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("tryCatch", "try"), ,
              drop = FALSE]
  calls <- calls[in_span(calls), , drop = FALSE]
  rows <- lapply(seq_len(nrow(calls)), function(i) {
    call_id <- parent_of(parent_of(calls$id[[i]]))
    k   <- kids(call_id)
    tok <- k$token
    first <- NA_character_
    for (j in seq_len(nrow(k))[-1L]) {
      if (tok[[j]] %in% .EXPR_TOKENS && tok[[j - 1L]] != "EQ_SUB") {
        first <- utils::getParseText(pd, k$id[[j]])
        break
      }
    }
    data.frame(
      file = label, line = calls$line1[[i]], call = calls$text[[i]],
      fn = enclosing_fn(call_id), expr = first,
      flagged = calls$text[[i]] == "try" ||
        any(k$text[tok == "SYMBOL_SUB"] %in% c("error", "condition")),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    file = character(0L), line = integer(0L), call = character(0L),
    fn = character(0L), expr = character(0L), flagged = logical(0L),
    stringsAsFactors = FALSE)), rows))
  retry <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
                pd$text == ".retry_after_time_limit", , drop = FALSE]
  attr(out, "n_retry") <- sum(in_span(retry))
  out
}

# Every handler in the code a worker runs: the files it sources for analysis
# and the body of .pkg_worker in update.R.
.worker_handler_inventory <- function(scripts = test_path("..", "..", "scripts")) {
  files <- c("git.R", "context.R", "binary.R", "release_text.R", "analyze.R",
             file.path("metrics", sort(list.files(file.path(scripts, "metrics"),
                                                  pattern = "[.]R$"))))
  parts <- lapply(files, function(f) .handler_inventory(file.path(scripts, f), f))
  parts <- c(parts, list(.handler_inventory(file.path(scripts, "update.R"), "update.R",
                                            within = ".pkg_worker")))
  out <- do.call(rbind, parts)
  attr(out, "n_retry") <- sum(vapply(parts, function(p) attr(p, "n_retry"), integer(1L)))
  out
}

# Which inventory rows an allowlist row matches: same file, function and call,
# and the same guarded expression when the allowlist names one.
.allow_matches <- function(inv, allow) {
  inv$file == allow$file & inv$fn == allow$fn & inv$call == allow$call &
    (is.na(allow$expr) | (!is.na(inv$expr) & inv$expr == allow$expr))
}

# Flagged handlers no allowlist row accounts for.
.unlisted_handlers <- function(inv, allowlist) {
  listed <- rep(FALSE, nrow(inv))
  for (i in seq_len(nrow(allowlist))) {
    listed <- listed | .allow_matches(inv, allowlist[i, , drop = FALSE])
  }
  inv[inv$flagged & !listed, , drop = FALSE]
}
