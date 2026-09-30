# tests/testthat/helper-time-limit.R: a cap that fires once, for the tests of
# the handlers that retry it.

# The error R raises when the elapsed limit fires.
.cap_error <- function() simpleError(.time_limit_msg())

# f, except that the first call for which when(...) is TRUE raises the cap
# error instead. With when = NULL the very first call raises it.
.fires_cap_once <- function(f, when = NULL) {
  force(f)
  force(when)
  fired <- FALSE
  function(...) {
    if (!fired && (is.null(when) || isTRUE(when(...)))) {
      fired <<- TRUE
      stop(.cap_error())
    }
    f(...)
  }
}

# Bind name to value in the global environment, where the scripts are sourced,
# until the calling test ends.
.local_global <- function(name, value, frame = parent.frame()) {
  had <- exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (had) get(name, envir = globalenv(), inherits = FALSE)
  assign(name, value, envir = globalenv())
  withr::defer({
    if (had) assign(name, old, envir = globalenv())
    else rm(list = name, envir = globalenv())
  }, envir = frame)
  invisible(old)
}

# Spend about `secs` of elapsed time in R code, where the limit is checked.
.busy <- function(secs) {
  t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < secs) sum(stats::runif(100))
  invisible(NULL)
}
