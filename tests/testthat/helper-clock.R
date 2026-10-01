# tests/testthat/helper-clock.R: a clock for the scripts that never gives two
# reads the same second.

# Stand in for Sys.time() as the scripts see it, until `frame` exits. It starts
# at the real time and moves one second forward at each read, which is what a
# second boundary between two reads does.
.local_stepping_clock <- function(frame = parent.frame()) {
  at <- base::Sys.time()
  .local_global("Sys.time", function() {
    at <<- at + 1
    at
  }, frame = frame)
}
