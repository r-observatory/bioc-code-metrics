library(testthat)

# Source all scripts in dependency order.
source("scripts/config.R")
source("scripts/git.R")
source("scripts/context.R")
source("scripts/binary.R")
for (f in sort(list.files("scripts/metrics", pattern = "\\.R$", full.names = TRUE))) source(f)
source("scripts/analyze.R")
source("scripts/export.R")
source("scripts/prune.R")
source("scripts/render_notes.R")
source("scripts/update.R")

test_dir("tests/testthat", stop_on_failure = TRUE)
