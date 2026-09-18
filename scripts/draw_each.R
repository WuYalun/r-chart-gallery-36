#!/usr/bin/env Rscript
# Run every per-chart script in scripts/charts/.
args <- commandArgs(trailingOnly = FALSE)
this <- sub("^--file=", "", args[grep("^--file=", args)])
scripts_dir <- if (length(this)) dirname(normalizePath(this[1])) else getwd()
chart_dir <- file.path(scripts_dir, "charts")
files <- sort(list.files(chart_dir, pattern = "^[0-9].*\\.R$", full.names = TRUE))
if (!length(files)) stop("no chart scripts in ", chart_dir)
for (f in files) {
  message("==== ", basename(f), " ====")
  status <- system2("Rscript", f)
  if (status != 0) stop("failed: ", f)
}
message("all ", length(files), " charts done")
