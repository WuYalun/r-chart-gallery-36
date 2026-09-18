#!/usr/bin/env Rscript
# 06 雷达图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(fmsb))

radar <- as.data.frame(rbind(
  max = rep(10, 6),
  min = rep(0, 6),
  Ivy = c(8.5, 6.2, 9.0, 5.5, 7.8, 6.0),
  UI = c(6.0, 8.8, 5.4, 9.1, 6.5, 8.0),
  Lab = c(7.2, 5.0, 7.8, 6.4, 8.9, 5.6)
))
colnames(radar) <- c("Speed", "Quality", "Coverage", "UX", "Stability", "Cost")
save_base(file.path(FIG_DIR, "06_radar.png"), {
  par(mar = c(2, 2, 3, 2))
  radarchart(
    radar, axistype = 1, seg = 5, pcol = pal_cluster[1:3],
    pfcol = adjustcolor(pal_cluster[1:3], 0.35), plwd = 2, plty = 1,
    cglcol = "grey70", cglty = 1, axislabcol = "grey30",
    caxislabels = seq(0, 10, 2), vlcex = 0.9,
    title = "06 Radar chart  |  fmsb::radarchart"
  )
  legend("topright", legend = rownames(radar)[-(1:2)], col = pal_cluster[1:3],
         lty = 1, lwd = 2, bty = "n")
})
