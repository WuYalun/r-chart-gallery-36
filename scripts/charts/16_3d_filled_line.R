#!/usr/bin/env Rscript
# 16 三维填充折线图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

x <- seq(0, 10, length.out = 80)
series <- list(
  pmax(0.2, 2.2 * dnorm(x, 3.2, 1.1) * 6 + 0.15 * sin(x)),
  pmax(0.2, 2.0 * dnorm(x, 5.0, 1.3) * 6 + 0.2),
  pmax(0.2, 1.8 * dnorm(x, 6.2, 1.0) * 6),
  pmax(0.2, 1.6 * dnorm(x, 4.0, 1.6) * 5 + 0.3)
)
cols16 <- c("#fee08b", "#66c2a5", "#8da0cb", "#e78ac3")
save_base(file.path(FIG_DIR, "16_3d_filled_line.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp_empty(c(0, 10), c(0.4, 4.6), c(0, 3.2), theta = 38, phi = 22,
                      xlab = "Time", ylab = "Series", zlab = "Value",
                      main = "16 3D filled line  |  area walls via trans3d")
  for (s in 4:1) {
    xs <- c(x, rev(x))
    ys <- c(rep(s, length(x)), rev(rep(s, length(x))))
    zs <- c(series[[s]], rep(0, length(x)))
    pts <- trans3d(xs, ys, zs, pmat)
    polygon(pts, col = adjustcolor(cols16[s], 0.92), border = "grey25", lwd = 0.4)
    line <- trans3d(x, rep(s, length(x)), series[[s]], pmat)
    lines(line, col = "grey20", lwd = 1)
  }
})
