#!/usr/bin/env Rscript
# 25 三维密度图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(25)
xx <- c(rnorm(400, 0, 0.7), rnorm(120, 1.2, 0.5))
yy <- c(rnorm(400, 0, 0.7), rnorm(120, 0.4, 0.5))
kd <- kde2d(xx, yy, n = 45, lims = c(-2.5, 2.5, -2.5, 2.5))
nr <- length(kd$x)
facet_col <- viridis(64, option = "plasma")
zcol <- facet_col[cut(kd$z[-nr, -nr], 64, labels = FALSE)]
save_base(file.path(FIG_DIR, "25_3d_density_scatter.png"), {
  par(mar = c(1.2, 1, 3, 4))
  persp(
    kd$x, kd$y, kd$z, theta = 40, phi = 28, expand = 0.6,
    col = zcol, border = NA, ticktype = "detailed",
    xlab = "X", ylab = "Y", zlab = "Density",
    main = "25 3D density  |  persp of MASS::kde2d"
  )
})
