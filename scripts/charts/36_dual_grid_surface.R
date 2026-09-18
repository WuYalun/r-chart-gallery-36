#!/usr/bin/env Rscript
# 36 双网格曲面图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

xs <- seq(-pi, pi, length.out = 24)
ys <- seq(-pi, pi, length.out = 24)
z1 <- outer(xs, ys, function(a, b) 0.7 * sin(a) * cos(b))
z2 <- outer(xs, ys, function(a, b) 0.45 * cos(0.8 * a) * sin(0.8 * b) + 0.9)
save_base(file.path(FIG_DIR, "36_dual_grid_surface.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp(
    xs, ys, z1, theta = 35, phi = 30, expand = 0.65, zlim = c(-1, 1.5),
    col = adjustcolor("#41b6c4", 0.55), border = "grey25", lwd = 0.3,
    ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z",
    main = "36 Dual grid surface"
  )
  nr <- length(xs)
  for (i in 1:(nr - 1)) for (j in 1:(nr - 1)) {
    xx <- c(xs[i], xs[i + 1], xs[i + 1], xs[i])
    yy <- c(ys[j], ys[j], ys[j + 1], ys[j + 1])
    zz <- c(z2[i, j], z2[i + 1, j], z2[i + 1, j + 1], z2[i, j + 1])
    pts <- trans3d(xx, yy, zz, pmat)
    polygon(pts, col = adjustcolor("#fee08b", 0.45), border = "grey30", lwd = 0.25)
  }
})
