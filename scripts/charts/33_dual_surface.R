#!/usr/bin/env Rscript
# 33 双曲面图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

xs <- seq(-2, 2, length.out = 28)
ys <- seq(-2, 2, length.out = 28)
z1 <- outer(xs, ys, function(a, b) 1.4 * exp(-((a + 0.6)^2 + (b + 0.4)^2)))
z2 <- outer(xs, ys, function(a, b) 1.1 * exp(-((a - 0.7)^2 + (b - 0.5)^2)))
nr <- length(xs)
col1 <- colorRampPalette(c("#c7e9c0", "#41b6c4", "#225ea8"))(64)
col2 <- colorRampPalette(c("#fee08b", "#fdae61", "#f46d43"))(64)
save_base(file.path(FIG_DIR, "33_dual_surface.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp(
    xs, ys, z1, theta = 38, phi = 28, expand = 0.7, zlim = c(0, 1.6),
    col = col1[cut(z1[-nr, -nr], 64, labels = FALSE)], border = NA,
    ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z",
    main = "33 Dual surface"
  )
  for (i in 1:(nr - 1)) for (j in 1:(nr - 1)) {
    xx <- c(xs[i], xs[i + 1], xs[i + 1], xs[i])
    yy <- c(ys[j], ys[j], ys[j + 1], ys[j + 1])
    zz <- c(z2[i, j], z2[i + 1, j], z2[i + 1, j + 1], z2[i, j + 1])
    pts <- trans3d(xx, yy, zz, pmat)
    ci <- cut(mean(zz), breaks = seq(min(z2), max(z2) + 1e-8, length.out = 65), labels = FALSE)
    polygon(pts, col = adjustcolor(col2[ci], 0.7), border = NA)
  }
})
