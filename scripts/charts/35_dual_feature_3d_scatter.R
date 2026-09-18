#!/usr/bin/env Rscript
# 35 双特征渲染三维散点图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(scatterplot3d))

set.seed(35)
trunk <- data.frame(
  x = 0.06 * rnorm(90), y = 0.06 * rnorm(90), z = runif(90, 0, 2.0),
  feat = runif(90, 0.05, 0.25)
)
can <- data.frame(
  x = 0.7 * rnorm(500) * pmax(0.2, 1 - abs(rnorm(500, 0, 0.5))),
  y = 0.7 * rnorm(500) * pmax(0.2, 1 - abs(rnorm(500, 0, 0.5))),
  z = rnorm(500, 3.3, 0.55),
  feat = rnorm(500, 0.75, 0.15)
)
d35 <- rbind(trunk, can)
d35$feat <- pmin(pmax(d35$feat, 0), 1)
ramp <- viridis(100, option = "D")
cols <- ramp[pmax(1, cut(d35$feat, 100, labels = FALSE))]
save_base(file.path(FIG_DIR, "35_dual_feature_3d_scatter.png"), {
  par(mar = c(2.5, 2.5, 3, 1))
  scatterplot3d(
    d35$x, d35$y, d35$z, color = cols, pch = 16,
    cex.symbols = 0.35 + 0.7 * d35$feat,
    xlab = "X", ylab = "Y", zlab = "Z", angle = 52, grid = TRUE,
    main = "35 Dual-feature 3D scatter  |  color + size"
  )
})
