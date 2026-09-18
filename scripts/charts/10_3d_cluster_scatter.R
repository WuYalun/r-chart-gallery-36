#!/usr/bin/env Rscript
# 10 三维聚类散点图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(scatterplot3d))

set.seed(10)
map <- expand.grid(x = seq(-2, 2, length.out = 70), y = seq(-2, 2, length.out = 70))
map$z <- 1.2 * exp(-((map$x + 0.3)^2 + (map$y - 0.2)^2) / 1.4) +
  0.7 * exp(-((map$x - 1)^2 + (map$y + 0.8)^2) / 0.9)
map$cl <- cut(atan2(map$y, map$x) + 0.4 * map$z, breaks = 8, labels = paste0("C", 1:8))
samp <- map[sample.int(nrow(map), 1800), ]
cols10 <- brewer.pal(8, "Set2")[as.integer(samp$cl)]
save_base(file.path(FIG_DIR, "10_3d_cluster_scatter.png"), {
  par(mar = c(2.5, 2.5, 3, 1))
  scatterplot3d(
    samp$x, samp$y, samp$z, color = cols10, pch = 16, cex.symbols = 0.45,
    xlab = "X", ylab = "Y", zlab = "Z", angle = 55, grid = TRUE,
    main = "10 3D cluster scatter"
  )
  legend("topright", legend = levels(samp$cl), pch = 16, col = brewer.pal(8, "Set2"),
         bty = "n", cex = 0.7, ncol = 2)
})
