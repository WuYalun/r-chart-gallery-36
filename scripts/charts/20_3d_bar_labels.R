#!/usr/bin/env Rscript
# 20 带类别标签的三维柱状图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

mat20 <- matrix(c(9, 14, 11, 16, 12, 8, 13, 18, 10, 7, 15, 12), nrow = 4, byrow = TRUE)
col20 <- colorRampPalette(c("#8da0cb", "#66c2a5", "#fee08b", "#fc8d62"))(12)
save_base(file.path(FIG_DIR, "20_3d_bar_labels.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp_empty(c(0.3, 4.7), c(0.3, 3.7), c(0, 22), theta = -38, phi = 22,
                      xlab = "Quarter", ylab = "Region", zlab = "Sales",
                      main = "20 3D bar with category labels")
  k <- 1
  for (j in 3:1) for (i in 1:4) {
    draw_cube(pmat, i, j, 0, mat20[i, j], dx = 0.28, dy = 0.22, col = col20[k])
    lab <- trans3d(i, j, mat20[i, j] + 0.8, pmat)
    text(lab$x, lab$y, labels = mat20[i, j], cex = 0.7)
    k <- k + 1
  }
})
