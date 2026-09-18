#!/usr/bin/env Rscript
# 04 三维堆叠柱状图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

layers <- list(
  matrix(c(3, 4, 5, 3, 2, 4, 5, 3, 4, 2, 3, 5), nrow = 4),
  matrix(c(4, 3, 4, 5, 3, 5, 3, 4, 3, 4, 5, 3), nrow = 4),
  matrix(c(2, 3, 2, 2, 4, 2, 3, 2, 2, 3, 2, 2), nrow = 4)
)
cols4 <- c("#8da0cb", "#66c2a5", "#fc8d62")
save_base(file.path(FIG_DIR, "04_3d_stacked_bar.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp_empty(c(0.3, 4.7), c(0.3, 3.7), c(0, 16), theta = 42, phi = 24,
                      xlab = "Group", ylab = "Dose", zlab = "Value",
                      main = "04 3D stacked bar  |  persp + trans3d cubes")
  for (j in 3:1) {
    for (i in 1:4) {
      z0 <- 0
      for (k in 1:3) {
        z1 <- z0 + layers[[k]][i, j]
        draw_cube(pmat, i, j, z0, z1, dx = 0.28, dy = 0.22, col = cols4[k])
        z0 <- z1
      }
    }
  }
  legend("topright", legend = c("Layer A", "Layer B", "Layer C"),
         fill = cols4, bty = "n", cex = 0.85)
})
