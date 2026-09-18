#!/usr/bin/env Rscript
# 18b 相关性气泡 + 聚类树
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages({
  library(linkET)
  library(ComplexHeatmap)
  library(circlize)
  library(grid)
})

set.seed(18)
n <- 80
env <- data.frame(
  Temp = rnorm(n), pH = rnorm(n), DO = rnorm(n), TN = rnorm(n), TP = rnorm(n),
  TOC = rnorm(n), NH4 = rnorm(n), Cond = rnorm(n), Sal = rnorm(n)
)
env$DO  <- -0.82 * env$Temp + rnorm(n, sd = 0.28)
env$TN  <-  0.85 * env$TP   + rnorm(n, sd = 0.22)
env$TOC <-  0.72 * env$TN   + rnorm(n, sd = 0.28)
env$NH4 <-  0.68 * env$TN   + rnorm(n, sd = 0.30)
env$Sal <- -0.58 * env$Temp + rnorm(n, sd = 0.35)
env$pH  <-  0.35 * env$Temp + 0.72 * env$pH
cm <- correlate(env)$r
col_fun <- colorRamp2(c(-1, 0, 1), c("#1b7837", "#f7f7f7", "#762a83"))
ht <- Heatmap(
  cm, name = "r", col = col_fun, rect_gp = gpar(type = "none"),
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.rect(x, y, width, height, gp = gpar(col = "#d0d5dd", fill = "white", lwd = 0.4))
    grid.circle(
      x, y,
      r = (abs(cm[i, j]) * 0.42 + 0.08) * min(unit.c(width, height)),
      gp = gpar(fill = col_fun(cm[i, j]), col = "#4b5563", lwd = 0.4)
    )
  },
  cluster_rows = TRUE, cluster_columns = TRUE,
  row_dend_width = unit(16, "mm"), column_dend_height = unit(16, "mm"),
  row_names_gp = gpar(fontsize = 9), column_names_gp = gpar(fontsize = 9),
  heatmap_legend_param = list(title = "r", at = c(-1, 0, 1))
)
ragg::agg_png(file.path(FIG_DIR, "18_corr_bubble_dendrogram.png"),
              width = 7.6, height = 6.4, units = "in", res = DPI, background = "white")
draw(ht, column_title = "18 Correlation bubbles + hclust dendrogram",
     column_title_gp = gpar(fontsize = 13, fontface = "bold"))
dev.off()
message("wrote ", file.path(FIG_DIR, "18_corr_bubble_dendrogram.png"))
