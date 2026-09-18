#!/usr/bin/env Rscript
# 03 方块热图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(3)
m03 <- block_matrix(10, 10, 4, 4, noise = 0.22)
m03 <- m03 + 0.18 * sin(row(m03) / 1.7) * cos(col(m03) / 1.9)
df03 <- mat_df(m03, "C", "R")
p <- ggplot(df03, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.88, height = 0.88, linewidth = 0.7) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "value") +
  coord_fixed() +
  labs(title = "03 Square heatmap", subtitle = "ggplot2::geom_tile with white cell gaps",
       x = NULL, y = NULL) +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "03_square_heatmap.png"))
