#!/usr/bin/env Rscript
# 17 气泡热图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(17)
nr <- 12; nc <- 14
m17 <- block_matrix(nr, nc, 3, 4, noise = 0.1)
m17 <- rescale(m17 + 0.12 * sin(row(m17) / 2.2 + col(m17) / 3), to = c(0.18, 1))
rownames(m17) <- paste0("Gene", sprintf("%02d", seq_len(nr)))
colnames(m17) <- paste0("S", sprintf("%02d", seq_len(nc)))
df17 <- mat_df(m17)
p <- ggplot(df17, aes(x, y)) +
  geom_tile(fill = "#f8f8f8", color = "#d0d5dd", width = 1, height = 1, linewidth = 0.35) +
  geom_point(aes(fill = value, size = value), shape = 21, color = "#4b5563", stroke = 0.35) +
  scale_fill_gradientn(
    colors = c("#3f007d", "#6a51a3", "#9e9ac8", "#cbc9e2", "#f2f0f7", "#fee6ce", "#fdae6b", "#e6550d"),
    name = "value"
  ) +
  scale_size(range = c(4.0, 8.8), guide = "none") +
  coord_fixed() +
  labs(title = "17 Bubble heatmap",
       subtitle = "geom_tile grid + geom_point(shape=21)",
       x = NULL, y = NULL) +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "17_bubble_heatmap.png"), width = 8.0, height = 6.0)
