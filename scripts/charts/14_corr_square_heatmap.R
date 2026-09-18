#!/usr/bin/env Rscript
# 14 相关性方块热图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(14)
n14 <- 12
X14 <- matrix(rnorm(100 * n14), 100, n14)
grp14 <- rep(1:4, each = 3)
for (g in unique(grp14)) {
  idx <- which(grp14 == g)
  X14[, idx] <- X14[, idx] + 1.5 * rnorm(100)
}
colnames(X14) <- c("Temp", "pH", "DO", "TN", "TP", "TOC", "NH4", "NO3", "Cond", "Sal", "ORP", "Chl")
m14 <- cor(X14)
ord14 <- hclust(as.dist(1 - abs(m14)))$order
m14 <- m14[ord14, ord14]
df14 <- mat_df(m14)
p <- ggplot(df14, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.9, height = 0.9, linewidth = 0.6) +
  scale_fill_gradientn(
    colors = c("#1b7837", "#a6dba0", "#f7f7f7", "#c2a5cf", "#762a83"),
    limits = c(-1, 1), name = "r"
  ) +
  coord_fixed() +
  labs(title = "14 Correlation square heatmap",
       subtitle = "ggplot2::geom_tile, hclust-ordered, PRGn palette",
       x = NULL, y = NULL) +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "14_corr_square_heatmap.png"))
