#!/usr/bin/env Rscript
# 19 三角气泡热图
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
df19 <- mat_df(m14)
df19$i <- as.integer(df19$x)
df19$j <- nlevels(df19$y) + 1 - as.integer(df19$y)
df19 <- df19[df19$i <= df19$j, ]
p <- ggplot(df19, aes(x, y)) +
  geom_tile(fill = "#f7f7f7", color = "#d0d5dd", linewidth = 0.35) +
  geom_point(aes(fill = value, size = abs(value)), shape = 21, color = "#4b5563", stroke = 0.35) +
  scale_fill_gradientn(
    colors = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"),
    limits = c(-1, 1), name = "r"
  ) +
  scale_size(range = c(2.2, 9.5), guide = "none") +
  coord_fixed() +
  labs(title = "19 Triangle bubble heatmap",
       subtitle = "Lower triangle: cell grid + sized bubbles",
       x = NULL, y = NULL) +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "19_triangle_bubble.png"))
