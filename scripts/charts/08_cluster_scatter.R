#!/usr/bin/env Rscript
# 08 聚类散点 / 聚类场
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(8)
centers <- data.frame(
  cx = c(-3.2, -1.2, 0.6, 2.8, -2.4, 0.2, 2.2, -0.8, 3.2, 1.4, -3.5, 2.8),
  cy = c( 2.8,  3.1, 2.5, 2.9,  0.4, 0.6, 0.2, -2.2, -1.8, -3.0, -0.8, 1.4),
  cl = factor(paste0("C", 1:12))
)
grid <- expand.grid(x = seq(-4.2, 4.2, length.out = 180), y = seq(-3.6, 3.6, length.out = 150))
dx <- outer(grid$x, centers$cx, "-")
dy <- outer(grid$y, centers$cy, "-")
nn <- max.col(-(dx^2 + dy^2), ties.method = "first")
grid$cluster <- centers$cl[nn]
hole <- with(grid, (x > -1.6 & x < 0.2 & y > 0.8 & y < 2.2) |
               (x > 1.0 & x < 2.4 & y > -0.4 & y < 1.0) |
               (x > -0.4 & x < 1.2 & y > -2.6 & y < -1.4))
grid$cluster[hole] <- NA
p <- ggplot(grid, aes(x, y, fill = cluster)) +
  geom_raster() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(12),
                    na.value = "#d9f0ff", name = "Cluster") +
  coord_fixed(expand = FALSE) +
  labs(title = "08 Cluster scatter / cluster map",
       subtitle = "Nearest-center spatial map",
       x = "X", y = "Y") +
  theme_ex() + theme(panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "08_cluster_scatter.png"))
