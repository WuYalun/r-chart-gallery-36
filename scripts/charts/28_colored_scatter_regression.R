#!/usr/bin/env Rscript
# 28 着色散点 + 回归线
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

n <- 800
x <- rnorm(n, 50, 9)
y <- 0.9 * x + rnorm(n, 5, 3.8)
z <- y + rnorm(n, 0, 2)
d28 <- data.frame(x, y, z)
p <- ggplot(d28, aes(x, y, color = z)) +
  geom_point(size = 1.6, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = "grey10", linewidth = 0.75) +
  scale_color_viridis_c(option = "viridis", name = "depth") +
  labs(title = "28 Colored scatter + regression",
       subtitle = "2D colored scatter with linear fit",
       x = "Observed", y = "Estimated depth") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "28_3d_cluster_regression.png"))
