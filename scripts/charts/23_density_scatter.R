#!/usr/bin/env Rscript
# 23 密度散点图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

n <- 900
x <- rnorm(n, 50, 10)
y <- 0.85 * x + rnorm(n, 8, 4.5)
d23 <- data.frame(x, y, dens = {
  dens <- kde2d(x, y, n = 80)
  dens$z[cbind(findInterval(x, dens$x), findInterval(y, dens$y))]
})
p <- ggplot(d23, aes(x, y, color = dens)) +
  geom_point(size = 1.5, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = "grey15", linewidth = 0.7) +
  scale_color_viridis_c(option = "viridis", name = "density") +
  labs(title = "23 Density scatter", subtitle = "Points colored by kde2d + regression line",
       x = "Observed", y = "Estimated depth") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "23_density_scatter.png"))
