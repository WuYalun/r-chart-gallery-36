#!/usr/bin/env Rscript
# 12 局部放大图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(cowplot))

set.seed(12)
d12 <- data.frame(x = cars$speed + rnorm(nrow(cars), 0, 0.2), y = cars$dist)
fit <- lm(y ~ x, d12)
p_main <- ggplot(d12, aes(x, y)) +
  geom_point(color = "#2b6cb0", size = 2.2) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "#c53030", linewidth = 0.7) +
  annotate("rect", xmin = 12, xmax = 18, ymin = 18, ymax = 48,
           color = "#c53030", fill = NA, linewidth = 0.6) +
  labs(title = "12 Inset / local zoom", subtitle = "cowplot inset of the boxed region",
       x = "Speed", y = "Stopping distance") +
  theme_ex()
p_zoom <- ggplot(d12, aes(x, y)) +
  geom_point(color = "#2b6cb0", size = 2.4) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "#c53030", linewidth = 0.7) +
  coord_cartesian(xlim = c(12, 18), ylim = c(18, 48), expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Zoom") +
  theme_ex(base_size = 9) +
  theme(plot.title = element_text(size = 10),
        panel.border = element_rect(color = "#c53030", fill = NA, linewidth = 0.8),
        plot.background = element_rect(fill = "white", color = "grey70"))
p <- ggdraw(p_main) + draw_plot(p_zoom, x = 0.52, y = 0.12, width = 0.42, height = 0.42)
save_gg(p, file.path(FIG_DIR, "12_inset_zoom.png"))
