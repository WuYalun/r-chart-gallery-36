#!/usr/bin/env Rscript
# 02 线型热图（条带）
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

t <- seq(0, 20, length.out = 160)
stripes <- bind_rows(lapply(1:16, function(i) {
  z <- sin(t / (1.8 + i / 8) + i / 3) * exp(-((t - 6 - i * 0.4) / 7)^2) +
    0.35 * sin(t * 0.7 + i) + 0.08 * i
  data.frame(time = t, series = sprintf("Series %02d", i), z = z)
}))
p <- ggplot(stripes, aes(time, series, fill = z)) +
  geom_tile(height = 0.72, width = diff(t)[1] * 1.02) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "z") +
  scale_y_discrete(limits = rev) +
  labs(title = "02 Linear heatmap", subtitle = "ggplot2::geom_tile · one thin stripe per series",
       x = "Time", y = NULL) +
  theme_ex() + theme(panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "02_linear_heatmap.png"))
