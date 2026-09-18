#!/usr/bin/env Rscript
# 07 线型热图（序列 / 谱图）
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

t <- 1:160
series <- lapply(1:16, function(i) {
  base <- sin(t / (9 + i / 5) + i / 2.4) * exp(-((t - 28 - 7 * i) / 22)^2)
  base + 0.18 * sin(t / 4.2 + i) + 0.05 * rnorm(length(t))
})
mat07 <- do.call(rbind, series)
df07 <- as.data.frame(as.table(mat07))
names(df07) <- c("series", "time", "z")
df07$series <- as.integer(df07$series)
df07$time <- as.integer(df07$time)
p <- ggplot(df07, aes(time, series, fill = z)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "intensity") +
  scale_y_reverse(breaks = 1:16) +
  labs(title = "07 Linear heatmap (series)", subtitle = "ggplot2::geom_raster of 16 synthetic series",
       x = "Time", y = "Series") +
  theme_ex() + theme(panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "07_linear_heatmap_series.png"))
