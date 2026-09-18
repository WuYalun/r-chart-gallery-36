#!/usr/bin/env Rscript
# 05 风玫瑰图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

dir_lab <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
ws_br <- c("0-2", "2-4", "4-6", "6-8", ">8")
wind <- expand.grid(dir = factor(dir_lab, levels = dir_lab), ws = factor(ws_br, levels = ws_br))
wgt <- (cos((as.numeric(wind$dir) - 11) / 16 * 2 * pi) + 1.2)^1.6
wind$freq <- pmax(rpois(nrow(wind), lambda = 4 * wgt * as.numeric(wind$ws) / 3), 0)
p <- ggplot(wind, aes(x = dir, y = freq, fill = ws)) +
  geom_col(width = 1, color = "white", linewidth = 0.2) +
  coord_polar(start = -pi / 16) +
  scale_fill_brewer(palette = "YlGnBu", name = "Wind speed\n(m/s)") +
  labs(title = "05 Wind rose", subtitle = "ggplot2::geom_col + coord_polar",
       x = NULL, y = "Frequency") +
  theme_ex() +
  theme(axis.text.y = element_blank(), panel.grid.major.x = element_line(color = "grey80"))
save_gg(p, file.path(FIG_DIR, "05_wind_rose.png"))
