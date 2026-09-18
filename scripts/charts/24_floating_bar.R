#!/usr/bin/env Rscript
# 24 悬浮柱状图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

x <- 1:14
mid <- 9 + 7 * dnorm(x, 7.5, 2.6) / dnorm(7.5, 7.5, 2.6)
half <- 3.2 + 0.35 * sin(x / 2)
d24 <- data.frame(x = x, xmin = x - 0.32, xmax = x + 0.32,
                  ymin = mid - half, ymax = mid + half)
p <- ggplot(d24) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#2c7c7c", color = "grey20", linewidth = 0.25) +
  scale_x_continuous(breaks = x, labels = x) +
  labs(title = "24 Floating bar chart",
       subtitle = "geom_rect range bars (not rooted at 0)",
       x = "Sample", y = "Value") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "24_floating_bar.png"))
