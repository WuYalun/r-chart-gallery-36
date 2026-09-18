#!/usr/bin/env Rscript
# 22 不等宽柱状图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

d22 <- data.frame(
  cat = c("A", "B", "C", "D"),
  xmin = c(0.4, 2.0, 3.3, 5.1),
  xmax = c(1.6, 3.0, 4.9, 5.7),
  ymax = c(8.2, 9.4, 7.1, 5.5)
)
d22$xmid <- (d22$xmin + d22$xmax) / 2
p <- ggplot(d22) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = ymax, fill = cat),
            color = "grey20", linewidth = 0.3) +
  geom_text(aes(x = xmid, y = ymax + 0.35, label = cat), size = 4) +
  scale_fill_manual(values = c("#8da0cb", "#66c2a5", "#a6d854", "#fee08b")) +
  scale_x_continuous(breaks = d22$xmid, labels = d22$cat) +
  labs(title = "22 Unequal-width bar chart",
       subtitle = "ggplot2::geom_rect · variable bar widths",
       x = "Category", y = "Value") +
  theme_ex() + theme(legend.position = "none")
save_gg(p, file.path(FIG_DIR, "22_unequal_width_bar.png"))
