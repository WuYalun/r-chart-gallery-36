#!/usr/bin/env Rscript
# 30 冲击图 / 堆叠填充柱
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

d30 <- expand.grid(
  sample = factor(paste0("S", 1:10), levels = paste0("S", 1:10)),
  layer = factor(c("L1", "L2", "L3", "L4"), levels = c("L1", "L2", "L3", "L4"))
)
d30$value <- c(
  4,5,6,5,7,6,8,7,5,4,
  3,4,4,5,4,5,4,5,4,3,
  5,4,5,4,6,5,5,4,5,4,
  2,3,2,3,2,3,3,2,3,2
)
p <- ggplot(d30, aes(sample, value, fill = layer)) +
  geom_col(position = "stack", width = 0.86, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("#fee08b", "#abdda4", "#66c2a5", "#3288bd"), name = "Layer") +
  labs(title = "30 Filled stacked bar / impact chart",
       subtitle = "ggplot2::geom_col(position='stack') from baseline",
       x = "Sample", y = "Value") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "30_streamgraph.png"))
