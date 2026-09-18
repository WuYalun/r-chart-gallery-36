#!/usr/bin/env Rscript
# 15 华夫图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(15)
counts <- c(A = 10, B = 8, C = 7, D = 5, E = 4, F = 2)
tiles <- sample(unlist(mapply(rep, names(counts), counts)))
waffle <- data.frame(x = rep(1:6, each = 6), y = rep(6:1, 6),
                     cat = factor(tiles, levels = names(counts)))
p <- ggplot(waffle, aes(x, y, fill = cat)) +
  geom_tile(color = "white", linewidth = 1.2, width = 0.92, height = 0.92) +
  scale_fill_manual(values = c("#8da0cb", "#66c2a5", "#fc8d62", "#e78ac3", "#a6d854", "#ffd92f"),
                    name = "Group") +
  coord_equal() +
  labs(title = "15 Waffle chart", subtitle = "6x6 square pie", x = NULL, y = NULL) +
  theme_ex() + theme(axis.text = element_blank(), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "15_waffle.png"))
