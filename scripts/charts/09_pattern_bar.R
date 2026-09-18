#!/usr/bin/env Rscript
# 09 带填充纹理的柱状图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(ggpattern))

d9 <- expand.grid(
  cond = factor(c("Type1", "Type2", "Type3"), levels = c("Type1", "Type2", "Type3")),
  series = factor(c("A", "B", "C", "D"))
)
d9$value <- c(18, 17, 19, 16, 20, 18, 17, 21, 15, 19, 18, 16)
p <- ggplot(d9, aes(cond, value, fill = series, pattern = series)) +
  geom_col_pattern(position = position_dodge(0.88), width = 0.82,
                   color = "grey20", linewidth = 0.3,
                   pattern_fill = "grey20", pattern_color = "grey20",
                   pattern_density = 0.28, pattern_spacing = 0.03) +
  scale_pattern_manual(values = c(A = "stripe", B = "crosshatch", C = "circle", D = "stripe")) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  labs(title = "09 Pattern-filled bar chart", subtitle = "ggpattern::geom_col_pattern",
       x = "Condition", y = "Value") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "09_pattern_bar.png"))
