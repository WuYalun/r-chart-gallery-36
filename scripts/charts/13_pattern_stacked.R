#!/usr/bin/env Rscript
# 13 带填充纹理的堆叠图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(ggpattern))

d13 <- expand.grid(
  month = factor(month.abb[1:8], levels = month.abb[1:8]),
  part = factor(c("Bottom", "Mid", "Top"), levels = c("Bottom", "Mid", "Top"))
)
d13$value <- c(6, 7, 5, 8, 6, 7, 8, 5, 5, 4, 6, 5, 7, 5, 4, 6, 4, 5, 4, 3, 4, 5, 3, 4)
p <- ggplot(d13, aes(month, value, fill = part, pattern = part)) +
  geom_col_pattern(position = "stack", width = 0.7, color = "grey20", linewidth = 0.25,
                   pattern_fill = "grey20", pattern_color = "grey20",
                   pattern_density = 0.3, pattern_spacing = 0.03) +
  scale_pattern_manual(values = c(Bottom = "stripe", Mid = "crosshatch", Top = "circle")) +
  scale_fill_manual(values = c("#66c2a5", "#8da0cb", "#fee08b")) +
  labs(title = "13 Pattern-filled stacked bar", subtitle = "ggpattern stacked columns",
       x = "Month", y = "Count") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "13_pattern_stacked.png"))
