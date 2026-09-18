#!/usr/bin/env Rscript
# 32 小提琴图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(32)
d32 <- bind_rows(lapply(1:6, function(i) {
  data.frame(grp = paste0("G", i),
             y = rnorm(120, mean = 5 + (i %% 3) * 0.6, sd = 0.7 + 0.15 * (i %% 2)))
}))
p <- ggplot(d32, aes(grp, y, fill = grp)) +
  geom_violin(trim = FALSE, color = "grey20", linewidth = 0.3, alpha = 0.9) +
  geom_boxplot(width = 0.1, outlier.size = 0.5, fill = "white") +
  scale_fill_manual(values = pal_ridge[c(1, 3, 5, 7, 9, 11)]) +
  labs(title = "32 Violin plot", subtitle = "ggplot2::geom_violin across 6 groups",
       x = NULL, y = "Value") +
  theme_ex() + theme(legend.position = "none")
save_gg(p, file.path(FIG_DIR, "32_violin.png"))
