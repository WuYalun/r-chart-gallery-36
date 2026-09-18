#!/usr/bin/env Rscript
# 01 山脊图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(ggridges))

ridge_s <- bind_rows(lapply(1:12, function(i) {
  data.frame(
    value = rnorm(350, mean = 2.2 + i * 0.22, sd = 0.55 + 0.04 * (i %% 3)),
    series = factor(sprintf("Series %s", LETTERS[13 - i]),
                    levels = sprintf("Series %s", LETTERS[12:1]))
  )
}))
p <- ggplot(ridge_s, aes(x = value, y = series, fill = series, color = series)) +
  geom_density_ridges(scale = 1.35, rel_min_height = 0.01, alpha = 0.92, linewidth = 0.25) +
  scale_fill_manual(values = pal_ridge) +
  scale_color_manual(values = pal_ridge) +
  labs(title = "01 Ridgeline plot", subtitle = "ggridges::geom_density_ridges",
       x = "Value", y = NULL) +
  theme_ridges() + theme(legend.position = "none")
save_gg(p, file.path(FIG_DIR, "01_ridgeline.png"))
