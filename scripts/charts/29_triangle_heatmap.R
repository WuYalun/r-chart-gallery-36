#!/usr/bin/env Rscript
# 29 三角热图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

cars_num <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec", "am", "gear")]
m29 <- cor(cars_num)
df29 <- mat_df(m29)
df29$i <- as.integer(df29$x)
df29$j <- nlevels(df29$y) + 1 - as.integer(df29$y)
df29 <- df29[df29$i <= df29$j, ]
p <- ggplot(df29, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.9, height = 0.9, linewidth = 0.55) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), limits = c(-1, 1), name = "r") +
  coord_fixed() +
  labs(title = "29 Triangle heatmap",
       subtitle = "Lower-triangle geom_tile, RdBu, white gaps",
       x = NULL, y = NULL) +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "29_triangle_heatmap.png"))
