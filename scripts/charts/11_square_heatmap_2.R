#!/usr/bin/env Rscript
# 11 方块热图 2
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

set.seed(11)
labs11 <- paste0("V", sprintf("%02d", 1:10))
X11 <- matrix(rnorm(120 * 10), 120, 10)
grp11 <- c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4)
shared <- sapply(1:4, function(g) rnorm(120))
for (j in seq_along(grp11)) {
  X11[, j] <- 0.45 * X11[, j] + 1.55 * shared[, grp11[j]] +
    0.25 * shared[, ((grp11[j] %% 4) + 1)]
}
m11 <- cor(X11)
dimnames(m11) <- list(labs11, labs11)
df11 <- mat_df(m11, "V", "V")
p <- ggplot(df11, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.9, height = 0.9, linewidth = 0.65) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), limits = c(-1, 1),
                       name = "r", breaks = c(-1, 0, 1)) +
  coord_fixed() +
  labs(title = "11 Square heatmap 2",
       subtitle = "ggplot2::geom_tile on a named 10 x 10 correlation matrix",
       x = NULL, y = NULL) +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "11_square_heatmap_2.png"))
