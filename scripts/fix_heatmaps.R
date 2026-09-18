#!/usr/bin/env Rscript
# Second-pass heatmap / correlation-plot restyle.
# Fixes broken #11, restyles #17, adds correlation connecting arms to #18,
# and tightens #03 / #07 / #14 / #19 / #21 / #29.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(RColorBrewer)
  library(linkET)
  library(ComplexHeatmap)
  library(circlize)
  library(grid)
})

set.seed(20260918)
FIG_DIR <- "/Users/aaron/Desktop/report/output/r-chart-gallery-20260918/figures"
W <- 7.2; H <- 5.6; DPI <- 160

theme_ex <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#1f2933"),
      plot.subtitle = element_text(size = 9, color = "#6b7280"),
      plot.caption = element_text(size = 8, color = "#9aa0a6"),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}
save_gg <- function(p, path, width = W, height = H) {
  ggsave(path, p, width = width, height = height, dpi = DPI, bg = "white")
}

mat_df <- function(m, x_prefix = "C", y_prefix = "R") {
  if (is.null(colnames(m))) colnames(m) <- paste0(x_prefix, sprintf("%02d", seq_len(ncol(m))))
  if (is.null(rownames(m))) rownames(m) <- paste0(y_prefix, sprintf("%02d", seq_len(nrow(m))))
  df <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
  names(df) <- c("y", "x", "value")
  df$x <- factor(df$x, levels = colnames(m))
  df$y <- factor(df$y, levels = rev(rownames(m)))
  df
}

block_matrix <- function(nr, nc, n_row_g = 3, n_col_g = 4, noise = 0.12) {
  rg <- cut(seq_len(nr), n_row_g, labels = FALSE)
  cg <- cut(seq_len(nc), n_col_g, labels = FALSE)
  m <- outer(rg, cg, function(a, b) a * 0.55 + b)
  m <- m + matrix(rnorm(nr * nc, 0, noise * diff(range(m))), nr, nc)
  rescale(m, to = c(0.08, 1))
}

# ---------------------------------------------------------------------------
# 03 Square heatmap: mosaic tiles with white gaps (not a smooth bullseye)
# ---------------------------------------------------------------------------
message("03 square heatmap")
set.seed(3)
m03 <- block_matrix(10, 10, 4, 4, noise = 0.22)
m03 <- m03 + 0.18 * sin(row(m03) / 1.7) * cos(col(m03) / 1.9)
df03 <- mat_df(m03, "C", "R")
p03 <- ggplot(df03, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.88, height = 0.88, linewidth = 0.7) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "value") +
  coord_fixed() +
  labs(title = "03 Square heatmap",
       subtitle = "ggplot2::geom_tile with white cell gaps",
       x = NULL, y = NULL, caption = "Demo 10 x 10 mosaic matrix") +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))
save_gg(p03, file.path(FIG_DIR, "03_square_heatmap.png"))

# ---------------------------------------------------------------------------
# 07 Linear heatmap of series: continuous raster, no checkerboard
# ---------------------------------------------------------------------------
message("07 linear heatmap series")
t <- 1:160
series <- lapply(1:16, function(i) {
  base <- sin(t / (9 + i / 5) + i / 2.4) * exp(-((t - 28 - 7 * i) / 22)^2)
  base + 0.18 * sin(t / 4.2 + i) + 0.05 * rnorm(length(t))
})
mat07 <- do.call(rbind, series)
df07 <- as.data.frame(as.table(mat07))
names(df07) <- c("series", "time", "z")
df07$series <- as.integer(df07$series)
df07$time <- as.integer(df07$time)
p07 <- ggplot(df07, aes(time, series, fill = z)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "intensity") +
  scale_y_reverse(breaks = 1:16) +
  labs(title = "07 Linear heatmap (series)",
       subtitle = "ggplot2::geom_raster of 16 synthetic series",
       x = "Time", y = "Series", caption = "Demo spectrogram-like matrix") +
  theme_ex()
save_gg(p07, file.path(FIG_DIR, "07_linear_heatmap_series.png"))

# ---------------------------------------------------------------------------
# 11 Square heatmap 2: named red-blue gapped tiles (was a solid NA square)
# ---------------------------------------------------------------------------
message("11 square heatmap 2")
set.seed(11)
labs11 <- paste0("V", sprintf("%02d", 1:10))
X11 <- matrix(rnorm(120 * 10), 120, 10)
grp11 <- c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4)
shared <- sapply(1:4, function(g) rnorm(120))
for (j in seq_along(grp11)) {
  X11[, j] <- 0.45 * X11[, j] + 1.55 * shared[, grp11[j]] + 0.25 * shared[, ((grp11[j] %% 4) + 1)]
}
m11 <- cor(X11)
dimnames(m11) <- list(labs11, labs11)
df11 <- mat_df(m11, "V", "V")
p11 <- ggplot(df11, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.9, height = 0.9, linewidth = 0.65) +
  scale_fill_gradientn(
    colors = rev(brewer.pal(11, "RdBu")), limits = c(-1, 1),
    name = "r", breaks = c(-1, 0, 1)
  ) +
  coord_fixed() +
  labs(title = "11 Square heatmap 2",
       subtitle = "ggplot2::geom_tile on a named 10 x 10 correlation matrix",
       x = NULL, y = NULL, caption = "Demo block-correlated variables") +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))
save_gg(p11, file.path(FIG_DIR, "11_square_heatmap_2.png"))

# ---------------------------------------------------------------------------
# 14 Correlation square heatmap: purple-green gapped squares
# ---------------------------------------------------------------------------
message("14 correlation square heatmap")
set.seed(14)
n14 <- 12
X14 <- matrix(rnorm(100 * n14), 100, n14)
grp14 <- rep(1:4, each = 3)
for (g in unique(grp14)) {
  idx <- which(grp14 == g)
  X14[, idx] <- X14[, idx] + 1.5 * rnorm(100)
}
colnames(X14) <- c("Temp", "pH", "DO", "TN", "TP", "TOC", "NH4", "NO3", "Cond", "Sal", "ORP", "Chl")
m14 <- cor(X14)
ord14 <- hclust(as.dist(1 - abs(m14)))$order
m14 <- m14[ord14, ord14]
df14 <- mat_df(m14)
p14 <- ggplot(df14, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.9, height = 0.9, linewidth = 0.6) +
  scale_fill_gradientn(
    colors = c("#1b7837", "#a6dba0", "#f7f7f7", "#c2a5cf", "#762a83"),
    limits = c(-1, 1), name = "r"
  ) +
  coord_fixed() +
  labs(title = "14 Correlation square heatmap",
       subtitle = "ggplot2::geom_tile, hclust-ordered, PRGn palette",
       x = NULL, y = NULL, caption = "Demo environmental correlation matrix") +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))
save_gg(p14, file.path(FIG_DIR, "14_corr_square_heatmap.png"))

# ---------------------------------------------------------------------------
# 17 Bubble heatmap: dense purple sequential grid with cell frames
# ---------------------------------------------------------------------------
message("17 bubble heatmap")
set.seed(17)
nr <- 12; nc <- 14
m17 <- block_matrix(nr, nc, 3, 4, noise = 0.1)
m17 <- rescale(m17 + 0.12 * sin(row(m17) / 2.2 + col(m17) / 3), to = c(0.18, 1))
rownames(m17) <- paste0("Gene", sprintf("%02d", seq_len(nr)))
colnames(m17) <- paste0("S", sprintf("%02d", seq_len(nc)))
df17 <- mat_df(m17)
p17 <- ggplot(df17, aes(x, y)) +
  geom_tile(fill = "#f8f8f8", color = "#d0d5dd", width = 1, height = 1, linewidth = 0.35) +
  geom_point(aes(fill = value, size = value), shape = 21, color = "#4b5563", stroke = 0.35) +
  scale_fill_gradientn(
    colors = c("#3f007d", "#6a51a3", "#9e9ac8", "#cbc9e2", "#f2f0f7", "#fee6ce", "#fdae6b", "#e6550d"),
    name = "value"
  ) +
  scale_size(range = c(4.0, 8.8), guide = "none") +
  coord_fixed() +
  labs(title = "17 Bubble heatmap",
       subtitle = "geom_tile grid + geom_point(shape=21); size and fill encode value",
       x = NULL, y = NULL, caption = "Demo 14 samples x 12 genes") +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))
save_gg(p17, file.path(FIG_DIR, "17_bubble_heatmap.png"), width = 8.0, height = 6.0)

# ---------------------------------------------------------------------------
# 18 Correlation bubble heatmap + Mantel connecting arms (linkET)
# ---------------------------------------------------------------------------
message("18 correlation bubble + connecting arms")
set.seed(18)
n <- 80
env <- data.frame(
  Temp = rnorm(n),
  pH   = rnorm(n),
  DO   = rnorm(n),
  TN   = rnorm(n),
  TP   = rnorm(n),
  TOC  = rnorm(n),
  NH4  = rnorm(n),
  Cond = rnorm(n),
  Sal  = rnorm(n)
)
env$DO  <- -0.82 * env$Temp + rnorm(n, sd = 0.28)
env$TN  <-  0.85 * env$TP   + rnorm(n, sd = 0.22)
env$TOC <-  0.72 * env$TN   + rnorm(n, sd = 0.28)
env$NH4 <-  0.68 * env$TN   + rnorm(n, sd = 0.30)
env$Sal <- -0.58 * env$Temp + rnorm(n, sd = 0.35)
env$pH  <-  0.35 * env$Temp + 0.72 * env$pH

spec <- data.frame(
  cya1 =  1.8 * env$Temp - 1.1 * env$DO + rnorm(n, sd = 0.22),
  cya2 =  1.6 * env$Temp + rnorm(n, sd = 0.25),
  cya3 =  1.3 * env$Temp - 0.7 * env$Sal + rnorm(n, sd = 0.28),
  cya4 =  1.1 * env$DO   + rnorm(n, sd = 0.30),
  pro1 =  1.9 * env$TN   + rnorm(n, sd = 0.20),
  pro2 =  1.7 * env$TP   + rnorm(n, sd = 0.22),
  pro3 =  1.5 * env$TOC  + rnorm(n, sd = 0.25),
  pro4 =  1.2 * env$NH4  + rnorm(n, sd = 0.28),
  fir1 = -1.6 * env$pH   + rnorm(n, sd = 0.24),
  fir2 =  1.5 * env$Cond + rnorm(n, sd = 0.26),
  fir3 =  1.4 * env$Sal  + rnorm(n, sd = 0.26),
  fir4 =  1.1 * env$Cond - 0.8 * env$pH + rnorm(n, sd = 0.30)
)

mantel <- mantel_test(
  spec, env,
  spec_select = list(
    Cyano   = 1:4,
    Proteo  = 5:8,
    Firmic  = 9:12
  )
) |>
  mutate(
    rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
             labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
    pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
             labels = c("<= 0.01", "0.01 - 0.05", "> 0.05"))
  )

p18 <- qcorrplot(correlate(env), type = "full") +
  geom_tile(fill = NA, color = "#d0d5dd", linewidth = 0.35) +
  geom_point(aes(fill = r, size = abs(r)), shape = 21, color = "#4b5563", stroke = 0.35) +
  scale_fill_gradientn(
    colours = c("#1b7837", "#a6dba0", "#f7f7f7", "#c2a5cf", "#762a83"),
    limits = c(-1, 1), name = "Pearson's r"
  ) +
  scale_size(range = c(2.4, 8.6), guide = "none") +
  geom_couple(
    aes(colour = pd),
    data = mantel,
    curvature = nice_curvature(),
    size = 0.9,
    label.size = 3.6,
    nudge_x = 0.85
  ) +
  scale_colour_manual(values = c("#D95F02", "#1B9E77", "#c8c8c8"), name = "Mantel's p") +
  labs(
    title = "18 Correlation bubble heatmap",
    subtitle = "linkET::qcorrplot bubbles + geom_couple correlation connecting arms",
    x = NULL, y = NULL,
    caption = "Demo: env Pearson bubbles (size = |r|); arms = Mantel test to taxa groups"
  ) +
  theme_ex() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    plot.margin = margin(8, 22, 8, 8)
  )
save_gg(p18, file.path(FIG_DIR, "18_corr_bubble.png"), width = 10.2, height = 6.5)

# Dendrogram arms as a second view of the same correlation bubbles
message("18b dendrogram arms (ComplexHeatmap)")
cm <- correlate(env)$r
col_fun <- colorRamp2(c(-1, 0, 1), c("#1b7837", "#f7f7f7", "#762a83"))
ht <- Heatmap(
  cm,
  name = "r",
  col = col_fun,
  rect_gp = gpar(type = "none"),
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.rect(x, y, width, height, gp = gpar(col = "#d0d5dd", fill = "white", lwd = 0.4))
    grid.circle(
      x, y,
      r = (abs(cm[i, j]) * 0.42 + 0.08) * min(unit.c(width, height)),
      gp = gpar(fill = col_fun(cm[i, j]), col = "#4b5563", lwd = 0.4)
    )
  },
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  row_dend_width = unit(16, "mm"),
  column_dend_height = unit(16, "mm"),
  row_names_gp = gpar(fontsize = 9),
  column_names_gp = gpar(fontsize = 9),
  heatmap_legend_param = list(title = "r", at = c(-1, 0, 1))
)
ragg::agg_png(
  file.path(FIG_DIR, "18_corr_bubble_dendrogram.png"),
  width = 7.6, height = 6.4, units = "in", res = DPI, background = "white"
)
draw(ht, column_title = "18 Correlation bubbles + hclust dendrogram arms",
     column_title_gp = gpar(fontsize = 13, fontface = "bold"))
dev.off()

# ---------------------------------------------------------------------------
# 19 Triangle bubble heatmap: cell grid + sequential-ish blue/orange bubbles
# ---------------------------------------------------------------------------
message("19 triangle bubble")
vars19 <- colnames(m14)
df19 <- mat_df(m14)
df19$i <- as.integer(df19$x)
df19$j <- nlevels(df19$y) + 1 - as.integer(df19$y)
df19 <- df19[df19$i <= df19$j, ]
p19 <- ggplot(df19, aes(x, y)) +
  geom_tile(fill = "#f7f7f7", color = "#d0d5dd", linewidth = 0.35) +
  geom_point(aes(fill = value, size = abs(value)), shape = 21, color = "#4b5563", stroke = 0.35) +
  scale_fill_gradientn(
    colors = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"),
    limits = c(-1, 1), name = "r"
  ) +
  scale_size(range = c(2.2, 9.5), guide = "none") +
  coord_fixed() +
  labs(title = "19 Triangle bubble heatmap",
       subtitle = "Lower triangle: cell grid + sized bubbles",
       x = NULL, y = NULL, caption = "Demo lower triangle of env correlations") +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))
save_gg(p19, file.path(FIG_DIR, "19_triangle_bubble.png"))

# ---------------------------------------------------------------------------
# 21 Triangle square heatmap: sequential teal/blue squares
# ---------------------------------------------------------------------------
message("21 triangle square")
df21 <- mat_df(m14)
df21$i <- as.integer(df21$x)
df21$j <- nlevels(df21$y) + 1 - as.integer(df21$y)
df21 <- df21[df21$i <= df21$j, ]
p21 <- ggplot(df21, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.9, height = 0.9, linewidth = 0.55) +
  scale_fill_gradientn(
    colors = c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe", "#084081"),
    limits = c(-1, 1), name = "r"
  ) +
  coord_fixed() +
  labs(title = "21 Triangle square heatmap",
       subtitle = "Lower-triangle geom_tile, sequential teal",
       x = NULL, y = NULL, caption = "Demo lower triangle of env correlations") +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))
save_gg(p21, file.path(FIG_DIR, "21_triangle_square.png"))

# ---------------------------------------------------------------------------
# 29 Triangle heatmap: red-blue gapped squares, no crowded coefficients
# ---------------------------------------------------------------------------
message("29 triangle heatmap")
cars_num <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec", "am", "gear")]
m29 <- cor(cars_num)
df29 <- mat_df(m29)
df29$i <- as.integer(df29$x)
df29$j <- nlevels(df29$y) + 1 - as.integer(df29$y)
df29 <- df29[df29$i <= df29$j, ]
p29 <- ggplot(df29, aes(x, y, fill = value)) +
  geom_tile(color = "white", width = 0.9, height = 0.9, linewidth = 0.55) +
  scale_fill_gradientn(
    colors = rev(brewer.pal(11, "RdBu")), limits = c(-1, 1), name = "r"
  ) +
  coord_fixed() +
  labs(title = "29 Triangle heatmap",
       subtitle = "Lower-triangle geom_tile, RdBu, white gaps",
       x = NULL, y = NULL, caption = "Demo: mtcars correlations") +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))
save_gg(p29, file.path(FIG_DIR, "29_triangle_heatmap.png"))

message("done")
