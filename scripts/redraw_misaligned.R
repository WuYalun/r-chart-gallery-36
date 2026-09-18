#!/usr/bin/env Rscript
# Redraw charts whose layout or geometry needed a second pass.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(MASS)
  library(RColorBrewer)
  library(viridis)
  library(ggridges)
  library(ggpattern)
  library(ggalluvial)
  library(cowplot)
  library(scatterplot3d)
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
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}
save_gg <- function(p, path, width = W, height = H) {
  ggsave(path, p, width = width, height = height, dpi = DPI, bg = "white")
}
save_base <- function(path, expr, width = W, height = H) {
  ragg::agg_png(path, width = width, height = height, units = "in", res = DPI, background = "white")
  on.exit(dev.off(), add = TRUE)
  eval.parent(substitute(expr))
}
shade_hex <- function(hex, s) {
  rgb_v <- pmin(pmax(col2rgb(hex) / 255 * s, 0), 1)
  rgb(rgb_v[1, ], rgb_v[2, ], rgb_v[3, ])
}

# 3D box + cubes via persp/trans3d (no plot3D/XQuartz)
persp_empty <- function(xlim, ylim, zlim, theta = 38, phi = 22, expand = 0.65,
                        xlab = "", ylab = "", zlab = "", main = "") {
  dummy <- matrix(zlim[1], nrow = 2, ncol = 2)
  persp(
    x = xlim, y = ylim, z = dummy, zlim = zlim, theta = theta, phi = phi,
    expand = expand, col = "white", border = "white", ticktype = "detailed",
    xlab = xlab, ylab = ylab, zlab = zlab, main = main, box = TRUE
  )
}
draw_cube <- function(pmat, x, y, z0, z1, dx = 0.28, dy = 0.28, col = "#4daf4a") {
  faces <- list(
    list(c(x - dx, x + dx, x + dx, x - dx), c(y + dy, y + dy, y + dy, y + dy), c(z0, z0, z1, z1), 0.62),
    list(c(x - dx, x - dx, x - dx, x - dx), c(y - dy, y + dy, y + dy, y - dy), c(z0, z0, z1, z1), 0.78),
    list(c(x + dx, x + dx, x + dx, x + dx), c(y - dy, y + dy, y + dy, y - dy), c(z0, z0, z1, z1), 0.55),
    list(c(x - dx, x + dx, x + dx, x - dx), c(y - dy, y - dy, y - dy, y - dy), c(z0, z0, z1, z1), 0.88),
    list(c(x - dx, x + dx, x + dx, x - dx), c(y - dy, y - dy, y + dy, y + dy), c(z1, z1, z1, z1), 1.00)
  )
  for (f in faces) {
    pts <- trans3d(f[[1]], f[[2]], f[[3]], pmat)
    polygon(pts, col = shade_hex(col, f[[4]]), border = "grey25", lwd = 0.35)
  }
}

pal_ridge <- colorRampPalette(c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"))(12)
pal_stack <- c("#66c2a5", "#8da0cb", "#fc8d62")
pal_bar3d <- c("#8da0cb", "#66c2a5", "#e78ac3", "#fc8d62", "#a6d854")

# ---------------------------------------------------------------------------
# 01 Ridgeline: many overlapping colorful series
# ---------------------------------------------------------------------------
message("01 ridgeline")
ridge <- bind_rows(lapply(1:12, function(i) {
  data.frame(
    x = density(rnorm(500, mean = 2.2 + i * 0.22, sd = 0.55 + 0.04 * (i %% 3)), n = 200)$x,
    y = density(rnorm(500, mean = 2.2 + i * 0.22, sd = 0.55 + 0.04 * (i %% 3)), n = 200)$y,
    series = sprintf("Series %s", LETTERS[i])
  )
}))
# Use raw samples for geom_density_ridges
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
  labs(title = "01 Ridgeline plot", subtitle = "ggridges::geom_density_ridges · overlapping series",
       x = "Value", y = NULL, caption = "Aligned to source: many stacked colorful ridges") +
  theme_ridges() + theme(legend.position = "none")
save_gg(p, file.path(FIG_DIR, "01_ridgeline.png"))

# ---------------------------------------------------------------------------
# 02 Linear heatmap = thin horizontal stripes (not a 2D field)
# ---------------------------------------------------------------------------
message("02 linear stripe heatmap")
t <- seq(0, 20, length.out = 160)
stripes <- bind_rows(lapply(1:16, function(i) {
  z <- sin(t / (1.8 + i / 8) + i / 3) * exp(-((t - 6 - i * 0.4) / 7)^2) +
    0.35 * sin(t * 0.7 + i) + 0.08 * i
  data.frame(time = t, series = sprintf("Series %02d", i), z = z)
}))
p <- ggplot(stripes, aes(time, series, fill = z)) +
  geom_tile(height = 0.72, width = diff(t)[1] * 1.02) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), name = "z") +
  scale_y_discrete(limits = rev) +
  labs(title = "02 Linear heatmap", subtitle = "ggplot2::geom_tile · one thin stripe per series",
       x = "Time", y = NULL, caption = "Aligned to source: striped series heatmap") +
  theme_ex() + theme(panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "02_linear_heatmap.png"))

# ---------------------------------------------------------------------------
# 04 3D stacked bar with axes (source: Bar3D Plot)
# ---------------------------------------------------------------------------
message("04 3D stacked bar")
layers <- list(
  matrix(c(3, 4, 5, 3, 2, 4, 5, 3, 4, 2, 3, 5), nrow = 4),
  matrix(c(4, 3, 4, 5, 3, 5, 3, 4, 3, 4, 5, 3), nrow = 4),
  matrix(c(2, 3, 2, 2, 4, 2, 3, 2, 2, 3, 2, 2), nrow = 4)
)
cols4 <- c("#8da0cb", "#66c2a5", "#fc8d62")
save_base(file.path(FIG_DIR, "04_3d_stacked_bar.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp_empty(c(0.3, 4.7), c(0.3, 3.7), c(0, 16), theta = 42, phi = 24,
                      xlab = "Group", ylab = "Dose", zlab = "Value",
                      main = "04 3D stacked bar  |  persp + trans3d cubes")
  # back-to-front: large y first
  for (j in 3:1) {
    for (i in 1:4) {
      z0 <- 0
      for (k in 1:3) {
        z1 <- z0 + layers[[k]][i, j]
        draw_cube(pmat, i, j, z0, z1, dx = 0.28, dy = 0.22, col = cols4[k])
        z0 <- z1
      }
    }
  }
  legend("topright", legend = c("Layer A", "Layer B", "Layer C"),
         fill = cols4, bty = "n", cex = 0.85)
})

# ---------------------------------------------------------------------------
# 08 Cluster scatter -> spatial cluster map covering the panel
# ---------------------------------------------------------------------------
message("08 cluster map")
set.seed(8)
centers <- data.frame(
  cx = c(-3.2, -1.2, 0.6, 2.8, -2.4, 0.2, 2.2, -0.8, 3.2, 1.4, -3.5, 2.8),
  cy = c( 2.8,  3.1, 2.5, 2.9,  0.4, 0.6, 0.2, -2.2, -1.8, -3.0, -0.8, 1.4),
  cl = factor(paste0("C", 1:12))
)
grid <- expand.grid(x = seq(-4.2, 4.2, length.out = 180), y = seq(-3.6, 3.6, length.out = 150))
dx <- outer(grid$x, centers$cx, "-")
dy <- outer(grid$y, centers$cy, "-")
nn <- max.col(-(dx^2 + dy^2), ties.method = "first")
grid$cluster <- centers$cl[nn]
# punch a few rectangular "holes" like the source
hole <- with(grid, (x > -1.6 & x < 0.2 & y > 0.8 & y < 2.2) |
               (x > 1.0 & x < 2.4 & y > -0.4 & y < 1.0) |
               (x > -0.4 & x < 1.2 & y > -2.6 & y < -1.4))
grid$cluster[hole] <- NA
p <- ggplot(grid, aes(x, y, fill = cluster)) +
  geom_raster() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(12), na.value = "#d9f0ff", name = "Cluster") +
  coord_fixed(expand = FALSE) +
  labs(title = "08 Cluster scatter / cluster map",
       subtitle = "Nearest-center spatial map (source is a filled cluster field, not sparse blobs)",
       x = "X", y = "Y", caption = "Demo Voronoi-like cluster field") +
  theme_ex() + theme(panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "08_cluster_scatter.png"))

# ---------------------------------------------------------------------------
# 09 Pattern grouped bars: 3 conditions x 4 series, similar heights
# ---------------------------------------------------------------------------
message("09 pattern bars")
d9 <- expand.grid(cond = factor(c("Type1", "Type2", "Type3"), levels = c("Type1", "Type2", "Type3")),
                  series = factor(c("A", "B", "C", "D")))
d9$value <- c(18, 17, 19, 16, 20, 18, 17, 21, 15, 19, 18, 16)
p <- ggplot(d9, aes(cond, value, fill = series, pattern = series)) +
  geom_col_pattern(position = position_dodge(0.88), width = 0.82,
                   color = "grey20", linewidth = 0.3,
                   pattern_fill = "grey20", pattern_color = "grey20",
                   pattern_density = 0.28, pattern_spacing = 0.03) +
  scale_pattern_manual(values = c(A = "stripe", B = "crosshatch", C = "circle", D = "stripe")) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  labs(title = "09 Pattern-filled bar chart", subtitle = "ggpattern::geom_col_pattern · grouped columns",
       x = "Condition", y = "Value", caption = "Aligned to source: clustered hatched bars") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "09_pattern_bar.png"))

# ---------------------------------------------------------------------------
# 10 3D cluster scatter: dense colored landscape of points
# ---------------------------------------------------------------------------
message("10 3D cluster landscape")
set.seed(10)
map <- expand.grid(x = seq(-2, 2, length.out = 70), y = seq(-2, 2, length.out = 70))
map$z <- 1.2 * exp(-((map$x + 0.3)^2 + (map$y - 0.2)^2) / 1.4) +
  0.7 * exp(-((map$x - 1)^2 + (map$y + 0.8)^2) / 0.9)
map$cl <- cut(atan2(map$y, map$x) + 0.4 * map$z, breaks = 8, labels = paste0("C", 1:8))
samp <- map[sample.int(nrow(map), 1800), ]
cols10 <- brewer.pal(8, "Set2")[as.integer(samp$cl)]
save_base(file.path(FIG_DIR, "10_3d_cluster_scatter.png"), {
  par(mar = c(2.5, 2.5, 3, 1))
  scatterplot3d(
    samp$x, samp$y, samp$z, color = cols10, pch = 16, cex.symbols = 0.45,
    xlab = "X", ylab = "Y", zlab = "Z", angle = 55, grid = TRUE,
    main = "10 3D cluster scatter  |  dense colored point landscape"
  )
  legend("topright", legend = levels(samp$cl), pch = 16, col = brewer.pal(8, "Set2"),
         bty = "n", cex = 0.7, ncol = 2)
})

# ---------------------------------------------------------------------------
# 12 Inset zoom in the SAME panel, not facet_zoom split
# ---------------------------------------------------------------------------
message("12 inset")
set.seed(12)
d12 <- data.frame(x = cars$speed + rnorm(nrow(cars), 0, 0.2), y = cars$dist)
fit <- lm(y ~ x, d12)
p_main <- ggplot(d12, aes(x, y)) +
  geom_point(color = "#2b6cb0", size = 2.2) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "#c53030", linewidth = 0.7) +
  annotate("rect", xmin = 12, xmax = 18, ymin = 18, ymax = 48,
           color = "#c53030", fill = NA, linewidth = 0.6) +
  labs(title = "12 Inset / local zoom", subtitle = "cowplot inset of the boxed region",
       x = "Speed", y = "Stopping distance") +
  theme_ex()
p_zoom <- ggplot(d12, aes(x, y)) +
  geom_point(color = "#2b6cb0", size = 2.4) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "#c53030", linewidth = 0.7) +
  coord_cartesian(xlim = c(12, 18), ylim = c(18, 48), expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Zoom") +
  theme_ex(base_size = 9) +
  theme(plot.title = element_text(size = 10), panel.border = element_rect(color = "#c53030", fill = NA, linewidth = 0.8),
        plot.background = element_rect(fill = "white", color = "grey70"))
p <- ggdraw(p_main) + draw_plot(p_zoom, x = 0.52, y = 0.12, width = 0.42, height = 0.42)
save_gg(p, file.path(FIG_DIR, "12_inset_zoom.png"))

# ---------------------------------------------------------------------------
# 13 Pattern stacked bars
# ---------------------------------------------------------------------------
message("13 pattern stacked")
d13 <- expand.grid(month = factor(month.abb[1:8], levels = month.abb[1:8]),
                   part = factor(c("Bottom", "Mid", "Top"), levels = c("Bottom", "Mid", "Top")))
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

# ---------------------------------------------------------------------------
# 15 Waffle: fewer larger squares, mixed colors
# ---------------------------------------------------------------------------
message("15 waffle")
set.seed(15)
counts <- c(A = 10, B = 8, C = 7, D = 5, E = 4, F = 2)
tiles <- sample(unlist(mapply(rep, names(counts), counts)))
waffle <- data.frame(x = rep(1:6, each = 6), y = rep(6:1, 6),
                     cat = factor(tiles, levels = names(counts)))
p <- ggplot(waffle, aes(x, y, fill = cat)) +
  geom_tile(color = "white", linewidth = 1.2, width = 0.92, height = 0.92) +
  scale_fill_manual(values = c("#8da0cb", "#66c2a5", "#fc8d62", "#e78ac3", "#a6d854", "#ffd92f"), name = "Group") +
  coord_equal() +
  labs(title = "15 Waffle chart", subtitle = "6x6 square pie with mixed cells",
       x = NULL, y = NULL) +
  theme_ex() + theme(axis.text = element_blank(), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "15_waffle.png"))

# ---------------------------------------------------------------------------
# 16 3D filled line: separate area walls, not a connected surface
# ---------------------------------------------------------------------------
message("16 3D filled area walls")
x <- seq(0, 10, length.out = 80)
series <- list(
  pmax(0.2, 2.2 * dnorm(x, 3.2, 1.1) * 6 + 0.15 * sin(x)),
  pmax(0.2, 2.0 * dnorm(x, 5.0, 1.3) * 6 + 0.2),
  pmax(0.2, 1.8 * dnorm(x, 6.2, 1.0) * 6),
  pmax(0.2, 1.6 * dnorm(x, 4.0, 1.6) * 5 + 0.3)
)
cols16 <- c("#fee08b", "#66c2a5", "#8da0cb", "#e78ac3")
save_base(file.path(FIG_DIR, "16_3d_filled_line.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp_empty(c(0, 10), c(0.4, 4.6), c(0, 3.2), theta = 38, phi = 22,
                      xlab = "Time", ylab = "Series", zlab = "Value",
                      main = "16 3D filled line  |  area walls via trans3d")
  for (s in 4:1) {
    xs <- c(x, rev(x))
    ys <- c(rep(s, length(x)), rev(rep(s, length(x))))
    zs <- c(series[[s]], rep(0, length(x)))
    pts <- trans3d(xs, ys, zs, pmat)
    polygon(pts, col = adjustcolor(cols16[s], 0.92), border = "grey25", lwd = 0.4)
    line <- trans3d(x, rep(s, length(x)), series[[s]], pmat)
    lines(line, col = "grey20", lwd = 1)
  }
})

# ---------------------------------------------------------------------------
# 20 3D bars with labels on top
# ---------------------------------------------------------------------------
message("20 3D bars with labels")
mat20 <- matrix(c(9, 14, 11, 16, 12, 8, 13, 18, 10, 7, 15, 12), nrow = 4, byrow = TRUE)
rownames(mat20) <- c("Q1", "Q2", "Q3", "Q4")
colnames(mat20) <- c("A", "B", "C")
col20 <- colorRampPalette(c("#8da0cb", "#66c2a5", "#fee08b", "#fc8d62"))(12)
save_base(file.path(FIG_DIR, "20_3d_bar_labels.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp_empty(c(0.3, 4.7), c(0.3, 3.7), c(0, 22), theta = -38, phi = 22,
                      xlab = "Quarter", ylab = "Region", zlab = "Sales",
                      main = "20 3D bar with category labels")
  k <- 1
  for (j in 3:1) for (i in 1:4) {
    draw_cube(pmat, i, j, 0, mat20[i, j], dx = 0.28, dy = 0.22, col = col20[k])
    lab <- trans3d(i, j, mat20[i, j] + 0.8, pmat)
    text(lab$x, lab$y, labels = mat20[i, j], cex = 0.7)
    k <- k + 1
  }
})

# ---------------------------------------------------------------------------
# 22 Unequal-width bars on a baseline (not a 0-1 Mekko)
# ---------------------------------------------------------------------------
message("22 unequal width")
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
       subtitle = "ggplot2::geom_rect · compact / variable bar widths",
       x = "Category", y = "Value", caption = "Aligned to source: a few bars of different widths") +
  theme_ex() + theme(legend.position = "none")
save_gg(p, file.path(FIG_DIR, "22_unequal_width_bar.png"))

# ---------------------------------------------------------------------------
# 23 Density scatter: cigar cloud + viridis + regression
# ---------------------------------------------------------------------------
message("23 density scatter")
n <- 900
x <- rnorm(n, 50, 10)
y <- 0.85 * x + rnorm(n, 8, 4.5)
d23 <- data.frame(x, y, dens = {
  dens <- kde2d(x, y, n = 80)
  dens$z[cbind(findInterval(x, dens$x), findInterval(y, dens$y))]
})
p <- ggplot(d23, aes(x, y, color = dens)) +
  geom_point(size = 1.5, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = "grey15", linewidth = 0.7) +
  scale_color_viridis_c(option = "viridis", name = "density") +
  labs(title = "23 Density scatter", subtitle = "Points colored by kde2d + regression line",
       x = "Observed", y = "Estimated depth") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "23_density_scatter.png"))

# ---------------------------------------------------------------------------
# 24 Floating bars in an arch
# ---------------------------------------------------------------------------
message("24 floating arch")
x <- 1:14
mid <- 9 + 7 * dnorm(x, 7.5, 2.6) / dnorm(7.5, 7.5, 2.6)
half <- 3.2 + 0.35 * sin(x / 2)
d24 <- data.frame(
  x = factor(x),
  xmin = x - 0.32, xmax = x + 0.32,
  ymin = mid - half, ymax = mid + half
)
p <- ggplot(d24) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#2c7c7c", color = "grey20", linewidth = 0.25) +
  scale_x_continuous(breaks = x, labels = x) +
  labs(title = "24 Floating bar chart",
       subtitle = "geom_rect range bars forming an arch (not rooted at 0)",
       x = "Sample", y = "Value") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "24_floating_bar.png"))

# ---------------------------------------------------------------------------
# 25 3D density: kernel density SURFACE, not a point cloud
# ---------------------------------------------------------------------------
message("25 3D density surface")
set.seed(25)
xx <- c(rnorm(400, 0, 0.7), rnorm(120, 1.2, 0.5))
yy <- c(rnorm(400, 0, 0.7), rnorm(120, 0.4, 0.5))
kd <- kde2d(xx, yy, n = 45, lims = c(-2.5, 2.5, -2.5, 2.5))
nr <- length(kd$x); nc <- length(kd$y)
facet_col <- viridis(64, option = "plasma")
z <- kd$z
zcol <- facet_col[cut(z[-nr, -nc], 64, labels = FALSE)]
save_base(file.path(FIG_DIR, "25_3d_density_scatter.png"), {
  par(mar = c(1.2, 1, 3, 4))
  persp(
    kd$x, kd$y, kd$z, theta = 40, phi = 28, expand = 0.6,
    col = zcol, border = NA, ticktype = "detailed",
    xlab = "X", ylab = "Y", zlab = "Density",
    main = "25 3D density  |  persp of MASS::kde2d"
  )
})

# ---------------------------------------------------------------------------
# 26 Bidirectional stacked DIAMOND of bar segments
# ---------------------------------------------------------------------------
message("26 diamond stacked")
ny <- 18
cols26 <- c("#2166ac", "#67a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2182b")
nk <- length(cols26)
rows <- lapply(1:ny, function(i) {
  w <- dnorm(i, mean = 9.5, sd = 3.4)
  segs <- (c(0.12, 0.18, 0.2, 0.2, 0.18, 0.12) + runif(nk, -0.02, 0.02))
  segs <- pmax(segs, 0.04); segs <- segs / sum(segs)
  widths <- segs * w * 18
  xmin <- -sum(widths) / 2
  bind_rows(lapply(1:nk, function(k) {
    xmax <- xmin + widths[k]
    out <- data.frame(y = i, xmin = xmin, xmax = xmax, fill = paste0("S", k))
    xmin <<- xmax
    out
  }))
})
d26 <- bind_rows(rows)
p <- ggplot(d26, aes(ymin = y - 0.48, ymax = y + 0.48, xmin = xmin, xmax = xmax, fill = fill)) +
  geom_rect(color = "white", linewidth = 0.15) +
  scale_fill_manual(values = cols26, name = "Class") +
  labs(title = "26 Bidirectional stacked chart",
       subtitle = "Stacked horizontal segments forming a diamond / butterfly",
       x = "Stacked share", y = "Bin") +
  theme_ex() + theme(axis.text.y = element_blank())
save_gg(p, file.path(FIG_DIR, "26_bidirectional_stacked.png"))

# ---------------------------------------------------------------------------
# 27 Horizontal bidirectional: violin of many thin stacked layers
# ---------------------------------------------------------------------------
message("27 violin stacked")
ny <- 28
cols27 <- colorRampPalette(c("#d73027", "#fdae61", "#ffffbf", "#abd9e9", "#4575b4"))(10)
nk <- length(cols27)
rows <- lapply(1:ny, function(i) {
  w <- dnorm(i, mean = 14.5, sd = 5.2)
  segs <- (dnorm(seq(-2, 2, length.out = nk)) + 0.08)
  segs <- segs / sum(segs)
  widths <- segs * w * 22
  xmin <- -sum(widths) / 2
  bind_rows(lapply(1:nk, function(k) {
    xmax <- xmin + widths[k]
    out <- data.frame(y = i, xmin = xmin, xmax = xmax, fill = paste0("L", k))
    xmin <<- xmax
    out
  }))
})
d27 <- bind_rows(rows)
p <- ggplot(d27, aes(ymin = y - 0.5, ymax = y + 0.5, xmin = xmin, xmax = xmax, fill = fill)) +
  geom_rect(color = NA) +
  scale_fill_manual(values = cols27, guide = "none") +
  labs(title = "27 Horizontal bidirectional stacked",
       subtitle = "Many thin stacked layers forming a violin / pyramid",
       x = NULL, y = NULL) +
  theme_ex() + theme(axis.text = element_blank(), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "27_horizontal_bidirectional.png"))

# ---------------------------------------------------------------------------
# 28 Source image is 2D scatter + regression + color (bathymetry), not a 3D plane
# ---------------------------------------------------------------------------
message("28 2D colored scatter + regression")
n <- 800
x <- rnorm(n, 50, 9)
y <- 0.9 * x + rnorm(n, 5, 3.8)
z <- y + rnorm(n, 0, 2)
d28 <- data.frame(x, y, z)
p <- ggplot(d28, aes(x, y, color = z)) +
  geom_point(size = 1.6, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = "grey10", linewidth = 0.75) +
  scale_color_viridis_c(option = "viridis", name = "depth") +
  labs(title = "28 Colored scatter + regression",
       subtitle = "Source thumbnail is 2D bathymetry scatter (not a 3D plane)",
       x = "Observed", y = "Estimated depth") +
  theme_ex()
save_gg(p, file.path(FIG_DIR, "28_3d_cluster_regression.png"))

# ---------------------------------------------------------------------------
# 30 冲击图 = filled stacked BAR/AREA from baseline, not centered stream
# ---------------------------------------------------------------------------
message("30 stacked filled")
d30 <- expand.grid(sample = factor(paste0("S", 1:10), levels = paste0("S", 1:10)),
                   layer = factor(c("L1", "L2", "L3", "L4"), levels = c("L1", "L2", "L3", "L4")))
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

# ---------------------------------------------------------------------------
# 31 Sankey with crossing X flows
# ---------------------------------------------------------------------------
message("31 crossing alluvial")
d31 <- data.frame(
  freq = c(28, 10, 18, 14, 22, 8, 16, 12, 20, 9, 15, 11),
  source = c("A1","A1","A1","A2","A2","A2","A3","A3","A3","A4","A4","A4"),
  dest  = c("B3","B2","B1","B1","B3","B4","B4","B1","B2","B2","B4","B3")
)
p <- ggplot(d31, aes(y = freq, axis1 = source, axis2 = dest)) +
  geom_alluvium(aes(fill = source), width = 1/8, alpha = 0.85, knot.pos = 0.35) +
  geom_stratum(width = 1/6, fill = "grey95", color = "grey30") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.3) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"), guide = "none") +
  scale_x_discrete(limits = c("From", "To"), expand = c(0.15, 0.15)) +
  labs(title = "31 Sankey / alluvial", subtitle = "ggalluvial with crossing flows",
       y = NULL) +
  theme_ex() + theme(axis.text.y = element_blank(), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "31_sankey.png"))

# ---------------------------------------------------------------------------
# 32 Several violins in a row
# ---------------------------------------------------------------------------
message("32 violins")
set.seed(32)
d32 <- bind_rows(lapply(1:6, function(i) {
  data.frame(grp = paste0("G", i), y = rnorm(120, mean = 5 + (i %% 3) * 0.6, sd = 0.7 + 0.15 * (i %% 2)))
}))
p <- ggplot(d32, aes(grp, y, fill = grp)) +
  geom_violin(trim = FALSE, color = "grey20", linewidth = 0.3, alpha = 0.9) +
  geom_boxplot(width = 0.1, outlier.size = 0.5, fill = "white") +
  scale_fill_manual(values = pal_ridge[c(1, 3, 5, 7, 9, 11)]) +
  labs(title = "32 Violin plot", subtitle = "ggplot2::geom_violin across 6 groups",
       x = NULL, y = "Value") +
  theme_ex() + theme(legend.position = "none")
save_gg(p, file.path(FIG_DIR, "32_violin.png"))

# ---------------------------------------------------------------------------
# 33 Dual surface: two offset surfaces, cyan + orange
# ---------------------------------------------------------------------------
message("33 dual surface")
xs <- seq(-2, 2, length.out = 28)
ys <- seq(-2, 2, length.out = 28)
z1 <- outer(xs, ys, function(a, b) 1.4 * exp(-((a + 0.6)^2 + (b + 0.4)^2)))
z2 <- outer(xs, ys, function(a, b) 1.1 * exp(-((a - 0.7)^2 + (b - 0.5)^2)))
nr <- length(xs)
col1 <- colorRampPalette(c("#c7e9c0", "#41b6c4", "#225ea8"))(64)
col2 <- colorRampPalette(c("#fee08b", "#fdae61", "#f46d43"))(64)
save_base(file.path(FIG_DIR, "33_dual_surface.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp(
    xs, ys, z1, theta = 38, phi = 28, expand = 0.7, zlim = c(0, 1.6),
    col = col1[cut(z1[-nr, -nr], 64, labels = FALSE)], border = NA,
    ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z",
    main = "33 Dual surface  |  two offset peaks"
  )
  # second surface facets
  for (i in 1:(nr - 1)) for (j in 1:(nr - 1)) {
    xx <- c(xs[i], xs[i + 1], xs[i + 1], xs[i])
    yy <- c(ys[j], ys[j], ys[j + 1], ys[j + 1])
    zz <- c(z2[i, j], z2[i + 1, j], z2[i + 1, j + 1], z2[i, j + 1])
    pts <- trans3d(xx, yy, zz, pmat)
    ci <- cut(mean(zz), breaks = seq(min(z2), max(z2) + 1e-8, length.out = 65), labels = FALSE)
    polygon(pts, col = adjustcolor(col2[ci], 0.7), border = NA)
  }
})

# ---------------------------------------------------------------------------
# 34 Dual triangle: sequential teal on BOTH triangles, white diagonal
# ---------------------------------------------------------------------------
message("34 dual triangle teal")
vars <- c("Cyl", "Disp", "Hp", "Drat", "Wt", "Qsec", "Vs", "Am")
set.seed(34)
m1 <- cor(matrix(rnorm(80 * 8), ncol = 8))
m2 <- cor(matrix(rnorm(80 * 8), ncol = 8))
dimnames(m1) <- dimnames(m2) <- list(vars, vars)
df <- expand.grid(Var1 = factor(vars, levels = vars), Var2 = factor(vars, levels = rev(vars)))
df$i <- as.integer(df$Var1)
df$j <- match(as.character(df$Var2), vars)
df$v1 <- m1[cbind(as.character(df$Var1), as.character(df$Var2))]
df$v2 <- m2[cbind(as.character(df$Var1), as.character(df$Var2))]
df$diag <- df$i == df$j
df$upper <- df$i < df$j
p <- ggplot(df, aes(Var1, Var2)) +
  geom_tile(data = subset(df, upper), aes(fill = v1), color = "white", linewidth = 0.6) +
  geom_tile(data = subset(df, !upper & !diag), aes(fill = v2), color = "white", linewidth = 0.6) +
  geom_tile(data = subset(df, diag), fill = "white", color = "grey80") +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), name = "value") +
  coord_fixed() +
  labs(title = "34 Dual-triangle heatmap",
       subtitle = "Two triangles, one sequential teal scale, white diagonal",
       x = NULL, y = NULL) +
  theme_ex() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "34_dual_triangle_heatmap.png"))

# ---------------------------------------------------------------------------
# 35 Tree-like dual-feature 3D scatter
# ---------------------------------------------------------------------------
message("35 tree scatter")
set.seed(35)
theta <- runif(90, 0, 2 * pi)
trunk <- data.frame(
  x = 0.06 * rnorm(90), y = 0.06 * rnorm(90), z = runif(90, 0, 2.0),
  feat = runif(90, 0.05, 0.25)
)
can <- data.frame(
  x = 0.7 * rnorm(500) * pmax(0.2, 1 - abs(rnorm(500, 0, 0.5))),
  y = 0.7 * rnorm(500) * pmax(0.2, 1 - abs(rnorm(500, 0, 0.5))),
  z = rnorm(500, 3.3, 0.55),
  feat = rnorm(500, 0.75, 0.15)
)
d35 <- rbind(trunk, can)
d35$feat <- pmin(pmax(d35$feat, 0), 1)
ramp <- viridis(100, option = "D")
cols <- ramp[pmax(1, cut(d35$feat, 100, labels = FALSE))]
save_base(file.path(FIG_DIR, "35_dual_feature_3d_scatter.png"), {
  par(mar = c(2.5, 2.5, 3, 1))
  scatterplot3d(
    d35$x, d35$y, d35$z, color = cols, pch = 16,
    cex.symbols = 0.35 + 0.7 * d35$feat,
    xlab = "X", ylab = "Y", zlab = "Z", angle = 52, grid = TRUE,
    main = "35 Dual-feature 3D scatter  |  color + size (tree)"
  )
})

# ---------------------------------------------------------------------------
# 36 Dual GRID surface
# ---------------------------------------------------------------------------
message("36 dual grid")
xs <- seq(-pi, pi, length.out = 24)
ys <- seq(-pi, pi, length.out = 24)
z1 <- outer(xs, ys, function(a, b) 0.7 * sin(a) * cos(b))
z2 <- outer(xs, ys, function(a, b) 0.45 * cos(0.8 * a) * sin(0.8 * b) + 0.9)
save_base(file.path(FIG_DIR, "36_dual_grid_surface.png"), {
  par(mar = c(1.2, 1, 3, 1))
  pmat <- persp(
    xs, ys, z1, theta = 35, phi = 30, expand = 0.65, zlim = c(-1, 1.5),
    col = adjustcolor("#41b6c4", 0.55), border = "grey25", lwd = 0.3,
    ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z",
    main = "36 Dual grid surface  |  two meshed surfaces"
  )
  nr <- length(xs)
  for (i in 1:(nr - 1)) for (j in 1:(nr - 1)) {
    xx <- c(xs[i], xs[i + 1], xs[i + 1], xs[i])
    yy <- c(ys[j], ys[j], ys[j + 1], ys[j + 1])
    zz <- c(z2[i, j], z2[i + 1, j], z2[i + 1, j + 1], z2[i, j + 1])
    pts <- trans3d(xx, yy, zz, pmat)
    polygon(pts, col = adjustcolor("#fee08b", 0.45), border = "grey30", lwd = 0.25)
  }
})

message("redraw done")
