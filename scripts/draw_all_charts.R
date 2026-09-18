#!/usr/bin/env Rscript
# Draw one example for each of the 36 chart types identified from the source gallery.
# Reproducible demo data only; no real experimental measurements.

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
  library(ggforce)
  library(ggalluvial)
  library(ggnewscale)
  library(ggrepel)
  library(cowplot)
  library(corrplot)
  library(ggcorrplot)
  library(fmsb)
  library(scatterplot3d)
  library(hexbin)
})

set.seed(20260918)

OUT_DIR <- "/Users/aaron/Desktop/report/output/r-chart-gallery-20260918"
FIG_DIR <- file.path(OUT_DIR, "figures")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

W <- 7.2
H <- 5.6
DPI <- 160

pal_set2 <- brewer.pal(8, "Set2")
pal_spectral <- rev(brewer.pal(11, "Spectral"))
pal_cluster <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
heat_cols <- colorRampPalette(pal_spectral)(100)

theme_ex <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#1f2933"),
      plot.subtitle = element_text(size = 9, color = "#6b7280"),
      plot.caption = element_text(size = 8, color = "#9aa0a6"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
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

density_at <- function(x, y, n = 80) {
  dens <- kde2d(x, y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  dens$z[cbind(pmin(pmax(ix, 1), n), pmin(pmax(iy, 1), n))]
}

shade_hex <- function(hex, s) {
  rgb_v <- pmin(pmax(col2rgb(hex) / 255 * s, 0), 1)
  rgb(rgb_v[1, ], rgb_v[2, ], rgb_v[3, ])
}

iso_project <- function(x, y, z) {
  list(px = x + 0.58 * y, py = z + 0.32 * y)
}

bar_faces <- function(x, y, z0, z1, fill, key, w = 0.32, d = 0.32) {
  face <- function(xs, ys, zs, face_name, shade) {
    p <- iso_project(xs, ys, zs)
    data.frame(
      px = p$px, py = p$py,
      fill_draw = shade_hex(fill, shade),
      face = face_name, key = key,
      ysort = y, xsort = x, zsort = z0
    )
  }
  rbind(
    face(c(x - w, x + w, x + w, x - w), rep(y - d, 4), c(z0, z0, z1, z1), "front", 0.88),
    face(c(x + w, x + w, x + w, x + w), c(y - d, y + d, y + d, y - d), c(z0, z0, z1, z1), "side", 0.68),
    face(c(x - w, x + w, x + w, x - w), c(y - d, y - d, y + d, y + d), rep(z1, 4), "top", 1.00)
  )
}

gg_isometric_bars <- function(bars, title, subtitle, caption) {
  faces <- bind_rows(bars)
  faces$grp <- paste(faces$key, faces$face, sep = "_")
  faces <- faces[order(-faces$ysort, faces$xsort, faces$zsort), ]
  ggplot(faces, aes(px, py, group = grp)) +
    geom_polygon(aes(fill = fill_draw), color = "grey25", linewidth = 0.25) +
    scale_fill_identity() +
    coord_equal() +
    labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    theme_ex() +
    theme(axis.text = element_blank(), panel.grid = element_blank())
}

add_surface <- function(pmat, x, y, z, col, border = "grey30", alpha = 0.4, lwd = 0.3) {
  nx <- length(x)
  ny <- length(y)
  for (i in 1:(nx - 1)) {
    for (j in 1:(ny - 1)) {
      xx <- c(x[i], x[i + 1], x[i + 1], x[i])
      yy <- c(y[j], y[j], y[j + 1], y[j + 1])
      zz <- c(z[i, j], z[i + 1, j], z[i + 1, j + 1], z[i, j + 1])
      pts <- trans3d(xx, yy, zz, pmat)
      polygon(pts, col = adjustcolor(col, alpha), border = border, lwd = lwd)
    }
  }
}

make_cor_df <- function(mat) {
  if (is.null(colnames(mat))) colnames(mat) <- paste0("V", seq_len(ncol(mat)))
  if (is.null(rownames(mat))) rownames(mat) <- colnames(mat)
  vars <- colnames(mat)
  as.data.frame(as.table(mat)) |>
    mutate(
      Var1 = factor(Var1, levels = vars),
      Var2 = factor(Var2, levels = rev(vars)),
      i = as.integer(Var1),
      j = as.integer(factor(Var2, levels = vars))
    )
}

cars_num <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec", "am", "gear")]
cars_cor <- cor(cars_num)

cluster2d <- bind_rows(
  lapply(seq_along(pal_cluster), function(k) {
    ang <- (k - 1) * pi / 3
    data.frame(
      x = rnorm(80, 3 * cos(ang), 0.45),
      y = rnorm(80, 3 * sin(ang), 0.45),
      cluster = paste0("C", k)
    )
  })
)

cluster3d <- bind_rows(
  lapply(1:4, function(k) {
    data.frame(
      x = rnorm(70, c(-1.5, 1.5, -1.2, 1.4)[k], 0.35),
      y = rnorm(70, c(-1.2, -1.4, 1.5, 1.2)[k], 0.35),
      z = rnorm(70, c(0.4, 1.6, 0.8, 2.0)[k], 0.35),
      cluster = paste0("C", k)
    )
  })
)

field <- expand.grid(x = seq(0, 10, length.out = 80), y = seq(0, 8, length.out = 64))
field$z <- with(field, sin(x / 1.6) * cos(y / 1.8) + 0.35 * sin(x * y / 12) + 0.15 * x / 10)

sq <- expand.grid(x = LETTERS[1:8], y = paste0("G", 1:8))
sq$value <- as.vector(outer(1:8, 1:8, function(a, b) sin(a / 2) + cos(b / 2.4) + 0.2 * a))

results <- list()
record <- function(id, zh, en, pkg, fun, path) {
  results[[length(results) + 1]] <<- data.frame(
    id = id, chinese = zh, english = en,
    r_package = pkg, r_function = fun, file = basename(path),
    stringsAsFactors = FALSE
  )
}

safe_draw <- function(id, fun) {
  message(sprintf("[%02d] drawing...", id))
  tryCatch(fun(), error = function(e) {
    message(sprintf("[%02d] FAILED: %s", id, e$message))
    results[[length(results) + 1]] <<- data.frame(
      id = id, chinese = NA, english = NA,
      r_package = NA, r_function = paste("ERROR:", e$message),
      file = NA, stringsAsFactors = FALSE
    )
  })
}

# ---------------------------------------------------------------------------
# 01 Ridgeline
# ---------------------------------------------------------------------------
safe_draw(1, function() {
  path <- file.path(FIG_DIR, "01_ridgeline.png")
  d <- iris
  p <- ggplot(d, aes(x = Sepal.Length, y = Species, fill = Species, color = Species)) +
    geom_density_ridges(scale = 1.15, alpha = 0.85, rel_min_height = 0.01, linewidth = 0.3) +
    scale_fill_manual(values = pal_set2[1:3]) +
    scale_color_manual(values = pal_set2[1:3]) +
    labs(title = "01 Ridgeline plot", subtitle = "ggridges::geom_density_ridges · iris Sepal.Length",
         x = "Sepal length (cm)", y = "Species", caption = "Demo data: iris") +
    theme_ridges() + theme(legend.position = "none")
  save_gg(p, path)
  record(1, "山脊图", "Ridgeline plot", "ggridges", "geom_density_ridges()", path)
})

# ---------------------------------------------------------------------------
# 02 Linear heatmap (continuous raster)
# ---------------------------------------------------------------------------
safe_draw(2, function() {
  path <- file.path(FIG_DIR, "02_linear_heatmap.png")
  p <- ggplot(field, aes(x, y, fill = z)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradientn(colors = heat_cols, name = "z") +
    coord_fixed(expand = FALSE) +
    labs(title = "02 Linear heatmap", subtitle = "ggplot2::geom_raster(interpolate=TRUE)",
         x = "X", y = "Y", caption = "Demo field: sin/cos synthetic surface") +
    theme_ex()
  save_gg(p, path)
  record(2, "线型热图", "Linear heatmap plot", "ggplot2", "geom_raster()", path)
})

# ---------------------------------------------------------------------------
# 03 Square heatmap
# ---------------------------------------------------------------------------
safe_draw(3, function() {
  path <- file.path(FIG_DIR, "03_square_heatmap.png")
  p <- ggplot(sq, aes(x, y, fill = value)) +
    geom_tile(color = "white", width = 0.92, height = 0.92, linewidth = 0.6) +
    scale_fill_gradientn(colors = heat_cols, name = "value") +
    coord_fixed() +
    labs(title = "03 Square heatmap", subtitle = "ggplot2::geom_tile with cell gaps",
         x = NULL, y = NULL, caption = "Demo matrix 8 x 8") +
    theme_ex() + theme(panel.grid = element_blank())
  save_gg(p, path)
  record(3, "方块热图", "Square heatmap plot", "ggplot2", "geom_tile()", path)
})

# ---------------------------------------------------------------------------
# 04 3D stacked bar
# ---------------------------------------------------------------------------
safe_draw(4, function() {
  path <- file.path(FIG_DIR, "04_3d_stacked_bar.png")
  layers <- list(
    matrix(c(4, 3, 5, 2, 6, 4, 3, 5, 2), 3, 3),
    matrix(c(3, 4, 2, 5, 3, 4, 4, 2, 5), 3, 3),
    matrix(c(2, 2, 3, 3, 2, 3, 2, 4, 3), 3, 3)
  )
  cols <- pal_set2[1:3]
  bars <- list()
  k_id <- 1
  for (i in 1:3) for (j in 1:3) {
    z0 <- 0
    for (k in 1:3) {
      z1 <- z0 + layers[[k]][i, j]
      bars[[k_id]] <- bar_faces(i, j, z0, z1, cols[k], paste(i, j, k, sep = "-"))
      k_id <- k_id + 1
      z0 <- z1
    }
  }
  p <- gg_isometric_bars(
    bars,
    "04 3D stacked bar",
    "Isometric ggplot2 polygons (plot3D needs XQuartz on this machine)",
    "Demo 3 groups x 3 doses x 3 stacked layers"
  )
  save_gg(p, path)
  record(4, "三维堆叠柱状图", "3D stacked bar chart", "ggplot2", "geom_polygon() isometric cubes", path)
})

# ---------------------------------------------------------------------------
# 05 Wind rose
# ---------------------------------------------------------------------------
safe_draw(5, function() {
  path <- file.path(FIG_DIR, "05_wind_rose.png")
  dirs <- seq(0, 337.5, by = 22.5)
  dir_lab <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
               "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  ws_br <- c("0-2", "2-4", "4-6", "6-8", ">8")
  wind <- expand.grid(dir = factor(dir_lab, levels = dir_lab), ws = factor(ws_br, levels = ws_br))
  # Prefer SW/W winds
  wgt <- (cos((as.numeric(wind$dir) - 11) / 16 * 2 * pi) + 1.2)^1.6
  wind$freq <- pmax(rpois(nrow(wind), lambda = 4 * wgt * as.numeric(wind$ws) / 3), 0)
  p <- ggplot(wind, aes(x = dir, y = freq, fill = ws)) +
    geom_col(width = 1, color = "white", linewidth = 0.2) +
    coord_polar(start = -pi / 16) +
    scale_fill_brewer(palette = "YlGnBu", name = "Wind speed\n(m/s)") +
    labs(title = "05 Wind rose", subtitle = "ggplot2::geom_col + coord_polar",
         x = NULL, y = "Frequency", caption = "Demo wind direction/speed bins") +
    theme_ex() +
    theme(axis.text.y = element_blank(), panel.grid.major.x = element_line(color = "grey80"))
  save_gg(p, path)
  record(5, "风玫瑰图", "Wind rose", "ggplot2", "geom_col() + coord_polar()", path)
})

# ---------------------------------------------------------------------------
# 06 Radar
# ---------------------------------------------------------------------------
safe_draw(6, function() {
  path <- file.path(FIG_DIR, "06_radar.png")
  radar <- as.data.frame(rbind(
    max = rep(10, 6),
    min = rep(0, 6),
    Ivy = c(8.5, 6.2, 9.0, 5.5, 7.8, 6.0),
    UI = c(6.0, 8.8, 5.4, 9.1, 6.5, 8.0),
    Lab = c(7.2, 5.0, 7.8, 6.4, 8.9, 5.6)
  ))
  colnames(radar) <- c("Speed", "Quality", "Coverage", "UX", "Stability", "Cost")
  save_base(path, {
    par(mar = c(2, 2, 3, 2))
    radarchart(
      radar, axistype = 1, seg = 5, pcol = pal_cluster[1:3],
      pfcol = adjustcolor(pal_cluster[1:3], 0.35), plwd = 2, plty = 1,
      cglcol = "grey70", cglty = 1, axislabcol = "grey30",
      caxislabels = seq(0, 10, 2), vlcex = 0.9,
      title = "06 Radar chart  |  fmsb::radarchart"
    )
    legend("topright", legend = rownames(radar)[-(1:2)], col = pal_cluster[1:3],
           lty = 1, lwd = 2, bty = "n")
  })
  record(6, "雷达图", "Radar / spider chart", "fmsb", "radarchart()", path)
})

# ---------------------------------------------------------------------------
# 07 Linear heatmap of series (spectrogram-like)
# ---------------------------------------------------------------------------
safe_draw(7, function() {
  path <- file.path(FIG_DIR, "07_linear_heatmap_series.png")
  t <- 1:120
  series <- lapply(1:18, function(i) {
    sin(t / (6 + i / 4) + i / 3) * exp(-((t - 40 - 3 * i) / 28)^2) +
      0.25 * sin(t / 3 + i)
  })
  mat <- do.call(rbind, series)
  df <- as.data.frame(as.table(mat))
  names(df) <- c("series", "time", "z")
  df$series <- as.integer(df$series)
  df$time <- as.integer(df$time)
  p <- ggplot(df, aes(time, series, fill = z)) +
    geom_tile() +
    scale_fill_gradientn(colors = heat_cols, name = "intensity") +
    scale_y_reverse() +
    labs(title = "07 Linear heatmap (series)", subtitle = "ggplot2::geom_tile of 18 synthetic series",
         x = "Time", y = "Series", caption = "Demo spectrogram-like matrix") +
    theme_ex() + theme(panel.grid = element_blank())
  save_gg(p, path)
  record(7, "线型热图（序列）", "Linear heatmap of series", "ggplot2", "geom_tile()", path)
})

# ---------------------------------------------------------------------------
# 08 Cluster scatter
# ---------------------------------------------------------------------------
safe_draw(8, function() {
  path <- file.path(FIG_DIR, "08_cluster_scatter.png")
  p <- ggplot(cluster2d, aes(x, y, color = cluster)) +
    geom_point(size = 2.2, alpha = 0.85) +
    scale_color_manual(values = pal_cluster) +
    coord_equal() +
    labs(title = "08 Cluster scatter", subtitle = "ggplot2::geom_point colored by k-means-like groups",
         x = "PC1 / X", y = "PC2 / Y", color = "Cluster", caption = "Demo: 6 isotropic Gaussians") +
    theme_ex()
  save_gg(p, path)
  record(8, "聚类散点图", "Cluster scatter plot", "ggplot2", "geom_point()", path)
})

# ---------------------------------------------------------------------------
# 09 Pattern-filled bar
# ---------------------------------------------------------------------------
safe_draw(9, function() {
  path <- file.path(FIG_DIR, "09_pattern_bar.png")
  d <- data.frame(
    group = rep(c("A", "B", "C", "D"), each = 3),
    class = rep(c("Control", "Treat1", "Treat2"), 4),
    value = c(12, 18, 15, 20, 14, 22, 9, 16, 19, 17, 21, 13)
  )
  p <- ggplot(d, aes(class, value, fill = group, pattern = group)) +
    geom_col_pattern(
      position = position_dodge(0.85), width = 0.8,
      pattern_color = "grey20", pattern_fill = "grey20",
      pattern_density = 0.25, pattern_spacing = 0.04, pattern_key_scale_factor = 0.5,
      color = "grey20", linewidth = 0.3
    ) +
    scale_pattern_manual(values = c(A = "stripe", B = "crosshatch", C = "circle", D = "stripe")) +
    scale_fill_manual(values = pal_set2[1:4]) +
    labs(title = "09 Pattern-filled bar chart", subtitle = "ggpattern::geom_col_pattern",
         x = "Condition", y = "Value", caption = "Demo grouped bars with hatch fills") +
    theme_ex()
  save_gg(p, path)
  record(9, "带填充纹理的柱状图", "Texture-filled bar chart", "ggpattern", "geom_col_pattern()", path)
})

# ---------------------------------------------------------------------------
# 10 3D cluster scatter
# ---------------------------------------------------------------------------
safe_draw(10, function() {
  path <- file.path(FIG_DIR, "10_3d_cluster_scatter.png")
  cols <- pal_cluster[as.integer(factor(cluster3d$cluster))]
  save_base(path, {
    par(mar = c(3, 3, 3, 1))
    s3d <- scatterplot3d(
      cluster3d$x, cluster3d$y, cluster3d$z,
      color = cols, pch = 19, cex.symbols = 0.9,
      xlab = "X", ylab = "Y", zlab = "Z", grid = TRUE, box = TRUE,
      angle = 55, main = "10 3D cluster scatter  |  scatterplot3d"
    )
    legend("topright", legend = unique(cluster3d$cluster), pch = 19,
           col = pal_cluster[1:4], bty = "n", cex = 0.85)
  })
  record(10, "三维聚类散点图", "3D cluster scatter", "scatterplot3d", "scatterplot3d()", path)
})

# ---------------------------------------------------------------------------
# 11 Square heatmap 2 (more colors, clustered look)
# ---------------------------------------------------------------------------
safe_draw(11, function() {
  path <- file.path(FIG_DIR, "11_square_heatmap_2.png")
  n <- 12
  set.seed(11)
  m <- matrix(rnorm(n * n), n)
  m <- m %*% t(m) / n
  df <- make_cor_df(m)
  names(df)[3] <- "value"
  p <- ggplot(df, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white", width = 0.9, height = 0.9) +
    scale_fill_gradientn(colors = c("#440154", "#21918c", "#fde725"), name = "value") +
    coord_fixed() +
    labs(title = "11 Square heatmap 2", subtitle = "ggplot2::geom_tile on a Gram matrix",
         x = NULL, y = NULL, caption = "Demo 12 x 12 symmetric matrix") +
    theme_ex() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid = element_blank())
  save_gg(p, path)
  record(11, "方块热图2", "Square heatmap 2", "ggplot2", "geom_tile()", path)
})

# ---------------------------------------------------------------------------
# 12 Inset / local zoom
# ---------------------------------------------------------------------------
safe_draw(12, function() {
  path <- file.path(FIG_DIR, "12_inset_zoom.png")
  d <- data.frame(x = cars$speed, y = cars$dist)
  p <- ggplot(d, aes(x, y)) +
    geom_point(size = 2.4, color = "#2b6cb0") +
    geom_smooth(method = "lm", se = TRUE, color = "#c53030", fill = "#feb2b2") +
    facet_zoom(xlim = c(12, 18), ylim = c(20, 50), horizontal = FALSE, zoom.size = 0.8) +
    labs(title = "12 Inset / local zoom", subtitle = "ggforce::facet_zoom on cars speed vs distance",
         x = "Speed", y = "Stopping distance", caption = "Demo data: datasets::cars") +
    theme_ex()
  save_gg(p, path, height = 7.2)
  record(12, "局部放大图", "Inset / zoom plot", "ggforce", "facet_zoom()", path)
})

# ---------------------------------------------------------------------------
# 13 Pattern-filled stacked bar
# ---------------------------------------------------------------------------
safe_draw(13, function() {
  path <- file.path(FIG_DIR, "13_pattern_stacked.png")
  d <- data.frame(
    month = factor(month.abb[1:8], levels = month.abb[1:8]),
    part = rep(c("Core", "Addon", "Other"), each = 8),
    value = abs(sin(1:24) * 8 + 6)
  )
  p <- ggplot(d, aes(month, value, fill = part, pattern = part)) +
    geom_col_pattern(
      position = "stack", width = 0.72,
      color = "grey20", linewidth = 0.25,
      pattern_density = 0.3, pattern_spacing = 0.035, pattern_fill = "grey20",
      pattern_color = "grey20"
    ) +
    scale_pattern_manual(values = c(Core = "stripe", Addon = "crosshatch", Other = "circle")) +
    scale_fill_manual(values = pal_set2[c(1, 3, 6)]) +
    labs(title = "13 Pattern-filled stacked bar", subtitle = "ggpattern::geom_col_pattern(position='stack')",
         x = "Month", y = "Count", caption = "Demo stacked composition") +
    theme_ex()
  save_gg(p, path)
  record(13, "带填充纹理的堆叠图", "Texture-filled stacked bar", "ggpattern", "geom_col_pattern(position='stack')", path)
})

# ---------------------------------------------------------------------------
# 14 Correlation square heatmap
# ---------------------------------------------------------------------------
safe_draw(14, function() {
  path <- file.path(FIG_DIR, "14_corr_square_heatmap.png")
  p <- ggcorrplot(
    cars_cor, method = "square", lab = FALSE, outline.color = "white",
    colors = c("#2166ac", "white", "#b2182b"),
    ggtheme = theme_ex()
  ) +
    labs(title = "14 Correlation square heatmap", subtitle = "ggcorrplot::ggcorrplot(method='square')",
         caption = "Demo: cor(mtcars numeric subset)")
  save_gg(p, path)
  record(14, "相关性方块热图", "Correlation square heatmap", "ggcorrplot", "ggcorrplot(method='square')", path)
})

# ---------------------------------------------------------------------------
# 15 Waffle
# ---------------------------------------------------------------------------
safe_draw(15, function() {
  path <- file.path(FIG_DIR, "15_waffle.png")
  counts <- c(A = 28, B = 22, C = 18, D = 12, E = 10, F = 10)
  tiles <- unlist(mapply(function(n, lab) rep(lab, n), counts, names(counts)))
  waffle <- data.frame(
    x = rep(1:10, each = 10),
    y = rep(10:1, times = 10),
    cat = factor(tiles, levels = names(counts))
  )
  p <- ggplot(waffle, aes(x, y, fill = cat)) +
    geom_tile(color = "white", linewidth = 1.1, width = 0.92, height = 0.92) +
    scale_fill_manual(values = pal_set2[1:6], name = "Category") +
    coord_equal() +
    labs(title = "15 Waffle chart", subtitle = "ggplot2::geom_tile 10x10 square pie",
         x = NULL, y = NULL, caption = "Demo composition totaling 100 squares") +
    theme_ex() + theme(axis.text = element_blank(), panel.grid = element_blank())
  save_gg(p, path)
  record(15, "华夫图", "Waffle chart", "ggplot2", "geom_tile() (waffle grid)", path)
})

# ---------------------------------------------------------------------------
# 16 3D filled line / ribbon
# ---------------------------------------------------------------------------
safe_draw(16, function() {
  path <- file.path(FIG_DIR, "16_3d_filled_line.png")
  x <- seq(0, 10, length.out = 40)
  zmat <- rbind(
    sin(x) + 2.0,
    0.6 * sin(x + 1) + 3.2,
    0.4 * cos(x / 1.4) + 4.5
  )
  save_base(path, {
    par(mar = c(2, 1, 3, 1))
    persp(
      x = x, y = 1:3, z = t(zmat), theta = 40, phi = 25,
      col = pal_set2[1:3], border = "grey30", ticktype = "detailed",
      xlab = "Time", ylab = "Series", zlab = "Value", expand = 0.6,
      main = "16 3D filled line  |  graphics::persp ribbons"
    )
  })
  record(16, "三维填充折线图", "3D filled line / ribbon", "graphics", "persp()", path)
})

# ---------------------------------------------------------------------------
# 17 Bubble heatmap
# ---------------------------------------------------------------------------
safe_draw(17, function() {
  path <- file.path(FIG_DIR, "17_bubble_heatmap.png")
  d <- expand.grid(x = LETTERS[1:10], y = paste0("R", 1:8))
  d$value <- as.vector(outer(1:10, 1:8, function(a, b) sin(a / 2.2 + b / 3) + 0.15 * a))
  p <- ggplot(d, aes(x, y)) +
    geom_point(aes(size = abs(value), fill = value), shape = 21, color = "grey20") +
    scale_fill_gradientn(colors = heat_cols, name = "value") +
    scale_size_area(max_size = 14, guide = "none") +
    coord_fixed() +
    labs(title = "17 Bubble heatmap", subtitle = "ggplot2::geom_point(shape=21) size + fill",
         x = NULL, y = NULL, caption = "Demo 10 x 8 bubble matrix") +
    theme_ex() + theme(panel.grid = element_blank())
  save_gg(p, path)
  record(17, "气泡热图", "Bubble heatmap", "ggplot2", "geom_point(shape=21)", path)
})

# ---------------------------------------------------------------------------
# 18 Correlation bubble heatmap
# ---------------------------------------------------------------------------
safe_draw(18, function() {
  path <- file.path(FIG_DIR, "18_corr_bubble.png")
  p <- ggcorrplot(
    cars_cor, method = "circle", lab = FALSE, outline.color = "grey30",
    colors = c("#2166ac", "white", "#b2182b"),
    ggtheme = theme_ex()
  ) +
    labs(title = "18 Correlation bubble heatmap", subtitle = "ggcorrplot::ggcorrplot(method='circle')",
         caption = "Demo: cor(mtcars numeric subset)")
  save_gg(p, path)
  record(18, "相关性气泡热图", "Correlation bubble heatmap", "ggcorrplot", "ggcorrplot(method='circle')", path)
})

# ---------------------------------------------------------------------------
# 19 Triangle bubble heatmap
# ---------------------------------------------------------------------------
safe_draw(19, function() {
  path <- file.path(FIG_DIR, "19_triangle_bubble.png")
  df <- make_cor_df(cars_cor) |>
    filter(as.integer(Var1) <= nlevels(Var1) + 1 - as.integer(Var2))
  names(df)[3] <- "r"
  p <- ggplot(df, aes(Var1, Var2)) +
    geom_point(aes(size = abs(r), fill = r), shape = 21, color = "grey20") +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, name = "r") +
    scale_size_area(max_size = 16, guide = "none") +
    coord_fixed() +
    labs(title = "19 Triangle bubble heatmap", subtitle = "Lower triangle geom_point bubbles of correlation",
         x = NULL, y = NULL, caption = "Demo: lower triangle of mtcars correlations") +
    theme_ex() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
  save_gg(p, path)
  record(19, "三角气泡热图", "Triangle bubble heatmap", "ggplot2", "geom_point() + lower.tri filter", path)
})

# ---------------------------------------------------------------------------
# 20 3D bar with category labels
# ---------------------------------------------------------------------------
safe_draw(20, function() {
  path <- file.path(FIG_DIR, "20_3d_bar_labels.png")
  mat <- matrix(c(8, 15, 10, 18, 12, 9, 14, 20, 11, 7, 16, 13), nrow = 4, byrow = TRUE)
  rownames(mat) <- c("Q1", "Q2", "Q3", "Q4")
  colnames(mat) <- c("North", "East", "South")
  heat <- colorRampPalette(c("#fee08b", "#66bd63", "#1a9850"))(12)
  bars <- list()
  labs <- list()
  k_id <- 1
  for (i in 1:4) for (j in 1:3) {
    val <- mat[i, j]
    bars[[k_id]] <- bar_faces(i, j, 0, val, heat[k_id], paste(i, j, sep = "-"))
    top <- iso_project(i, j, val + 0.7)
    labs[[k_id]] <- data.frame(px = top$px, py = top$py, lab = as.character(val))
    k_id <- k_id + 1
  }
  lab_df <- bind_rows(labs)
  faces <- bind_rows(bars)
  faces$grp <- paste(faces$key, faces$face, sep = "_")
  faces <- faces[order(-faces$ysort, faces$xsort, faces$zsort), ]
  p <- ggplot(faces, aes(px, py, group = grp)) +
    geom_polygon(aes(fill = fill_draw), color = "grey25", linewidth = 0.25) +
    geom_text(data = lab_df, aes(px, py, label = lab), inherit.aes = FALSE, size = 3) +
    annotate("text", x = 1:4, y = -0.4, label = rownames(mat), size = 3.2) +
    scale_fill_identity() +
    coord_equal() +
    labs(title = "20 3D bar with category labels",
         subtitle = "Isometric geom_polygon + geom_text on bar tops",
         x = NULL, y = NULL, caption = "Demo quarterly sales by region") +
    theme_ex() + theme(axis.text = element_blank(), panel.grid = element_blank())
  save_gg(p, path)
  record(20, "带类别标签的三维柱状图", "3D bar chart with category labels", "ggplot2", "geom_polygon() + geom_text()", path)
})

# ---------------------------------------------------------------------------
# 21 Triangle square heatmap
# ---------------------------------------------------------------------------
safe_draw(21, function() {
  path <- file.path(FIG_DIR, "21_triangle_square.png")
  p <- ggcorrplot(
    cars_cor, method = "square", type = "lower", lab = FALSE,
    outline.color = "white", colors = c("#313695", "white", "#a50026"),
    ggtheme = theme_ex()
  ) +
    labs(title = "21 Triangle square heatmap", subtitle = "ggcorrplot type='lower', method='square'",
         caption = "Demo: lower triangle of mtcars correlations")
  save_gg(p, path)
  record(21, "三角方块热图", "Triangle square heatmap", "ggcorrplot", "ggcorrplot(type='lower', method='square')", path)
})

# ---------------------------------------------------------------------------
# 22 Unequal-width bar (Marimekko-like)
# ---------------------------------------------------------------------------
safe_draw(22, function() {
  path <- file.path(FIG_DIR, "22_unequal_width_bar.png")
  d <- data.frame(
    cat = c("A", "B", "C", "D", "E"),
    share = c(0.32, 0.24, 0.18, 0.16, 0.10),
    value = c(0.72, 0.55, 0.81, 0.40, 0.63)
  )
  d$xmax <- cumsum(d$share)
  d$xmin <- c(0, head(d$xmax, -1))
  p <- ggplot(d) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = value, fill = cat),
              color = "white", linewidth = 0.6) +
    geom_text(aes(x = (xmin + xmax) / 2, y = value + 0.04, label = cat), size = 3.5) +
    scale_fill_manual(values = pal_set2[1:5]) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(title = "22 Unequal-width bar chart", subtitle = "ggplot2::geom_rect with width = category share",
         x = "Category share (width)", y = "Rate (height)", caption = "Demo Marimekko / variable-width bars") +
    theme_ex() + theme(legend.position = "none")
  save_gg(p, path)
  record(22, "不等宽柱状图", "Unequal-width bar chart", "ggplot2", "geom_rect()", path)
})

# ---------------------------------------------------------------------------
# 23 Density scatter
# ---------------------------------------------------------------------------
safe_draw(23, function() {
  path <- file.path(FIG_DIR, "23_density_scatter.png")
  n <- 800
  x <- rnorm(n, 50, 12)
  y <- 0.65 * x + rnorm(n, 10, 6)
  d <- data.frame(x, y, dens = density_at(x, y))
  p <- ggplot(d, aes(x, y, color = dens)) +
    geom_point(size = 1.6, alpha = 0.9) +
    geom_smooth(method = "lm", se = FALSE, color = "grey15", linewidth = 0.7) +
    scale_color_viridis_c(option = "plasma", name = "density") +
    labs(title = "23 Density scatter", subtitle = "Points colored by MASS::kde2d density",
         x = "Observed", y = "Predicted", caption = "Demo linear cloud with kernel density coloring") +
    theme_ex()
  save_gg(p, path)
  record(23, "密度散点图", "Density scatter plot", "ggplot2 + MASS", "geom_point() + kde2d()", path)
})

# ---------------------------------------------------------------------------
# 24 Floating bar
# ---------------------------------------------------------------------------
safe_draw(24, function() {
  path <- file.path(FIG_DIR, "24_floating_bar.png")
  d <- data.frame(
    grp = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")),
    lo = c(12, 18, 10, 22, 15, 9, 20, 14),
    hi = c(28, 33, 24, 40, 30, 21, 36, 27)
  )
  p <- ggplot(d, aes(grp, ymin = lo, ymax = hi, xmin = as.numeric(grp) - 0.28,
                     xmax = as.numeric(grp) + 0.28)) +
    geom_rect(aes(fill = hi - lo), color = "grey20", linewidth = 0.3) +
    scale_fill_gradient(low = "#c7e9c0", high = "#006d2c", name = "Range") +
    labs(title = "24 Floating bar chart", subtitle = "ggplot2::geom_rect from lo to hi (not from 0)",
         x = "Month", y = "Value range", caption = "Demo interval / Gantt-style floating columns") +
    theme_ex()
  save_gg(p, path)
  record(24, "悬浮柱状图", "Floating bar chart", "ggplot2", "geom_rect(ymin, ymax)", path)
})

# ---------------------------------------------------------------------------
# 25 3D density scatter
# ---------------------------------------------------------------------------
safe_draw(25, function() {
  path <- file.path(FIG_DIR, "25_3d_density_scatter.png")
  n <- 500
  x <- rnorm(n)
  y <- rnorm(n)
  z <- 0.4 * x - 0.3 * y + rnorm(n, sd = 0.5)
  dens <- density_at(x, y)
  cols <- viridis(100, option = "plasma")[cut(dens, 100, labels = FALSE)]
  save_base(path, {
    par(mar = c(3, 3, 3, 5))
    scatterplot3d(
      x, y, z, color = cols, pch = 19, cex.symbols = 0.6,
      xlab = "X", ylab = "Y", zlab = "Z", grid = TRUE, angle = 55,
      main = "25 3D density scatter  |  scatterplot3d + kde2d"
    )
    usr <- par("usr")
    legend_image <- as.raster(matrix(rev(viridis(100, option = "plasma")), ncol = 1))
    rasterImage(legend_image, usr[2] - 0.5, usr[3] + 0.15, usr[2] - 0.2, usr[4] - 0.15, xpd = TRUE)
  })
  record(25, "三维密度散点图", "3D density scatter", "scatterplot3d + MASS", "scatterplot3d() + kde2d()", path)
})

# ---------------------------------------------------------------------------
# 26 Bidirectional stacked (diamond / Likert)
# ---------------------------------------------------------------------------
safe_draw(26, function() {
  path <- file.path(FIG_DIR, "26_bidirectional_stacked.png")
  items <- paste("Item", 1:9)
  levs <- c("SD", "D", "N", "A", "SA")
  d <- expand.grid(item = factor(items, levels = items), att = factor(levs, levels = levs))
  base <- dnorm(as.numeric(d$item), mean = 5, sd = 2.2)
  d$pct <- base * c(0.12, 0.18, 0.28, 0.24, 0.18)[as.numeric(d$att)]
  d$pct <- d$pct / tapply(d$pct, d$item, sum)[d$item]
  d$signed <- ifelse(d$att %in% c("SD", "D"), -d$pct, d$pct)
  d$signed[d$att == "N"] <- 0
  # split Neutral across zero
  neu <- d[d$att == "N", ]
  left <- d
  left$signed[left$att == "N"] <- -neu$pct / 2
  right <- d
  right$signed[right$att == "N"] <- neu$pct / 2
  left$att <- factor(ifelse(left$att == "N", "N_L", as.character(left$att)),
                     levels = c("SA", "A", "N_L", "N_R", "D", "SD"))
  right$att <- factor(ifelse(right$att == "N", "N_R", as.character(right$att)),
                      levels = c("SA", "A", "N_L", "N_R", "D", "SD"))
  plot_df <- rbind(left[left$signed < 0, ], right[right$signed > 0, ])
  plot_df$fill <- recode(as.character(plot_df$att), N_L = "N", N_R = "N")
  plot_df$fill <- factor(plot_df$fill, levels = levs)
  p <- ggplot(plot_df, aes(item, signed, fill = fill)) +
    geom_col(width = 0.9, color = "white", linewidth = 0.2) +
    geom_hline(yintercept = 0, color = "grey20") +
    coord_flip() +
    scale_fill_manual(values = c("#b2182b", "#ef8a62", "#f7f7f7", "#67a9cf", "#2166ac"), name = "Likert") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "26 Bidirectional stacked chart", subtitle = "ggplot2::geom_col diverging Likert stack",
         x = NULL, y = "Share (negative = disagree)", caption = "Demo 5-point Likert items") +
    theme_ex()
  save_gg(p, path)
  record(26, "双向堆叠图", "Bidirectional stacked chart", "ggplot2", "geom_col() diverging stack", path)
})

# ---------------------------------------------------------------------------
# 27 Horizontal bidirectional stacked (pyramid)
# ---------------------------------------------------------------------------
safe_draw(27, function() {
  path <- file.path(FIG_DIR, "27_horizontal_bidirectional.png")
  age <- factor(paste0(seq(0, 80, 5), "-", seq(4, 84, 5)), levels = paste0(seq(0, 80, 5), "-", seq(4, 84, 5)))
  d <- expand.grid(age = age, sex = c("Female", "Male"), band = c("Low", "Mid", "High"))
  center <- dnorm(as.numeric(d$age), mean = 8, sd = 4)
  d$n <- round(400 * center * c(Female = 1.05, Male = 0.95)[d$sex] *
                 c(Low = 0.35, Mid = 0.4, High = 0.25)[d$band])
  d$n[d$sex == "Male"] <- -d$n[d$sex == "Male"]
  p <- ggplot(d, aes(age, n, fill = interaction(sex, band, sep = " / "))) +
    geom_col(width = 1, color = "white", linewidth = 0.15) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "grey20") +
    scale_fill_manual(values = colorRampPalette(c("#fdae61", "#d73027", "#abd9e9", "#4575b4"))(6), name = "Group") +
    labs(title = "27 Horizontal bidirectional stacked", subtitle = "Population-pyramid style stacked geom_col",
         x = "Age group", y = "Count (male left / female right)", caption = "Demo age-sex-band pyramid") +
    theme_ex() + theme(legend.position = "bottom")
  save_gg(p, path)
  record(27, "水平双向堆叠图", "Horizontal bidirectional stacked", "ggplot2", "geom_col() + coord_flip()", path)
})

# ---------------------------------------------------------------------------
# 28 3D cluster scatter with regression
# ---------------------------------------------------------------------------
safe_draw(28, function() {
  path <- file.path(FIG_DIR, "28_3d_cluster_regression.png")
  d <- cluster3d
  fit <- lm(z ~ x + y, data = d)
  cols <- pal_cluster[as.integer(factor(d$cluster))]
  save_base(path, {
    par(mar = c(3, 3, 3, 1))
    s3d <- scatterplot3d(
      d$x, d$y, d$z, color = cols, pch = 19, cex.symbols = 0.85,
      xlab = "X", ylab = "Y", zlab = "Z", grid = TRUE, angle = 55,
      main = "28 3D cluster + regression plane  |  scatterplot3d$plane3d"
    )
    s3d$plane3d(fit, draw_polygon = TRUE, draw_lines = TRUE,
                polygon_args = list(col = adjustcolor("grey70", 0.25), border = NA))
    legend("topright", legend = unique(d$cluster), pch = 19,
           col = pal_cluster[1:4], bty = "n", cex = 0.85)
  })
  record(28, "三维聚类散点图（含回归面）", "3D cluster scatter with regression", "scatterplot3d", "scatterplot3d() + plane3d()", path)
})

# ---------------------------------------------------------------------------
# 29 Triangle heatmap
# ---------------------------------------------------------------------------
safe_draw(29, function() {
  path <- file.path(FIG_DIR, "29_triangle_heatmap.png")
  p <- ggcorrplot(
    cars_cor, method = "square", type = "lower", lab = TRUE, lab_size = 2.6,
    outline.color = "white", colors = c("#4575b4", "#ffffbf", "#d73027"),
    ggtheme = theme_ex()
  ) +
    labs(title = "29 Triangle heatmap", subtitle = "ggcorrplot lower triangle with coefficients",
         caption = "Demo: mtcars correlations")
  save_gg(p, path)
  record(29, "三角热图", "Triangle heatmap", "ggcorrplot", "ggcorrplot(type='lower')", path)
})

# ---------------------------------------------------------------------------
# 30 Streamgraph (manual; ggstream unavailable on this R)
# ---------------------------------------------------------------------------
safe_draw(30, function() {
  path <- file.path(FIG_DIR, "30_streamgraph.png")
  t <- 1:80
  groups <- LETTERS[1:6]
  raw <- lapply(seq_along(groups), function(i) {
    data.frame(
      time = t,
      group = groups[i],
      y = pmax(0.4 + sin(t / (8 + i) + i) + 0.6 * sin(t / 18 + i * 0.7) + 0.15 * i, 0.05)
    )
  })
  d <- bind_rows(raw) |>
    group_by(time) |>
    mutate(
      ymax = cumsum(y) - sum(y) / 2,
      ymin = ymax - y
    ) |>
    ungroup()
  p <- ggplot(d, aes(time, ymin = ymin, ymax = ymax, fill = group)) +
    geom_ribbon(color = "white", linewidth = 0.15) +
    scale_fill_manual(values = pal_set2[1:6]) +
    labs(title = "30 Streamgraph (impact / theme-river)", subtitle = "Centered stacked geom_ribbon (ggstream not on CRAN for R 4.5.2)",
         x = "Time", y = "Centered stacked value", fill = "Series",
         caption = "Demo: hand-built stream layout") +
    theme_ex()
  save_gg(p, path)
  record(30, "冲击图 / 流图", "Streamgraph", "ggplot2", "geom_ribbon() centered stack", path)
})

# ---------------------------------------------------------------------------
# 31 Sankey / alluvial
# ---------------------------------------------------------------------------
safe_draw(31, function() {
  path <- file.path(FIG_DIR, "31_sankey.png")
  d <- data.frame(
    freq = c(32, 18, 14, 22, 10, 16, 12, 8, 20, 15, 9, 11),
    source = rep(c("Urban", "Suburban", "Rural"), each = 4),
    mid = rep(c("Train", "Bus", "Car", "Bike"), 3),
    dest = c("Work", "Work", "School", "Home",
             "Work", "School", "Home", "Home",
             "School", "Work", "Home", "School")
  )
  p <- ggplot(d, aes(y = freq, axis1 = source, axis2 = mid, axis3 = dest)) +
    geom_alluvium(aes(fill = source), width = 1 / 12, alpha = 0.8, knot.pos = 0.4) +
    geom_stratum(width = 1 / 8, fill = "grey95", color = "grey30") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_fill_manual(values = pal_set2[1:3], name = "Origin") +
    scale_x_discrete(limits = c("Origin", "Mode", "Destination"), expand = c(0.05, 0.05)) +
    labs(title = "31 Sankey / alluvial diagram", subtitle = "ggalluvial::geom_alluvium + geom_stratum",
         y = "Count", caption = "Demo commuting flows") +
    theme_ex() + theme(axis.title.x = element_blank())
  save_gg(p, path)
  record(31, "桑基图", "Sankey / alluvial diagram", "ggalluvial", "geom_alluvium()", path)
})

# ---------------------------------------------------------------------------
# 32 Violin
# ---------------------------------------------------------------------------
safe_draw(32, function() {
  path <- file.path(FIG_DIR, "32_violin.png")
  p <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
    geom_violin(trim = FALSE, alpha = 0.8, color = "grey20", linewidth = 0.3) +
    geom_boxplot(width = 0.12, outlier.size = 0.8, fill = "white") +
    scale_fill_manual(values = pal_set2[1:3]) +
    labs(title = "32 Violin plot", subtitle = "ggplot2::geom_violin + geom_boxplot",
         x = "Species", y = "Sepal length (cm)", caption = "Demo data: iris") +
    theme_ex() + theme(legend.position = "none")
  save_gg(p, path)
  record(32, "小提琴图", "Violin plot", "ggplot2", "geom_violin()", path)
})

# ---------------------------------------------------------------------------
# 33 Dual surface
# ---------------------------------------------------------------------------
safe_draw(33, function() {
  path <- file.path(FIG_DIR, "33_dual_surface.png")
  x <- seq(-2, 2, length.out = 28)
  y <- seq(-2, 2, length.out = 28)
  z1 <- outer(x, y, function(a, b) exp(-((a + 0.4)^2 + (b + 0.3)^2)))
  z2 <- outer(x, y, function(a, b) 0.7 * exp(-((a - 0.5)^2 + (b - 0.4)^2))) + 0.35
  save_base(path, {
    par(mar = c(2, 1, 3, 1))
    pmat <- persp(
      x, y, z1, theta = 40, phi = 25, ticktype = "detailed",
      col = adjustcolor("#c51b8a", 0.55), border = NA, expand = 0.7,
      xlab = "X", ylab = "Y", zlab = "Z", zlim = c(0, 1.15),
      main = "33 Dual surface  |  persp + trans3d second surface"
    )
    add_surface(pmat, x, y, z2, col = "#3182bd", border = NA, alpha = 0.45)
  })
  record(33, "双曲面图", "Dual surface plot", "graphics", "persp() + trans3d()", path)
})

# ---------------------------------------------------------------------------
# 34 Dual-triangle heatmap
# ---------------------------------------------------------------------------
safe_draw(34, function() {
  path <- file.path(FIG_DIR, "34_dual_triangle_heatmap.png")
  df <- make_cor_df(cars_cor)
  names(df)[3] <- "r"
  pmat <- ggcorrplot::cor_pmat(cars_num)
  df$p <- as.data.frame(as.table(pmat))$Freq
  df$upper <- as.integer(df$Var1) > nlevels(df$Var1) + 1 - as.integer(df$Var2)
  p <- ggplot(df, aes(Var1, Var2)) +
    geom_tile(data = subset(df, upper), aes(fill = r), color = "white") +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, name = "r") +
    new_scale_fill() +
    geom_tile(data = subset(df, !upper), aes(fill = -log10(pmax(p, 1e-10))), color = "white") +
    scale_fill_gradient(low = "#ffffcc", high = "#006837", name = "-log10 p") +
    coord_fixed() +
    labs(title = "34 Dual-triangle heatmap", subtitle = "ggnewscale: upper = correlation, lower = -log10(p)",
         x = NULL, y = NULL, caption = "Demo: mtcars r (upper) vs p-value (lower)") +
    theme_ex() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
  save_gg(p, path)
  record(34, "双三角热图", "Dual-triangle heatmap", "ggplot2 + ggnewscale", "geom_tile() + new_scale_fill()", path)
})

# ---------------------------------------------------------------------------
# 35 Dual-feature 3D scatter (tree-like color + size)
# ---------------------------------------------------------------------------
safe_draw(35, function() {
  path <- file.path(FIG_DIR, "35_dual_feature_3d_scatter.png")
  trunk <- data.frame(
    x = rnorm(80, 0, 0.08),
    y = rnorm(80, 0, 0.08),
    z = runif(80, 0, 2.2),
    feat1 = runif(80, 0.1, 0.3),
    feat2 = runif(80, 0.4, 0.8)
  )
  canopy <- data.frame(
    x = rnorm(420, 0, 0.55),
    y = rnorm(420, 0, 0.55),
    z = rnorm(420, 3.1, 0.45),
    feat1 = rnorm(420, 0.75, 0.12),
    feat2 = rnorm(420, 1.4, 0.3)
  )
  d <- rbind(trunk, canopy)
  d$feat1 <- pmin(pmax(d$feat1, 0), 1)
  ramp <- colorRampPalette(c("#8c510a", "#c7e9c0", "#006d2c"))(100)
  cols <- ramp[pmax(1, cut(d$feat1, 100, labels = FALSE))]
  cexs <- 0.35 + 0.9 * rescale(d$feat2)
  save_base(path, {
    par(mar = c(3, 3, 3, 1))
    scatterplot3d(
      d$x, d$y, d$z, color = cols, pch = 19, cex.symbols = cexs,
      xlab = "X", ylab = "Y", zlab = "Z", grid = TRUE, angle = 55,
      main = "35 Dual-feature 3D scatter  |  color + size"
    )
  })
  record(35, "双特征渲染三维散点图", "Dual-feature 3D scatter", "scatterplot3d", "scatterplot3d(color, cex.symbols)", path)
})

# ---------------------------------------------------------------------------
# 36 Dual grid surface
# ---------------------------------------------------------------------------
safe_draw(36, function() {
  path <- file.path(FIG_DIR, "36_dual_grid_surface.png")
  x <- seq(-pi, pi, length.out = 26)
  y <- seq(-pi, pi, length.out = 26)
  z1 <- outer(x, y, function(a, b) sin(a) * cos(b))
  z2 <- outer(x, y, function(a, b) 0.45 * cos(a * 0.8) * sin(b * 0.8) + 0.8)
  save_base(path, {
    par(mar = c(2, 1, 3, 1))
    pmat <- persp(
      x, y, z1, theta = 35, phi = 28, ticktype = "detailed",
      col = adjustcolor("#1d91c0", 0.55), border = "grey25", lwd = 0.25,
      xlab = "X", ylab = "Y", zlab = "Z", expand = 0.65, zlim = c(-1.2, 1.4),
      main = "36 Dual grid surface  |  persp mesh + second grid"
    )
    add_surface(pmat, x, y, z2, col = "#78c679", border = "grey20", alpha = 0.4, lwd = 0.25)
  })
  record(36, "双网格曲面图", "Dual grid surface plot", "graphics", "persp() + trans3d() grid", path)
})

# ---------------------------------------------------------------------------
# Catalog + HTML gallery
# ---------------------------------------------------------------------------
cat_df <- bind_rows(results)
write.csv(cat_df, file.path(OUT_DIR, "catalog.csv"), row.names = FALSE, fileEncoding = "UTF-8")

ok <- cat_df[!is.na(cat_df$file), ]
cards <- vapply(seq_len(nrow(ok)), function(i) {
  row <- ok[i, ]
  sprintf(
    '<figure class="card"><img src="figures/%s" alt="%s"><figcaption><strong>%02d. %s</strong><br><span>%s</span><br><code>%s</code></figcaption></figure>',
    row$file, row$chinese, as.integer(row$id), row$chinese, row$english, row$r_function
  )
}, character(1))

html <- paste0(
  '<!DOCTYPE html><html lang="zh-CN"><head><meta charset="utf-8">',
  '<title>36 种科研图表示例画廊</title>',
  '<style>',
  'body{font-family:-apple-system,BlinkMacSystemFont,"PingFang SC","Noto Sans SC",sans-serif;margin:24px;background:#f7f7f5;color:#1f2933;}',
  'h1{font-size:22px;margin:0 0 8px;} .meta{color:#6b7280;margin-bottom:20px;}',
  '.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(280px,1fr));gap:16px;}',
  '.card{background:#fff;border:1px solid #e5e7eb;border-radius:10px;padding:10px;margin:0;}',
  '.card img{width:100%;height:210px;object-fit:contain;background:#fff;}',
  'figcaption{font-size:13px;line-height:1.45;margin-top:8px;} code{font-size:11px;color:#334155;}',
  '</style></head><body>',
  '<h1>36 种图表类型：公开 R 语言案例画廊</h1>',
  '<p class="meta">源图识别 36 种类型；每种用公开 R 包绘制一个可复现案例。详见 REPORT.md。成功 ',
  nrow(ok), ' / ', nrow(cat_df), '。</p>',
  '<div class="grid">', paste(cards, collapse = "\n"), '</div>',
  '</body></html>'
)
writeLines(html, file.path(OUT_DIR, "index.html"), useBytes = TRUE)

message("Done. Catalog rows: ", nrow(cat_df), " success: ", nrow(ok))
print(cat_df[, c("id", "chinese", "file", "r_function")])
