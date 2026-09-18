# Shared helpers for the per-chart scripts in scripts/charts/.
# Each chart does: source(file.path(dirname(this_file), "..", "_common.R"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(MASS)
  library(RColorBrewer)
  library(viridis)
})

set.seed(20260918)

.gallery_root <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) {
    d <- dirname(normalizePath(f[1]))
    if (basename(d) == "charts") return(normalizePath(file.path(d, "..", "..")))
    if (basename(d) == "scripts") return(normalizePath(file.path(d, "..")))
    return(normalizePath(file.path(d, "..", "..")))
  }
  if (dir.exists("figures")) return(normalizePath("."))
  if (dir.exists("../figures")) return(normalizePath(".."))
  normalizePath(".")
})

FIG_DIR <- file.path(.gallery_root, "figures")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

W <- 7.2
H <- 5.6
DPI <- 160

pal_set2 <- brewer.pal(8, "Set2")
pal_spectral <- rev(brewer.pal(11, "Spectral"))
pal_cluster <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
pal_ridge <- colorRampPalette(c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"))(12)

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
  message("wrote ", path)
}

save_base <- function(path, expr, width = W, height = H) {
  ragg::agg_png(path, width = width, height = height, units = "in", res = DPI, background = "white")
  on.exit(dev.off(), add = TRUE)
  eval.parent(substitute(expr))
  message("wrote ", path)
}

shade_hex <- function(hex, s) {
  rgb_v <- pmin(pmax(col2rgb(hex) / 255 * s, 0), 1)
  rgb(rgb_v[1, ], rgb_v[2, ], rgb_v[3, ])
}

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

load_chart_common <- function() invisible(NULL)

.chart_source_common <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  here <- if (length(f)) dirname(normalizePath(f[1])) else getwd()
  common <- file.path(here, "..", "_common.R")
  if (!file.exists(common)) stop("找不到 scripts/_common.R")
  source(common, local = FALSE)
}
