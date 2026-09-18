#!/usr/bin/env Rscript
# 27 水平双向堆叠图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

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
       subtitle = "Thin stacked layers forming a violin / pyramid",
       x = NULL, y = NULL) +
  theme_ex() + theme(axis.text = element_blank(), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "27_horizontal_bidirectional.png"))
