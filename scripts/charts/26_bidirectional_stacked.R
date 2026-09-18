#!/usr/bin/env Rscript
# 26 双向堆叠图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

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
       subtitle = "Stacked horizontal segments forming a diamond",
       x = "Stacked share", y = "Bin") +
  theme_ex() + theme(axis.text.y = element_blank())
save_gg(p, file.path(FIG_DIR, "26_bidirectional_stacked.png"))
