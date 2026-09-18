#!/usr/bin/env Rscript
# 18 相关性气泡热图 + Mantel 连接臂
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(linkET))

set.seed(18)
n <- 80
env <- data.frame(
  Temp = rnorm(n), pH = rnorm(n), DO = rnorm(n), TN = rnorm(n), TP = rnorm(n),
  TOC = rnorm(n), NH4 = rnorm(n), Cond = rnorm(n), Sal = rnorm(n)
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
  spec_select = list(Cyano = 1:4, Proteo = 5:8, Firmic = 9:12)
) |>
  mutate(
    pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
             labels = c("<= 0.01", "0.01 - 0.05", "> 0.05"))
  )
p <- qcorrplot(correlate(env), type = "full") +
  geom_tile(fill = NA, color = "#d0d5dd", linewidth = 0.35) +
  geom_point(aes(fill = r, size = abs(r)), shape = 21, color = "#4b5563", stroke = 0.35) +
  scale_fill_gradientn(
    colours = c("#1b7837", "#a6dba0", "#f7f7f7", "#c2a5cf", "#762a83"),
    limits = c(-1, 1), name = "Pearson's r"
  ) +
  scale_size(range = c(2.4, 8.6), guide = "none") +
  geom_couple(aes(colour = pd), data = mantel, curvature = nice_curvature(),
              size = 0.9, label.size = 3.6, nudge_x = 0.85) +
  scale_colour_manual(values = c("#D95F02", "#1B9E77", "#c8c8c8"), name = "Mantel's p") +
  labs(title = "18 Correlation bubble heatmap",
       subtitle = "linkET::qcorrplot + geom_couple connecting arms",
       x = NULL, y = NULL) +
  theme_ex() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        plot.margin = margin(8, 22, 8, 8))
save_gg(p, file.path(FIG_DIR, "18_corr_bubble.png"), width = 10.2, height = 6.5)
