#!/usr/bin/env Rscript
# 34 双三角热图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))

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
