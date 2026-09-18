#!/usr/bin/env Rscript
# 31 桑基图
.here <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
source(file.path(.here, "..", "_common.R"))
suppressPackageStartupMessages(library(ggalluvial))

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
  labs(title = "31 Sankey / alluvial", subtitle = "ggalluvial with crossing flows", y = NULL) +
  theme_ex() + theme(axis.text.y = element_blank(), panel.grid = element_blank())
save_gg(p, file.path(FIG_DIR, "31_sankey.png"))
