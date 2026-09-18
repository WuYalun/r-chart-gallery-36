# 36 种科研图表：R 语言案例库

小组内部用的可复现图表示例，共 **36** 种。仓库公开：<https://github.com/WuYalun/r-chart-gallery-36>

- 说明：[`REPORT.md`](REPORT.md)
- 浏览：打开 [`index.html`](index.html)
- 类型一览：[`catalog.csv`](catalog.csv)

## 单独画一张

每种图一个脚本，在 `scripts/charts/`。公共主题和保存函数在 `scripts/_common.R`。

```bash
# 只画气泡热图
Rscript scripts/charts/17_bubble_heatmap.R

# 只画带连接臂的相关性气泡
Rscript scripts/charts/18_corr_bubble.R

# 全部重画
Rscript scripts/draw_each.R
```

PNG 写到 `figures/`。图里用的是演示数据（`iris` / `mtcars` / 合成矩阵）。

## 脚本一览

| 编号 | 脚本 |
| --- | --- |
| 01 | `scripts/charts/01_ridgeline.R` |
| 02 | `scripts/charts/02_linear_heatmap.R` |
| 03 | `scripts/charts/03_square_heatmap.R` |
| 04 | `scripts/charts/04_3d_stacked_bar.R` |
| 05 | `scripts/charts/05_wind_rose.R` |
| 06 | `scripts/charts/06_radar.R` |
| 07 | `scripts/charts/07_linear_heatmap_series.R` |
| 08 | `scripts/charts/08_cluster_scatter.R` |
| 09 | `scripts/charts/09_pattern_bar.R` |
| 10 | `scripts/charts/10_3d_cluster_scatter.R` |
| 11 | `scripts/charts/11_square_heatmap_2.R` |
| 12 | `scripts/charts/12_inset_zoom.R` |
| 13 | `scripts/charts/13_pattern_stacked.R` |
| 14 | `scripts/charts/14_corr_square_heatmap.R` |
| 15 | `scripts/charts/15_waffle.R` |
| 16 | `scripts/charts/16_3d_filled_line.R` |
| 17 | `scripts/charts/17_bubble_heatmap.R` |
| 18 | `scripts/charts/18_corr_bubble.R` |
| 18b | `scripts/charts/18b_corr_bubble_dendrogram.R` |
| 19 | `scripts/charts/19_triangle_bubble.R` |
| 20 | `scripts/charts/20_3d_bar_labels.R` |
| 21 | `scripts/charts/21_triangle_square.R` |
| 22 | `scripts/charts/22_unequal_width_bar.R` |
| 23 | `scripts/charts/23_density_scatter.R` |
| 24 | `scripts/charts/24_floating_bar.R` |
| 25 | `scripts/charts/25_3d_density_scatter.R` |
| 26 | `scripts/charts/26_bidirectional_stacked.R` |
| 27 | `scripts/charts/27_horizontal_bidirectional.R` |
| 28 | `scripts/charts/28_colored_scatter_regression.R` |
| 29 | `scripts/charts/29_triangle_heatmap.R` |
| 30 | `scripts/charts/30_stacked_filled.R` |
| 31 | `scripts/charts/31_sankey.R` |
| 32 | `scripts/charts/32_violin.R` |
| 33 | `scripts/charts/33_dual_surface.R` |
| 34 | `scripts/charts/34_dual_triangle_heatmap.R` |
| 35 | `scripts/charts/35_dual_feature_3d_scatter.R` |
| 36 | `scripts/charts/36_dual_grid_surface.R` |
