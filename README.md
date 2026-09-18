# 36 种科研图表：R 语言案例库

小组内部用的可复现图表示例，共 **36** 种，用公开 R 包各画一张。

- 说明：[`REPORT.md`](REPORT.md)
- 浏览：打开 [`index.html`](index.html)
- 类型一览：[`catalog.csv`](catalog.csv)

## 复现

```bash
Rscript scripts/draw_all_charts.R
Rscript scripts/redraw_misaligned.R
Rscript scripts/fix_heatmaps.R
```

图里用的是演示数据（`iris` / `mtcars` / 合成矩阵）。

## 第 17、18 张

- **17 气泡热图**：单元格网格 + 紫橙顺序色气泡（`geom_tile` + `geom_point(shape = 21)`）
- **18 相关性气泡热图**：`linkET::qcorrplot()` 气泡矩阵 + `geom_couple()` **Mantel 连接臂**；另有 `ComplexHeatmap` 聚类树版本 `figures/18_corr_bubble_dendrogram.png`
