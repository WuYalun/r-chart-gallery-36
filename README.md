# 36 种科研图表：公开 R 语言案例画廊

对照微信公众号「阿昆的科研日常」合集，识别 **36** 种图，并用公开 R 包各画一张可复现案例。

- 报告：[`REPORT.md`](REPORT.md)
- 浏览：用浏览器打开 [`index.html`](index.html)
- 对照表：[`catalog.csv`](catalog.csv)

## 复现

```bash
Rscript scripts/draw_all_charts.R
Rscript scripts/redraw_misaligned.R
Rscript scripts/fix_heatmaps.R
```

案例数据均为演示数据（`iris` / `mtcars` / 合成矩阵），不是源图里的原始观测。

## 第 17、18 张

- **17 气泡热图**：单元格网格 + 紫橙顺序色气泡（`geom_tile` + `geom_point(shape = 21)`）
- **18 相关性气泡热图**：`linkET::qcorrplot()` 气泡矩阵 + `geom_couple()` **Mantel 连接臂**；另有 `ComplexHeatmap` 聚类树版本 `figures/18_corr_bubble_dendrogram.png`
