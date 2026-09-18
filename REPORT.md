# 36 种科研图表：R 语言画法说明

- 用途：小组内部可复现案例库
- 日期：2026-09-18
- 环境：R 4.5.2（aarch64-apple-darwin20）
- 结果：**36** 种类型，每种 1 张案例图，**36 / 36** 成功

用浏览器打开 [`index.html`](index.html) 可一次浏览全部案例。复现脚本见 [`scripts/draw_all_charts.R`](scripts/draw_all_charts.R)。类型一览见 [`catalog.csv`](catalog.csv)。

---

## 1. 类型一览

按图形家族归类如下。

| 家族 | 编号 | 说明 |
| --- | --- | --- |
| 分布 | 1, 32 | 山脊图、小提琴图 |
| 热图 / 矩阵 | 2, 3, 7, 11, 17 | 连续栅格、方块、序列热图、气泡热图 |
| 相关矩阵变体 | 14, 18, 19, 21, 29, 34 | 方块 / 气泡 / 三角 / 双三角 |
| 柱状与构成 | 9, 13, 15, 22, 24, 26, 27 | 纹理柱、华夫、不等宽、悬浮、双向堆叠 |
| 极坐标 | 5, 6 | 风玫瑰、雷达 |
| 散点 | 8, 12, 23 | 聚类、局部放大、密度着色 |
| 流向 / 时间构成 | 30, 31 | 冲击图（流图）、桑基图 |
| 三维柱 | 4, 20 | 堆叠三维柱、带标签三维柱 |
| 三维散点 | 10, 25, 28, 35 | 聚类、密度、回归面、双特征渲染 |
| 三维曲面 | 16, 33, 36 | 填充折线/色带、双曲面、双网格曲面 |

下面是对应的 **公开 R 实现**。案例数据均为演示数据（`iris`、`mtcars`、`cars` 或合成数据）。

---

## 2. 公开 R 画法总表

每一种图优先写 **本案例实际用到的函数**，再给出常用公开替代。文档链接均为公开页面（CRAN / ggplot2 官网 / 书籍 / 问答）。

### 2.1 分布

**01 山脊图（Ridgeline Plot）**  
- 本案例：`ggridges::geom_density_ridges()`  
- 文档：<https://rdrr.io/cran/ggridges/man/geom_density_ridges.html>  
- 适用：多组一维分布对比，比分面密度图更省空间。  
- 案例文件：`figures/01_ridgeline.png`

**32 小提琴图（Violin Plot）**  
- 本案例：`ggplot2::geom_violin()` + `geom_boxplot()`  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_violin.html>  
- 案例文件：`figures/32_violin.png`

### 2.2 热图与矩阵

**02 线型热图（Linear Heatmap）**  
- 本案例：`ggplot2::geom_tile()`，一行一条细色带  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_tile.html>  
- 要点：多序列条带热图，不是二维连续场。  
- 案例文件：`figures/02_linear_heatmap.png`

**07 线型热图（序列版）**  
- 本案例：`ggplot2::geom_raster()` 画「时间 × 序列」矩阵，避免 `geom_tile` 缝隙造成的棋盘伪影  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_raster.html>  
- 要点：第 7 张是谱图 / 多序列热条，与第 2 张的连续场要分开。  
- 案例文件：`figures/07_linear_heatmap_series.png`

**03 / 11 方块热图**  
- 本案例：`geom_tile(color = "white", width < 1)` 留缝  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_tile.html>  
- 第 11 张必须给矩阵行列名，否则 `as.table()` 会得到 `NA` 轴、整图糊成一块。  
- 案例文件：`figures/03_square_heatmap.png`、`figures/11_square_heatmap_2.png`

**17 气泡热图**  
- 本案例：浅灰 `geom_tile` 单元格网格 + `geom_point(shape = 21)`，`size` 与 `fill` 双编码  
- 配色：紫 → 橙顺序色  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_point.html>  
- 案例文件：`figures/17_bubble_heatmap.png`

### 2.3 相关矩阵变体

相关图是本库里最大的子系列。公开方案主要是 `ggcorrplot`（ggplot2 语法）、`corrplot`（base 图）、以及带**连接臂**的 `linkET`。

- `ggcorrplot` 画廊：<https://cran.r-project.org/web/packages/ggcorrplot/vignettes/publication-ready-correlation-plots.html>  
- 手写 ggplot 气泡相关图：<https://www.r-bloggers.com/2024/02/correlation-heat-maps-with-ggplot2/>  
- 双三角不同色标：<https://stackoverflow.com/questions/71342643/single-heatmap-on-two-symetric-matrices-with-different-colours-and-scales-r>  
- Mantel 连接臂：`linkET::qcorrplot()` + `geom_couple()`，<https://github.com/Hy4m/linkET>  
- 气泡 + 聚类树：`ComplexHeatmap` `cell_fun` 画圆，`cluster_rows/columns = TRUE`

| 编号 | 中文 | 本案例函数 | 案例文件 |
| --- | --- | --- | --- |
| 14 | 相关性方块热图 | `geom_tile()`，PRGn、hclust 排序 | `figures/14_corr_square_heatmap.png` |
| 18 | 相关性气泡热图 | `linkET::qcorrplot()` 气泡 + `geom_couple()` Mantel **连接臂** | `figures/18_corr_bubble.png` |
| 18b | 气泡 + 聚类树 | `ComplexHeatmap` 圆点矩阵 + 上下/左右树 | `figures/18_corr_bubble_dendrogram.png` |
| 19 | 三角气泡热图 | 下三角单元格网格 + `geom_point(shape = 21)` | `figures/19_triangle_bubble.png` |
| 21 | 三角方块热图 | 下三角 `geom_tile()`，青绿顺序色 | `figures/21_triangle_square.png` |
| 29 | 三角热图 | 下三角 `geom_tile()`，RdBu 留缝 | `figures/29_triangle_heatmap.png` |
| 34 | 双三角热图 | `geom_tile()` + `ggnewscale::new_scale_fill()`，上三角相关系数、下三角 `-log10(p)` | `figures/34_dual_triangle_heatmap.png` |

`corrplot` 的 `method = "square"/"circle"/"color"` 与 `type = "lower"/"upper"` 也能覆盖 14/18/19/21/29。第 18 张的「连接臂」是生态学论文里常见的 **Mantel 相关连线**（环境因子气泡矩阵连到物种/功能群），不是单纯再画一张 `ggcorrplot(method = "circle")`。

### 2.4 柱状、构成、堆叠

**09 带填充纹理的柱状图**  
- 本案例：`ggpattern::geom_col_pattern()`  
- 文档：<https://cran.r-project.org/package=ggpattern>  
- 案例文件：`figures/09_pattern_bar.png`

**13 带填充纹理的堆叠图**  
- 本案例：`geom_col_pattern(position = "stack")`  
- 案例文件：`figures/13_pattern_stacked.png`

**15 华夫图**  
- 本案例：10×10 `geom_tile()` 方块饼图  
- 专用包：`waffle::waffle()`，<https://search.r-project.org/CRAN/refmans/waffle/html/waffle.html>  
- 案例文件：`figures/15_waffle.png`

**22 不等宽柱状图**  
- 本案例：`geom_rect()`，宽度 = 类别份额，高度 = 比率（Marimekko / mosaic 思路）  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_tile.html>  
- 案例文件：`figures/22_unequal_width_bar.png`

**24 悬浮柱状图**  
- 本案例：`geom_rect(ymin, ymax)`，柱子不从 0 起  
- 参考：<https://stackoverflow.com/questions/45981366/floating-bar-chart-with-trend-line-on-secondary-axis>  
- 也可用 `geom_linerange` / `geom_errorbar` 画区间。  
- 案例文件：`figures/24_floating_bar.png`

**26 双向堆叠图**  
- 本案例：Likert 发散堆叠，`geom_col()`，不同意一侧为负  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_bar.html>  
- 案例文件：`figures/26_bidirectional_stacked.png`

**27 水平双向堆叠图**  
- 本案例：年龄–性别金字塔，`geom_col()` + `coord_flip()`  
- 案例文件：`figures/27_horizontal_bidirectional.png`

### 2.5 极坐标

**05 风玫瑰图**  
- 本案例：风向 16 方位分箱 + 风速分级，`geom_col()` + `coord_polar()`  
- 气象专用：`openair::windRose()`，<https://openair-project.github.io/book/sections/directional-analysis/wind-roses.html>  
- 案例文件：`figures/05_wind_rose.png`

**06 雷达图**  
- 本案例：`fmsb::radarchart()`  
- 文档：<https://search.r-project.org/CRAN/refmans/fmsb/html/radarchart.html>  
- 要点：数据框前两行必须是各轴最大值、最小值。ggplot2 替代有 `ggradar`。  
- 案例文件：`figures/06_radar.png`

### 2.6 散点与局部放大

**08 聚类散点图**  
- 本案例：`geom_point()`，颜色 = 簇  
- 文档：<https://ggplot2.tidyverse.org/reference/geom_point.html>  
- 真实分析里通常先 `kmeans()` / `hclust()` / `Rtsne` / `uwot`。  
- 案例文件：`figures/08_cluster_scatter.png`

**12 局部放大图**  
- 本案例：`ggforce::facet_zoom()`  
- 文档：<https://ggforce.data-imaginist.com/reference/facet_zoom.html>  
- 插图式局部放大也可用 `cowplot::ggdraw()` + `draw_plot()`。  
- 案例文件：`figures/12_inset_zoom.png`

**23 密度散点图**  
- 本案例：`MASS::kde2d()` 给点着色 + `geom_smooth()`  
- 等高线替代：`geom_density_2d()` / `geom_density_2d_filled()`，<https://ggplot2.tidyverse.org/reference/geom_density_2d.html>  
- 案例文件：`figures/23_density_scatter.png`

### 2.7 流向与时间构成

**30 冲击图 / 流图（Streamgraph）**  
- 本案例：把各组 `y` 做成围绕 0 的累积带，`geom_ribbon()`  
- 专用包：`ggstream::geom_stream()`，<https://search.r-project.org/CRAN/refmans/ggstream/html/geom_stream.html>  
- 说明：当前 R 4.5.2 的 CRAN 没有可安装的 `ggstream`，因此用手写中心化堆叠带。  
- 案例文件：`figures/30_streamgraph.png`

**31 桑基图**  
- 本案例：`ggalluvial::geom_alluvium()` + `geom_stratum()`  
- 文档：<https://corybrunson.github.io/ggalluvial/>  
- 其他公开方案：GitHub `davidsjoberg/ggsankey`，交互式 `networkD3::sankeyNetwork()`。  
- 案例文件：`figures/31_sankey.png`

### 2.8 三维

本机未装 XQuartz，`plot3D` 因 `tcltk` 依赖 X11 无法加载。案例改用 **不依赖 X11** 的公开方法：

- 三维柱：`persp()` + `trans3d()` 画立方体
- 三维散点：`scatterplot3d`
- 三维曲面 / 密度山：`graphics::persp()` + `trans3d()`

若已安装 XQuartz，也可用：

- `plot3D::hist3D` / `box3D` / `text3D`：三维柱  
  教程：<https://openbiox.github.io/Bizard/Hiplot/003-barplot-3d.html>  
- `plot3D::scatter3D`、`persp3D`、`ribbon3D`、`surf3D`：散点与曲面  
  文档：<https://rdrr.io/cran/plot3D/man/persp3D.html>  
- 交互三维：`plotly` scatter3d，<https://plotly.com/r/3d-scatter-plots/>

| 编号 | 中文 | 本案例 | 有 XQuartz 时的常用公开函数 | 案例文件 |
| --- | --- | --- | --- | --- |
| 4 | 三维堆叠柱状图 | `persp` + `trans3d` 立方体 | `plot3D::box3D` 分层堆叠 | `figures/04_3d_stacked_bar.png` |
| 20 | 带类别标签的三维柱 | 同上，柱顶 `text()` | `hist3D` + `text3D` | `figures/20_3d_bar_labels.png` |
| 10 | 三维聚类散点 | `scatterplot3d` 密集点 | `scatter3D` | `figures/10_3d_cluster_scatter.png` |
| 25 | 三维密度 | `persp(MASS::kde2d())` | `persp3D` 密度曲面 | `figures/25_3d_density_scatter.png` |
| 28 | 二维着色散点 + 回归线 | `geom_point` + `geom_smooth` | — | `figures/28_3d_cluster_regression.png` |
| 35 | 双特征渲染三维散点 | 颜色 + 点大小 | `scatter3D(colvar, cex)` 或 plotly | `figures/35_dual_feature_3d_scatter.png` |
| 16 | 三维填充折线 | `trans3d` 面积墙 | `plot3D::ribbon3D` | `figures/16_3d_filled_line.png` |
| 33 | 双曲面图 | `persp()` + `trans3d()` 第二层 | `persp3D(..., add = TRUE)` | `figures/33_dual_surface.png` |
| 36 | 双网格曲面 | 同上，保留网格边 | `persp3D(border = ...)` 两层 | `figures/36_dual_grid_surface.png` |

第 35 张是「树状」点云：主干与树冠用不同颜色/大小编码两个特征。案例用棕色主干 + 绿色树冠做双通道渲染。

---

## 3. 案例图怎么画

统一约定：

- 随机种子 `20260918`
- 输出 7.2 × 5.6 in，160 dpi PNG（第 12 张局部放大为 7.2 × 7.2）
- 主包：`ggplot2`、`ggridges`、`ggpattern`、`ggforce`、`ggalluvial`、`ggcorrplot`、`ggnewscale`、`fmsb`、`scatterplot3d`、`MASS`

一键复现（按图分开的脚本）：

```bash
Rscript scripts/charts/17_bubble_heatmap.R   # 只画一张
Rscript scripts/draw_each.R                  # 36 张全画
```

旧的打包脚本仍可用：`scripts/draw_all_charts.R`、`scripts/redraw_misaligned.R`、`scripts/fix_heatmaps.R`。以后优先改 `scripts/charts/` 里的单图程序。

依赖（本机已装，若换机器需安装）：

```r
install.packages(c(
  "ggplot2", "dplyr", "tidyr", "scales", "MASS", "RColorBrewer", "viridis",
  "ggridges", "ggpattern", "ggforce", "ggalluvial", "ggnewscale", "ggrepel",
  "cowplot", "corrplot", "ggcorrplot", "fmsb", "scatterplot3d", "hexbin",
  "ragg", "vegan", "linkET", "circlize"
))
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("ComplexHeatmap"))
```

`openair`、`plot3D`、`ggstream`、`waffle`、`ggsankey` 是公开替代，不是本案例的硬依赖。

---

## 4. 文件夹内容

```
r-chart-gallery-20260918/
├── REPORT.md                 # 本说明
├── README.md                 # 仓库说明
├── index.html                # 36 张案例图浏览页
├── catalog.csv               # 类型一览（含文档链接）
├── scripts/
│   ├── _common.R             # 主题、保存、三维辅助
│   ├── charts/               # 每种图一个独立 R 脚本
│   ├── draw_each.R           # 依次运行 charts/ 下全部脚本
│   ├── draw_all_charts.R     # 旧打包初稿
│   ├── redraw_misaligned.R   # 旧几何修正
│   └── fix_heatmaps.R        # 旧热图修正
└── figures/                  # 01–36 案例 PNG（含 18b 聚类树）
```

| 文件 | 类型 |
| --- | --- |
| `figures/01_ridgeline.png` | 山脊图 |
| `figures/02_linear_heatmap.png` | 线型热图 |
| `figures/03_square_heatmap.png` | 方块热图 |
| `figures/04_3d_stacked_bar.png` | 三维堆叠柱 |
| `figures/05_wind_rose.png` | 风玫瑰 |
| `figures/06_radar.png` | 雷达图 |
| `figures/07_linear_heatmap_series.png` | 序列线型热图 |
| `figures/08_cluster_scatter.png` | 聚类散点 |
| `figures/09_pattern_bar.png` | 纹理柱状图 |
| `figures/10_3d_cluster_scatter.png` | 三维聚类散点 |
| `figures/11_square_heatmap_2.png` | 方块热图 2 |
| `figures/12_inset_zoom.png` | 局部放大 |
| `figures/13_pattern_stacked.png` | 纹理堆叠柱 |
| `figures/14_corr_square_heatmap.png` | 相关方块热图 |
| `figures/15_waffle.png` | 华夫图 |
| `figures/16_3d_filled_line.png` | 三维填充折线 |
| `figures/17_bubble_heatmap.png` | 气泡热图 |
| `figures/18_corr_bubble.png` | 相关气泡热图（Mantel 连接臂） |
| `figures/18_corr_bubble_dendrogram.png` | 相关气泡 + 聚类树 |
| `figures/19_triangle_bubble.png` | 三角气泡热图 |
| `figures/20_3d_bar_labels.png` | 带标签三维柱 |
| `figures/21_triangle_square.png` | 三角方块热图 |
| `figures/22_unequal_width_bar.png` | 不等宽柱 |
| `figures/23_density_scatter.png` | 密度散点 |
| `figures/24_floating_bar.png` | 悬浮柱 |
| `figures/25_3d_density_scatter.png` | 三维密度散点 |
| `figures/26_bidirectional_stacked.png` | 双向堆叠 |
| `figures/27_horizontal_bidirectional.png` | 水平双向堆叠 |
| `figures/28_3d_cluster_regression.png` | 三维聚类 + 回归面 |
| `figures/29_triangle_heatmap.png` | 三角热图 |
| `figures/30_streamgraph.png` | 冲击图 / 流图 |
| `figures/31_sankey.png` | 桑基图 |
| `figures/32_violin.png` | 小提琴图 |
| `figures/33_dual_surface.png` | 双曲面 |
| `figures/34_dual_triangle_heatmap.png` | 双三角热图 |
| `figures/35_dual_feature_3d_scatter.png` | 双特征三维散点 |
| `figures/36_dual_grid_surface.png` | 双网格曲面 |

---

## 5. 使用注意

1. **三维柱在论文里要谨慎。** 透视会让高低比较变难，二维分组柱通常更清晰。  
2. **相关矩阵不要重复编码。** 方块、气泡、三角已经够用；双三角适合「上三角相关系数、下三角 p 值 / 另一矩阵」。  
3. **华夫图和饼图一样，类别不宜太多。** 案例限制在 6 类、100 格。  
4. **风玫瑰** 若做气象论文，优先 `openair::windRose()`，它按规范处理 `ws`/`wd` 和百分频率圈。  
5. **桑基 vs 冲积图。** `ggalluvial` 更偏分类频率冲积；节点–链接式桑基可用 `networkD3` 或 `ggsankey`。  
6. 若需要与 `plot3D` 官方三维柱完全一致，先安装 [XQuartz](https://www.xquartz.org)，再改脚本调用 `hist3D` / `box3D`。

---

## 7. 迭代记录

脚本见 `scripts/redraw_misaligned.R`、`scripts/fix_heatmaps.R`。

| 编号 | 问题 | 修正后 |
| --- | --- | --- |
| 01 山脊图 | 只有 3 条 iris 密度 | 12 条重叠彩色脊线 |
| 02 线型热图 | 画成了二维连续场 | 每条序列一条细热力色带 |
| 04 三维堆叠柱 | 等轴测立方体、无坐标盒 | `persp` + `trans3d` 三维柱，带坐标轴 |
| 08 聚类散点 | 6 个稀疏高斯团 | 铺满画幅的密集聚类点 + 矩形空洞 |
| 10 三维聚类 | 4 个简单团 | 密集着色点构成地形 |
| 12 局部放大 | `facet_zoom` 上下分面 | 同一张图里的 inset 放大框 |
| 16 三维填充折线 | `persp` 连成一张曲面 | 各序列独立的 3D 填充墙 |
| 20 带标签三维柱 | 无坐标盒 | 三维柱顶标注数值 |
| 22 不等宽柱 | Marimekko（宽度=份额） | 基线上少数不等宽柱 |
| 24 悬浮柱 | 月份区间柱、顺序乱 | 拱形悬浮柱（不从 0 起） |
| 25 三维密度 | 三维散点 | `kde2d` 密度曲面 |
| 26 / 27 双向堆叠 | 矩形 Likert / 金字塔 | 菱形 / 小提琴形堆叠条 |
| 28 | 画成了三维回归面 | 二维着色散点 + 回归线 |
| 30 冲击图 | 中心化流图 | 从基线堆叠的填充柱 |
| 34 双三角 | 红蓝 vs 黄绿双色标 | 两侧同一青绿顺序色标、白色对角线 |
| 11 方块热图 2 | 无行列名 → 整块青绿、坐标为 NA | 命名 10×10 相关矩阵，RdBu 留缝 |
| 17 气泡热图 | 稀疏彩虹气泡 | 单元格网格 + 紫橙顺序色、大小随数值 |
| 18 相关性气泡 | 只有红蓝圆点，没有连接臂 | `linkET::geom_couple()` Mantel 连接臂；另存聚类树版 `18_corr_bubble_dendrogram.png` |
| 03 方块热图 | 平滑靶心 | 马赛克色块 + 白缝 |
| 07 序列热图 | `geom_tile` 棋盘伪影 | `geom_raster()` |
| 14 相关方块 | 红蓝 `ggcorrplot` | 紫绿 PRGn、hclust 排序 |
| 19 三角气泡 | 无单元格网格 | 下三角网格 + 变尺寸气泡 |
| 21 三角方块 | 红蓝发散 | 青绿顺序色下三角 |
| 29 三角热图 | 格内挤满数字 | RdBu 留缝、不标系数 |

---

## 6. 参考链接（检索用）

- ggplot2 矩形/热图：<https://ggplot2.tidyverse.org/reference/geom_tile.html>  
- ggplot2 小提琴：<https://ggplot2.tidyverse.org/reference/geom_violin.html>  
- ggridges：<https://rdrr.io/cran/ggridges/man/geom_density_ridges.html>  
- ggpattern：<https://cran.r-project.org/package=ggpattern>  
- ggcorrplot：<https://cran.r-project.org/web/packages/ggcorrplot/vignettes/publication-ready-correlation-plots.html>  
- fmsb 雷达图：<https://search.r-project.org/CRAN/refmans/fmsb/html/radarchart.html>  
- openair 风玫瑰：<https://openair-project.github.io/book/sections/directional-analysis/wind-roses.html>  
- ggalluvial：<https://corybrunson.github.io/ggalluvial/>  
- ggstream：<https://search.r-project.org/CRAN/refmans/ggstream/html/geom_stream.html>  
- waffle：<https://search.r-project.org/CRAN/refmans/waffle/html/waffle.html>  
- plot3D：<https://cran.r-project.org/package=plot3D>  
- Hiplot 三维柱教程（R + plot3D）：<https://openbiox.github.io/Bizard/Hiplot/003-barplot-3d.html>  
- plotly 三维散点：<https://plotly.com/r/3d-scatter-plots/>  
- 双色标三角热图：<https://stackoverflow.com/questions/71342643/single-heatmap-on-two-symetric-matrices-with-different-colours-and-scales-r>  
- linkET Mantel 连接臂：<https://github.com/Hy4m/linkET>  
- ComplexHeatmap：<https://jokergoo.github.io/ComplexHeatmap-reference/book/>  
- ggplot2 其他 geom 综述（流图、山脊、桑基、华夫）：<https://ivelasq.rbind.io/blog/other-geoms/>
