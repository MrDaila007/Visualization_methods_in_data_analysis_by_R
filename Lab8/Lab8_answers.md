# Lab 8: Tamara Munzner - Foundations 2

## Format: PDF (fill in the Answer Template from Google Drive)

## Section 1: Generate - Two Numbers

Given two numbers, design a visualization. Consider:
- What **marks** to use (points, lines, areas)
- What **channels** to encode data (position, color, size, shape)
- The **expressiveness** principle: encode all and only the data
- The **effectiveness** principle: use the most effective channels first

### Channel effectiveness ranking (Munzner):
1. Position on common scale (most effective)
2. Position on unaligned scale
3. Length
4. Tilt/Angle
5. Area
6. Color luminance
7. Color saturation
8. Curvature
9. Volume

## Section 2: Marks & Channels Analysis

For each given visualization:
1. Identify the **marks** (geometric primitives: points, lines, areas)
2. Identify the **channels** (visual variables encoding data)
3. Assess effectiveness using Munzner's principles

### Key principles:
- **Expressiveness**: The visual encoding should express all of the information in the dataset, and only that information
- **Effectiveness**: The most important attributes should be encoded with the most effective channels

## Section 3: Redesign - New Zealand Table

Redesign a table visualization:
1. Identify what data is being shown
2. What tasks does the reader need to perform?
3. What visual encodings would be more effective?

### Approach:
- Replace text-heavy tables with visual encodings where possible
- Use position (bar charts) instead of text for quantitative values
- Keep text for categorical labels
- Add color coding for quick scanning
- Consider sorting by a key metric

### Example redesign in R:
```r
library(gt)
library(gtExtras)

# Transform table data into a gt table with:
# - Conditional formatting (data_color)
# - Inline bar charts (gt_plt_bar)
# - Sparklines for trends (gt_plt_sparkline)
```

## Reference
- 1 dataset, 100 visualizations: https://100.datavizproject.com/
