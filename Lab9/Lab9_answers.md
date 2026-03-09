# Lab 9: Complexity Families + Color

## Format: Complete the worksheets from Google Drive

## Part 1: Complexity Families (Munzner)

### Notation complexity families:
- **Tables**: flat table, multidimensional table
- **Networks**: trees, general networks
- **Spatial**: fields (continuous), geometry (spatial data)
- **Sets**: lists, clusters
- **Clusters and partitions**: groupings

### For each dataset, identify:
1. What type of dataset is it? (table/network/spatial/set)
2. What are the items and attributes?
3. What derived data might be useful?

## Part 2: Color

### Color spaces:
- **RGB**: device-oriented (not perceptually uniform)
- **HSL/HSV**: intuitive but not perceptually uniform
- **CIELAB/HCL**: perceptually uniform (preferred for visualization)

### Color palette types:
1. **Sequential**: low → high (one hue, varying luminance)
   - Use for: ordered data, quantities
   - Example: viridis, plasma, Blues

2. **Diverging**: low ← neutral → high (two hues, varying luminance)
   - Use for: data with meaningful midpoint (e.g., 0, average)
   - Example: RdBu, PiYG, coolwarm

3. **Qualitative**: distinct categories (different hues, similar luminance)
   - Use for: nominal categorical data
   - Example: Set2, Dark2, Paired

### Color accessibility:
- ~8% of males, ~0.5% of females have color vision deficiency
- Always check with colorblind simulators
- Avoid red-green as sole distinguishing colors
- Use redundant encodings (shape + color)

### R packages for color:
```r
library(colorspace)
library(paletteer)
library(colorBlindness)

# Analyze a palette
specplot(hcl.colors(9, "viridis"))
specplot(hcl.colors(9, "RdBu"))

# Simulate color blindness
cvdPlot(your_plot)

# Browse palettes
paletteer::palettes_d_names
hcl_palettes(plot = TRUE)
```

### Reference:
- How to find & create good color palettes: https://blog.datawrapper.de/create-good-color-palettes/
