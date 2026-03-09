# Visualization Methods in Data Analysis by R

Laboratory works portfolio for the course "Visualization Methods in Data Analysis by R" (2026).

**Group 4, Variant 5**

## Labs

| Lab | Topic | Key Packages |
|-----|-------|-------------|
| Lab 0 | R Setup, tidyverse basics | ggplot2, patchwork, purrr |
| Lab 1 | Exercises from *Graphical Data Analysis with R* (Ch. 3-11) | lawstat, vcdExtra, Sleuth2, mfp, UsingR, vcd |
| Lab 2 | Automated Graphical EDA (autoGEDA) | dlookr, naniar, GGally |
| Lab 3 | GEDA + 12 Analytic Questions | palmerpenguins |
| Lab 4 | Heatmaps + Table Visualization | pheatmap, gplots, kableExtra, formattable |
| Lab 5 | The Ugliest Plot + Visualization Misleaders | ggplot2 |
| Lab 6 | Chart Recreation (USAFacts Foodborne Illness) | scales, camcorder |
| Lab 7 | Munzner Foundations 1: Data Abstraction | - |
| Lab 8 | Munzner Foundations 2: Marks & Channels | gt |
| Lab 9 | Complexity Families + Color | colorspace, RColorBrewer |
| Lab 10 | Final Portfolio | all of the above |

## Structure

```
Lab*/
  task.md          # Task description
  Lab*.R           # R script (Labs 0-6)
  Lab*_report.Rmd  # R Markdown report (all labs)
  Lab*_report.pdf  # Rendered PDF
  Lab*_answers.md  # Theory notes (Labs 7-9)
```

## Data Files

- `london-bikes.csv` -- London bike sharing dataset
- `data-I2Rsw.csv` -- CDC foodborne illness pathogens (Lab 6)
- `jackalope.csv`, `week3.csv` -- supplementary datasets

## How to Render

```r
# Set user library and pandoc path if needed
Sys.setenv(R_LIBS_USER = "~/R/x86_64-pc-linux-gnu-library/4.1")

# Render a single report
rmarkdown::render("Lab4/Lab4_report.Rmd")
```

## References

- Unwin, A. *Graphical Data Analysis with R*
- Munzner, T. *Visualization Analysis and Design*
- Lambert, N. & Zanin, C. *Practical Handbook of Thematic Cartography*
- [Datawrapper Blog: Color Scales](https://blog.datawrapper.de/interpolation-for-color-scales-and-maps/)
- [TidyTuesday](https://github.com/rfordatascience/tidytuesday)
