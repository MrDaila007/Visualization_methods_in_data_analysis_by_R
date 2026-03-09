# =============================================================================
# Lab 4: Build a Heat Map + Table Visualization
# Group 4, variant 5
# =============================================================================

library(conflicted)
library(tidyverse)
library(patchwork)

conflicts_prefer(dplyr::filter, dplyr::select)

# --- 1. Load TidyTuesday dataset ---
# Using a dataset suitable for heatmaps
# Historical monthly temperature data or similar matrix-like data

# Option: use built-in datasets
data(airquality)

aq <- airquality |>
  mutate(
    Date = as.Date(paste(1973, Month, Day, sep = "-")),
    Month_name = month.abb[Month]
  ) |>
  drop_na(Ozone)

# =============================================================================
# HEATMAP 1: ggplot2 geom_tile()
# =============================================================================

# Heatmap of Ozone levels by Day and Month
ggplot(airquality, aes(x = Day, y = factor(Month, labels = month.abb[5:9]),
                       fill = Ozone)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90",
                       name = "Ozone\n(ppb)") +
  labs(
    title = "Heatmap: Daily Ozone Levels (New York, 1973)",
    subtitle = "Built with ggplot2::geom_tile()",
    x = "Day of Month", y = "Month"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggsave("Lab4/heatmap_geom_tile.png", width = 12, height = 5)

# =============================================================================
# HEATMAP 2: Base R heatmap()
# =============================================================================

# Prepare matrix data
aq_matrix <- airquality |>
  select(Ozone, Solar.R, Wind, Temp) |>
  drop_na() |>
  as.matrix() |>
  scale()

png("Lab4/heatmap_base.png", width = 800, height = 600)
heatmap(
  aq_matrix[1:50, ],
  col = hcl.colors(50, "YlOrRd", rev = TRUE),
  scale = "none",
  main = "Base R heatmap() - Air Quality (first 50 observations)"
)
dev.off()

# =============================================================================
# HEATMAP 3: pheatmap
# =============================================================================

# install.packages("pheatmap")
library(pheatmap)

png("Lab4/heatmap_pheatmap.png", width = 900, height = 700)
pheatmap(
  aq_matrix[1:50, ],
  color = hcl.colors(50, "Viridis"),
  main = "pheatmap - Air Quality Data (scaled)",
  fontsize = 10,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_rownames = FALSE
)
dev.off()

# =============================================================================
# HEATMAP 4: Correlation heatmap with ggplot2
# =============================================================================

cor_matrix <- airquality |>
  select(Ozone, Solar.R, Wind, Temp) |>
  drop_na() |>
  cor()

cor_long <- cor_matrix |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 5) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "coral",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Correlation Heatmap - Air Quality Variables",
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12)
  ) +
  coord_fixed()

ggsave("Lab4/heatmap_correlation.png", width = 7, height = 6)

# =============================================================================
# HEATMAP 5: Calendar heatmap (Temperature)
# =============================================================================

aq_cal <- airquality |>
  mutate(
    Date = as.Date(paste(1973, Month, Day, sep = "-")),
    week = as.numeric(format(Date, "%U")),
    wday = factor(weekdays(Date, abbreviate = TRUE),
                  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

ggplot(aq_cal, aes(x = week, y = fct_rev(wday), fill = Temp)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(option = "inferno", name = "Temp (°F)") +
  labs(
    title = "Calendar Heatmap: Temperature (New York, May-Sep 1973)",
    x = "Week of Year", y = ""
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggsave("Lab4/heatmap_calendar.png", width = 12, height = 4)

# =============================================================================
# TABLE VISUALIZATION with gt
# =============================================================================

# install.packages(c("gt", "gtExtras"))
library(gt)
library(gtExtras)

# Summary table
aq_summary <- airquality |>
  group_by(Month = month.abb[Month]) |>
  summarise(
    `Mean Ozone` = round(mean(Ozone, na.rm = TRUE), 1),
    `Mean Temp` = round(mean(Temp), 1),
    `Mean Wind` = round(mean(Wind), 1),
    `Mean Solar` = round(mean(Solar.R, na.rm = TRUE), 1),
    Days = n(),
    .groups = "drop"
  )

aq_summary |>
  gt() |>
  tab_header(
    title = "New York Air Quality Summary (1973)",
    subtitle = "Monthly averages for May through September"
  ) |>
  data_color(
    columns = `Mean Ozone`,
    palette = c("white", "coral")
  ) |>
  data_color(
    columns = `Mean Temp`,
    palette = c("steelblue", "coral")
  ) |>
  cols_align(align = "center") |>
  gt_theme_538()

# Note: gt tables render in HTML/RStudio viewer, not as PNG directly
# Use gtsave() to export: gtsave(table, "Lab4/table_summary.html")

# =============================================================================
# TABLE with sparklines (gtExtras)
# =============================================================================

aq_spark <- airquality |>
  group_by(Month = month.abb[Month]) |>
  summarise(
    Ozone_vals = list(na.omit(Ozone)),
    Temp_vals = list(Temp),
    Wind_vals = list(Wind),
    `Avg Ozone` = round(mean(Ozone, na.rm = TRUE), 1),
    `Avg Temp` = round(mean(Temp), 1),
    .groups = "drop"
  )

aq_spark |>
  gt() |>
  gt_plt_sparkline(Ozone_vals, label = FALSE) |>
  gt_plt_sparkline(Temp_vals, label = FALSE) |>
  gt_plt_sparkline(Wind_vals, label = FALSE) |>
  tab_header(
    title = "Air Quality with Sparklines",
    subtitle = "Daily distributions shown as inline charts"
  ) |>
  gt_theme_538()

# --- Color interpolation note ---
# https://blog.datawrapper.de/interpolation-for-color-scales-and-maps/
# Key choices: sequential vs diverging vs qualitative
# For heatmaps: sequential (viridis, plasma, inferno) or diverging (RdBu)
# Always consider colorblind-friendly palettes

cat("\n=== Lab 4 complete! ===\n")
cat("5 heatmap variants + 2 table visualizations created.\n")
