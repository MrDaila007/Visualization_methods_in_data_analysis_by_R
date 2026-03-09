# =============================================================================
# Lab 5: Create the Ugliest Plot Possible!
# Group 4, variant 5
# =============================================================================

library(conflicted)
library(tidyverse)

conflicts_prefer(dplyr::filter, dplyr::select)

# --- Load data ---
library(palmerpenguins)
data(penguins)
penguins_clean <- penguins |> drop_na()

# =============================================================================
# THE UGLIEST PLOT POSSIBLE
# Incorporating as many misleaders and bad practices as possible:
#
# From CALVI's 11 misleaders:
# 1. Truncated axis - y-axis doesn't start at 0
# 2. Inverted axis - reversed scale
# 3. Aspect ratio - distorted proportions
# 4. Dual axis - two unrelated y-axes
# 5. Area as length - misleading bubble sizes
# 6. Color misuse - rainbow + non-intuitive mapping
# 7. 3D effects (simulated) - unnecessary decoration
# 8. Missing labels / misleading labels
# 9. Cherry-picking data
# 10. Inappropriate chart type
# 11. Overplotting / clutter
# =============================================================================

ugly_plot <- ggplot(penguins_clean, aes(x = bill_length_mm, y = body_mass_g)) +

  # Use inappropriate geom (line for non-sequential data)
  geom_line(aes(color = flipper_length_mm), linewidth = 2, alpha = 0.5) +

  # Add points with terrible aesthetics
  geom_point(
    aes(size = bill_depth_mm, shape = species, fill = island),
    color = "yellow", stroke = 3, alpha = 0.6
  ) +

  # Rainbow color scale (worst for perception)
  scale_color_gradientn(colors = rainbow(20)) +
  scale_fill_manual(values = c("red", "lime green", "magenta")) +

  # Terrible shapes
  scale_shape_manual(values = c(24, 25, 23)) +

  # Inverted and truncated y-axis (misleader!)
  scale_y_reverse(limits = c(6500, 2500)) +

  # Distorted aspect ratio
  coord_fixed(ratio = 0.005) +

  # Excessive and misleading labels
  labs(
    title = "AMAZING PENGUIN ANALYSIS!!!",
    subtitle = "This proves that penguins are DEFINITELY getting bigger (p < 0.001)*",
    x = "Length of the Bill in Millimeters (measured from the tip to base including\nthe cere region as defined by the Palmer Station protocol v2.3)",
    y = "WEIGHT",
    caption = "*not actually tested\nSource: probably the internet\nCreated with love and MS Paint energy",
    color = "Wing\nThingy",
    fill = "Place",
    size = "Bill\nWide",
    shape = "Type\nof\nBird"
  ) +

  # Horrific theme
  theme(
    # Background colors
    plot.background = element_rect(fill = "yellow", color = "red", linewidth = 5),
    panel.background = element_rect(fill = "lavender"),
    panel.grid.major = element_line(color = "red", linewidth = 1.5, linetype = "dotdash"),
    panel.grid.minor = element_line(color = "orange", linewidth = 1),

    # Title formatting
    plot.title = element_text(
      family = "serif", size = 28, color = "purple",
      face = "bold.italic", hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 12, color = "red", face = "italic", hjust = 0.5
    ),

    # Axis formatting
    axis.title.x = element_text(size = 8, color = "blue", angle = -5),
    axis.title.y = element_text(size = 22, color = "darkgreen", face = "bold"),
    axis.text.x = element_text(size = 14, color = "red", angle = 60, hjust = 1),
    axis.text.y = element_text(size = 6, color = "gray80"),

    # Legend mess
    legend.background = element_rect(fill = "pink", color = "green", linewidth = 2),
    legend.key = element_rect(fill = "lightyellow"),
    legend.title = element_text(face = "bold", color = "purple", size = 8),
    legend.text = element_text(size = 6, color = "brown"),
    legend.position = "left",

    # Caption
    plot.caption = element_text(size = 7, color = "gray60", face = "italic")
  ) +

  # Add unnecessary annotation
  annotate("text", x = 50, y = 4000, label = "WOW!",
           size = 20, color = "red", alpha = 0.3, angle = 30,
           fontface = "bold") +
  annotate("text", x = 40, y = 5500, label = "LOOK HERE -->",
           size = 8, color = "blue", fontface = "bold.italic") +
  annotate("rect", xmin = 45, xmax = 55, ymin = 3000, ymax = 4500,
           fill = "yellow", alpha = 0.3, color = "red", linewidth = 2)

ugly_plot
ggsave("Lab5/ugliest_plot.png", width = 14, height = 10, dpi = 150)

# =============================================================================
# BAD PRACTICES CATALOG (with individual examples)
# =============================================================================

# 1. Truncated Y-axis (misleading bar chart)
set.seed(42)
bad_data <- tibble(
  category = c("Product A", "Product B"),
  sales = c(98, 102)
)

p_bad1 <- ggplot(bad_data, aes(category, sales, fill = category)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(95, 105)) +  # Truncated!
  labs(title = "MISLEADING: Truncated Y-axis",
       subtitle = "Looks like 5x difference, actually ~4%") +
  scale_fill_manual(values = c("coral", "steelblue")) +
  theme_minimal()

p_good1 <- ggplot(bad_data, aes(category, sales, fill = category)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 110)) +  # Proper scale
  labs(title = "CORRECT: Y-axis from 0",
       subtitle = "True proportion visible") +
  scale_fill_manual(values = c("coral", "steelblue")) +
  theme_minimal()

p_bad1 + p_good1
ggsave("Lab5/misleader_truncated_axis.png", width = 10, height = 5)

# 2. Pie chart vs bar chart
pie_data <- tibble(
  category = LETTERS[1:6],
  value = c(23, 21, 19, 17, 12, 8)
)

p_bad2 <- ggplot(pie_data, aes(x = "", y = value, fill = category)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "MISLEADING: Pie Chart",
       subtitle = "Hard to compare similar values") +
  theme_void()

p_good2 <- ggplot(pie_data, aes(x = reorder(category, value), y = value,
                                fill = category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "BETTER: Ordered Bar Chart",
       subtitle = "Easy to compare values", x = "", y = "Value") +
  theme_minimal()

p_bad2 + p_good2
ggsave("Lab5/misleader_pie_vs_bar.png", width = 12, height = 5)

# 3. Rainbow colormap vs perceptually uniform
p_bad3 <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_fill_gradientn(colors = rainbow(7)) +
  labs(title = "BAD: Rainbow colormap") +
  theme_minimal()

p_good3 <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "GOOD: Viridis (perceptually uniform)") +
  theme_minimal()

p_bad3 + p_good3
ggsave("Lab5/misleader_rainbow_vs_viridis.png", width = 12, height = 5)

# =============================================================================
# CHART MAKEOVER: From ugly to beautiful
# =============================================================================

# Before (ugly)
p_before <- ggplot(penguins_clean, aes(body_mass_g, flipper_length_mm)) +
  geom_point(color = "red", size = 5, shape = 17) +
  labs(title = "penguin data") +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    panel.background = element_rect(fill = "lightblue"),
    panel.grid = element_line(color = "red")
  )

# After (clean)
p_after <- ggplot(penguins_clean, aes(body_mass_g, flipper_length_mm,
                                      color = species)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Penguin Body Mass vs Flipper Length",
    subtitle = "Clear species-specific patterns with linear trends",
    x = "Body Mass (g)",
    y = "Flipper Length (mm)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_before + p_after +
  plot_annotation(title = "Chart Makeover: Before & After")
ggsave("Lab5/chart_makeover.png", width = 14, height = 6)

cat("\n=== Lab 5 complete! ===\n")
cat("The ugliest plot and misleader examples created.\n")
