# =============================================================================
# Lab 6: ggplot Crossword + Cedric Scherer Visualization Recreation
# Group 4 - Foodborne Illness Pathogens (USAFacts)
# =============================================================================

library(conflicted)
library(tidyverse)

conflicts_prefer(dplyr::filter, dplyr::select)

# =============================================================================
# Part 1: Recreate the USAFacts Foodborne Illness Chart
# "Norovirus is the top pathogen contributing to foodborne illness,
#  according to CDC estimates."
# =============================================================================

# --- Load data ---
pathogens <- read_csv("data-I2Rsw.csv")
glimpse(pathogens)

# Rename columns for easier use
pathogens <- pathogens |>
  rename(
    pathogen = Pathogen,
    illnesses = Illnesses,
    low = `Low Estimate`,
    high = `High estimate`
  ) |>
  mutate(
    pathogen = fct_reorder(pathogen, illnesses),
    # Format labels
    illnesses_label = case_when(
      illnesses >= 1e6 ~ paste0(round(illnesses / 1e6, 1), "M"),
      illnesses >= 1e3 ~ paste0(round(illnesses / 1e3, 1), "K"),
      TRUE ~ as.character(illnesses)
    ),
    low_label = case_when(
      low >= 1e6 ~ paste0(round(low / 1e6, 1), "M"),
      low >= 1e3 ~ paste0(round(low / 1e3, 1), "K"),
      TRUE ~ as.character(low)
    ),
    high_label = case_when(
      high >= 1e6 ~ paste0(round(high / 1e6, 1), "M"),
      high >= 1e3 ~ paste0(round(high / 1e3, 1), "K"),
      TRUE ~ as.character(high)
    )
  )

# --- Recreate the chart ---
# USAFacts style: horizontal lollipop/dumbbell chart with range
usafacts_orange <- "#E87722"
usafacts_gray <- "#666666"
usafacts_lightgray <- "#D9D9D9"

p_main <- ggplot(pathogens, aes(y = pathogen)) +

  # Range line (low to high estimate)
  geom_segment(
    aes(x = low, xend = high, yend = pathogen),
    color = usafacts_lightgray, linewidth = 2
  ) +

  # Low estimate point
  geom_point(aes(x = low), color = usafacts_orange, size = 3) +

  # Midrange estimate point (main illness count)
  geom_point(aes(x = illnesses), color = usafacts_orange, size = 5) +

  # High estimate point
  geom_point(aes(x = high), color = usafacts_orange, size = 3) +

  # Labels for low estimates
  geom_text(
    aes(x = low, label = low_label),
    hjust = 1.3, size = 3, color = usafacts_gray
  ) +

  # Labels for midrange
  geom_text(
    aes(x = illnesses, label = illnesses_label),
    vjust = -1.2, size = 3, color = usafacts_gray, fontface = "bold"
  ) +

  # Labels for high estimates
  geom_text(
    aes(x = high, label = high_label),
    hjust = -0.3, size = 3, color = usafacts_gray
  ) +

  # Scale and formatting
  scale_x_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 10e6, 2e6),
    limits = c(-500000, 11000000),
    expand = c(0, 0)
  ) +

  labs(
    title = "Norovirus is the top pathogen contributing to foodborne\nillness, according to CDC estimates.",
    subtitle = "Estimated ranges of annual foodborne illnesses from the top six\npathogens, 2019 CDC study",
    x = "",
    y = "",
    caption = "Sorted by CDC midrange estimate. Estimate ranges are 90%\ncredible intervals.\nSource: Centers for Disease Control and Prevention"
  ) +

  # Header annotations for Low/High
  annotate("text", x = pathogens$low[pathogens$pathogen == "Norovirus"] - 200000,
           y = 6.7, label = "Low Estimate", size = 3, color = usafacts_gray,
           fontface = "italic") +
  annotate("text", x = pathogens$high[pathogens$pathogen == "Norovirus"] + 200000,
           y = 6.7, label = "High estimate", size = 3, color = usafacts_gray,
           fontface = "italic") +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "black",
                              lineheight = 1.2),
    plot.subtitle = element_text(size = 10, color = usafacts_gray,
                                 margin = margin(t = 5, b = 15)),
    plot.caption = element_text(size = 8, color = usafacts_gray, hjust = 0,
                                lineheight = 1.3),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 9, color = usafacts_gray),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#EDEDED"),
    plot.margin = margin(20, 20, 20, 10)
  )

p_main
ggsave("Lab6/foodborne_illness_recreation.png", width = 10, height = 6, dpi = 300)

# =============================================================================
# Part 2: Record GIF with camcorder (demonstration)
# =============================================================================

# install.packages("camcorder")
# library(camcorder)
#
# gg_record(
#   dir = "Lab6/recording",
#   device = "png",
#   width = 10,
#   height = 6,
#   units = "in",
#   dpi = 300
# )
#
# # Step 1: Basic structure
# ggplot(pathogens, aes(y = pathogen, x = illnesses)) +
#   geom_point() +
#   theme_minimal()
#
# # Step 2: Add range
# ggplot(pathogens, aes(y = pathogen)) +
#   geom_segment(aes(x = low, xend = high, yend = pathogen),
#                color = "gray80", linewidth = 2) +
#   geom_point(aes(x = illnesses), color = usafacts_orange, size = 5) +
#   theme_minimal()
#
# # Step 3: Add styling
# p_main  # Final version
#
# gg_playback(
#   name = "Lab6/build_animation.gif",
#   first_image_duration = 2,
#   last_image_duration = 5,
#   frame_duration = 1,
#   image_resize = 800
# )

# =============================================================================
# Part 3: Alternative visualizations of the same data
# =============================================================================

# Bar chart version
ggplot(pathogens, aes(x = pathogen, y = illnesses, fill = pathogen)) +
  geom_col(show.legend = FALSE, fill = usafacts_orange, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.3, color = usafacts_gray
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Foodborne Illness Estimates with Uncertainty Ranges",
    subtitle = "Error bars show 90% credible intervals",
    x = "", y = "Estimated Annual Illnesses"
  ) +
  theme_minimal()

ggsave("Lab6/foodborne_bar_alternative.png", width = 10, height = 5, dpi = 300)

# Dot plot with error bars
ggplot(pathogens, aes(x = illnesses, y = pathogen)) +
  geom_linerange(aes(xmin = low, xmax = high), color = "gray70", linewidth = 1) +
  geom_point(size = 4, color = usafacts_orange) +
  scale_x_log10(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  labs(
    title = "Foodborne Illness Estimates (Log Scale)",
    subtitle = "Log scale reveals the enormous range differences between pathogens",
    x = "Estimated Annual Illnesses (log scale)", y = ""
  ) +
  theme_minimal()

ggsave("Lab6/foodborne_logscale.png", width = 10, height = 5, dpi = 300)

cat("\n=== Lab 6 complete! ===\n")
