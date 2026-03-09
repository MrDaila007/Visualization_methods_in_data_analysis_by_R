# =============================================================================
# Lab 3: GEDA (Graphical Exploratory Data Analysis) + Analytic Questions
# Group 4, variant 5
# =============================================================================

library(conflicted)
library(tidyverse)
library(patchwork)

conflicts_prefer(dplyr::filter, dplyr::select)

# --- 1. Load TidyTuesday dataset ---
# Using Palmer Penguins (popular TidyTuesday dataset)
library(palmerpenguins)

data(penguins)
penguins_clean <- penguins |> drop_na()
glimpse(penguins_clean)

# =============================================================================
# GEDA: Graphical Exploratory Data Analysis
# =============================================================================

# --- Overview of distributions ---
# Continuous variables
p_bill_len <- ggplot(penguins_clean, aes(bill_length_mm)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  theme_minimal() + labs(title = "Bill Length")

p_bill_dep <- ggplot(penguins_clean, aes(bill_depth_mm)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  theme_minimal() + labs(title = "Bill Depth")

p_flip <- ggplot(penguins_clean, aes(flipper_length_mm)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  theme_minimal() + labs(title = "Flipper Length")

p_mass <- ggplot(penguins_clean, aes(body_mass_g)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  theme_minimal() + labs(title = "Body Mass")

(p_bill_len + p_bill_dep) / (p_flip + p_mass)
ggsave("Lab3/distributions_overview.png", width = 10, height = 8)

# Categorical variables
p_species <- ggplot(penguins_clean, aes(species, fill = species)) +
  geom_bar(show.legend = FALSE) + theme_minimal()

p_island <- ggplot(penguins_clean, aes(island, fill = island)) +
  geom_bar(show.legend = FALSE) + theme_minimal()

p_sex <- ggplot(penguins_clean, aes(sex, fill = sex)) +
  geom_bar(show.legend = FALSE) + theme_minimal()

p_species + p_island + p_sex
ggsave("Lab3/categorical_overview.png", width = 12, height = 4)

# =============================================================================
# Analytic Questions - One of each type
# =============================================================================

# --- ELEMENTARY QUESTIONS ---

# 1. IDENTIFY: What is the average body mass of Gentoo penguins?
gentoo_mass <- penguins_clean |>
  filter(species == "Gentoo") |>
  summarise(mean_mass = mean(body_mass_g))

ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  geom_hline(yintercept = gentoo_mass$mean_mass, linetype = "dashed", color = "red") +
  annotate("text", x = 3.3, y = gentoo_mass$mean_mass + 100,
           label = paste0("Gentoo mean: ", round(gentoo_mass$mean_mass), "g"),
           color = "red", size = 3.5) +
  labs(
    title = "IDENTIFY: What is the average body mass of Gentoo penguins?",
    subtitle = paste0("Answer: ", round(gentoo_mass$mean_mass), " grams"),
    x = "Species", y = "Body Mass (g)"
  ) +
  theme_minimal()

ggsave("Lab3/Q1_identify.png", width = 8, height = 6)

# 2. LOCATE: Where (on which island) are Chinstrap penguins found?
ggplot(penguins_clean, aes(x = island, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "LOCATE: Where are Chinstrap penguins found?",
    subtitle = "Answer: Only on Dream island",
    x = "Island", y = "Count"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

ggsave("Lab3/Q2_locate.png", width = 8, height = 6)

# 3. COMPARE: Do males have longer flippers than females?
ggplot(penguins_clean, aes(x = sex, y = flipper_length_mm, fill = sex)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  labs(
    title = "COMPARE: Do males have longer flippers than females?",
    subtitle = "Answer: Yes, males have consistently longer flippers",
    x = "Sex", y = "Flipper Length (mm)"
  ) +
  scale_fill_manual(values = c("coral", "steelblue")) +
  theme_minimal()

ggsave("Lab3/Q3_compare.png", width = 8, height = 6)

# 4. RANK: Which species has the largest body mass?
species_mass <- penguins_clean |>
  group_by(species) |>
  summarise(median_mass = median(body_mass_g)) |>
  arrange(desc(median_mass))

ggplot(penguins_clean, aes(x = reorder(species, body_mass_g, FUN = median),
                           y = body_mass_g, fill = species)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  labs(
    title = "RANK: Which species is heaviest?",
    subtitle = "Answer: Gentoo > Chinstrap > Adelie",
    x = "Species (ordered by median mass)", y = "Body Mass (g)"
  ) +
  theme_minimal()

ggsave("Lab3/Q4_rank.png", width = 8, height = 6)

# 5. CONNECT: Are bill length and bill depth related?
ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "CONNECT: Are bill length and depth related?",
    subtitle = "Answer: Within species, yes (positive). Overall, Simpson's paradox (negative).",
    x = "Bill Length (mm)", y = "Bill Depth (mm)"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

ggsave("Lab3/Q5_connect.png", width = 8, height = 6)

# 6. DISTINGUISH: What makes Gentoo penguins different from others?
library(GGally)

penguins_clean |>
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) |>
  ggpairs(
    columns = 2:5,
    aes(color = species, alpha = 0.5),
    upper = list(continuous = wrap("cor", size = 3)),
    lower = list(continuous = wrap("points", size = 1))
  ) +
  labs(title = "DISTINGUISH: What makes Gentoo different?") +
  theme_minimal()

ggsave("Lab3/Q6_distinguish.png", width = 10, height = 10)

# --- SYNOPTIC QUESTIONS ---

# 7. GROUP: Do penguin species form distinct clusters?
ggplot(penguins_clean, aes(x = bill_length_mm, y = flipper_length_mm,
                           color = species, shape = species)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(level = 0.95) +
  labs(
    title = "GROUP: Do species form distinct clusters?",
    subtitle = "Answer: Yes, three clear clusters with 95% confidence ellipses",
    x = "Bill Length (mm)", y = "Flipper Length (mm)"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

ggsave("Lab3/Q7_group.png", width = 8, height = 6)

# 8. CORRELATE: Are there dependencies between body measurements?
cor_data <- penguins_clean |>
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

ggcorr(cor_data, label = TRUE, label_round = 2, label_size = 4) +
  labs(title = "CORRELATE: Dependencies between measurements") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Lab3/Q8_correlate.png", width = 7, height = 7)

# 9. TRENDS: How does body mass change with bill length?
ggplot(penguins_clean, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species), alpha = 0.5) +
  geom_smooth(method = "loess", color = "black", se = TRUE) +
  labs(
    title = "TRENDS: Does body mass develop systematically with bill length?",
    subtitle = "Answer: Yes, positive trend overall with species-specific patterns",
    x = "Bill Length (mm)", y = "Body Mass (g)"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

ggsave("Lab3/Q9_trends.png", width = 8, height = 6)

# 10. CYCLES: (Not applicable for cross-sectional data, but we can check year)
penguins_by_year <- penguins |>
  drop_na() |>
  group_by(year, species) |>
  summarise(mean_mass = mean(body_mass_g), n = n(), .groups = "drop")

ggplot(penguins_by_year, aes(x = year, y = mean_mass, color = species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "CYCLES: Do measurements re-occur periodically?",
    subtitle = "Answer: Only 3 years of data - insufficient to detect cycles",
    x = "Year", y = "Mean Body Mass (g)"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

ggsave("Lab3/Q10_cycles.png", width = 8, height = 6)

# 11. OUTLIERS: Are there special cases?
ggplot(penguins_clean, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(color = species), alpha = 0.6) +
  geom_point(
    data = penguins_clean |>
      filter(body_mass_g > 6000 | flipper_length_mm > 230 | flipper_length_mm < 175),
    color = "red", size = 4, shape = 1, stroke = 2
  ) +
  labs(
    title = "OUTLIERS: Are any observations special?",
    subtitle = "Red circles mark extreme values",
    x = "Body Mass (g)", y = "Flipper Length (mm)"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

ggsave("Lab3/Q11_outliers.png", width = 8, height = 6)

# 12. FEATURES: What characterizes the data?
ggplot(penguins_clean, aes(x = flipper_length_mm, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "FEATURES: What is characteristic of the data?",
    subtitle = "Answer: Bimodal distribution - Gentoo is clearly separated from others",
    x = "Flipper Length (mm)", y = "Density"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

ggsave("Lab3/Q12_features.png", width = 8, height = 6)

cat("\n=== Lab 3 complete! ===\n")
cat("12 analytic questions answered with visualizations.\n")
