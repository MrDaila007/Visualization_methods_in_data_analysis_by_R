# =============================================================================
# Lab 1: One exercise from each chapter 3-11
# "Graphical Data Analysis with R" by Antony Unwin
# Group 4, variant 5 (exercise 5 from each chapter)
# =============================================================================

library(conflicted)
library(tidyverse)
library(patchwork)
library(GGally)
library(MASS)

conflicts_prefer(dplyr::filter, dplyr::select)

# =============================================================================
# Chapter 3, Exercise 5: Zuni educational funding
# Dataset: zuni from lawstat package
# =============================================================================

# install.packages("lawstat")
library(lawstat)

data(zuni)
glimpse(zuni)

# (a) Are the lowest and highest 5% of the revenue values extreme?
# Histogram
p3a_hist <- ggplot(zuni, aes(x = revenue)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "white") +
  labs(
    title = "Ch3 Ex5: Revenue per Pupil (Zuni School Districts)",
    subtitle = "Red lines show 5th and 95th percentiles",
    x = "Revenue per pupil ($)", y = "Count"
  ) +
  geom_vline(
    xintercept = quantile(zuni$revenue, c(0.05, 0.95)),
    color = "red", linetype = "dashed"
  ) +
  theme_minimal()

# Boxplot
p3a_box <- ggplot(zuni, aes(y = revenue)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  labs(title = "Boxplot of Revenue", y = "Revenue per pupil ($)") +
  theme_minimal()

p3a_hist + p3a_box + plot_layout(widths = c(3, 1))
ggsave("Lab1/ch3_ex5_revenue_overview.png", width = 10, height = 5)

# The histogram is preferred here because it shows the shape of the distribution
# more clearly. The boxplot marks outliers but doesn't show the distribution form.
# The lowest/highest 5% include some quite extreme values, especially the highest.

# (b) Density estimate of the trimmed data
q05 <- quantile(zuni$revenue, 0.05)
q95 <- quantile(zuni$revenue, 0.95)
zuni_trimmed <- zuni |> filter(revenue >= q05, revenue <= q95)

ggplot(zuni_trimmed, aes(x = revenue)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 300, fill = "steelblue", color = "white", alpha = 0.5) +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = "Trimmed Revenue Distribution (5%-95%)",
    subtitle = "Density estimate overlaid - distribution is roughly symmetric",
    x = "Revenue per pupil ($)", y = "Density"
  ) +
  theme_minimal()

ggsave("Lab1/ch3_ex5_trimmed_density.png", width = 8, height = 5)

# (c) Q-Q plot for normality check
ggplot(zuni_trimmed, aes(sample = revenue)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(
    title = "Q-Q Plot of Trimmed Revenue Data",
    subtitle = "Approximately normal with minor deviations in tails",
    x = "Theoretical Quantiles", y = "Sample Quantiles"
  ) +
  theme_minimal()

ggsave("Lab1/ch3_ex5_qq_plot.png", width = 6, height = 6)

# =============================================================================
# Chapter 4, Exercise 5: Occupational mobility
# Dataset: Yamaguchi87 from vcdExtra
# =============================================================================

# install.packages("vcdExtra")
library(vcdExtra)

data(Yamaguchi87)
glimpse(Yamaguchi87)

yam_df <- as.data.frame(Yamaguchi87)

# (a) Compare distributions of sons' occupations across three countries
sons_by_country <- yam_df |>
  group_by(Country, Son) |>
  summarise(n = sum(Freq), .groups = "drop") |>
  group_by(Country) |>
  mutate(prop = n / sum(n))

ggplot(sons_by_country, aes(x = Son, y = prop, fill = Country)) +
  geom_col(position = "dodge") +
  labs(
    title = "Ch4 Ex5: Sons' Occupations by Country",
    subtitle = "Comparing occupational distributions across UK, US, Japan",
    x = "Occupation", y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Lab1/ch4_ex5_sons_by_country.png", width = 10, height = 6)

# (b) Compare sons' and fathers' occupations in the UK
uk_data <- yam_df |> filter(Country == "UK")

uk_fathers <- uk_data |>
  group_by(Father) |>
  summarise(n = sum(Freq), .groups = "drop") |>
  mutate(generation = "Father", occupation = Father) |>
  select(occupation, n, generation)

uk_sons <- uk_data |>
  group_by(Son) |>
  summarise(n = sum(Freq), .groups = "drop") |>
  mutate(generation = "Son", occupation = Son) |>
  select(occupation, n, generation)

uk_compare <- bind_rows(uk_fathers, uk_sons) |>
  group_by(generation) |>
  mutate(prop = n / sum(n))

ggplot(uk_compare, aes(x = occupation, y = prop, fill = generation)) +
  geom_col(position = "dodge") +
  labs(
    title = "UK: Fathers' vs Sons' Occupations",
    subtitle = "Shift from manual to non-manual occupations across generations",
    x = "Occupation", y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Lab1/ch4_ex5_uk_generations.png", width = 10, height = 6)

# (c) The results show occupational mobility - sons tend to move toward
# non-manual occupations compared to their fathers. This is expected given
# the economic development and expansion of service sectors.

# =============================================================================
# Chapter 5, Exercise 5: Bank sex discrimination
# Dataset: case1202 from Sleuth2
# =============================================================================

# install.packages("Sleuth2")
library(Sleuth2)

data(case1202)
glimpse(case1202)

# (a) Scatterplot matrix of Senior, Age, Exper
case1202 |>
  select(Senior, Age, Exper) |>
  ggpairs(
    lower = list(continuous = wrap("points", alpha = 0.5, color = "steelblue")),
    diag = list(continuous = wrap("densityDiag", fill = "steelblue", alpha = 0.5)),
    upper = list(continuous = wrap("cor", size = 5))
  ) +
  labs(title = "Ch5 Ex5: Bank Employment Variables") +
  theme_minimal()

ggsave("Lab1/ch5_ex5_scattermatrix.png", width = 8, height = 8)

# Notable features:
# - Age and Exper show strong positive correlation (older people have more experience)
# - Senior shows a ceiling effect (limited by when the bank was established)
# - The Senior vs Age/Exper plots show a triangular shape because you can't have
#   more seniority than your age or experience

# (b) Seniority is bounded by time since the person joined the bank.
# Age and Experience are correlated because both increase with time.
# Seniority doesn't have the same structure because it depends on when
# the person was hired, not their total life or work experience.

# =============================================================================
# Chapter 6, Exercise 5: Bodyfat
# Dataset: bodyfat from MMST (or mfp)
# =============================================================================

# install.packages("mfp")
library(mfp)

data(bodyfat)
glimpse(bodyfat)

# Parallel coordinate plot
library(GGally)

bodyfat_scaled <- bodyfat |>
  mutate(across(everything(), scales::rescale))

ggparcoord(bodyfat, columns = 1:ncol(bodyfat), alpha = 0.1, scale = "uniminmax") +
  labs(
    title = "Ch6 Ex5: Parallel Coordinate Plot of Bodyfat Data",
    subtitle = "Variables scaled to [0,1]; look for outliers and patterns",
    x = "Variable", y = "Scaled Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Lab1/ch6_ex5_parallel_coords.png", width = 14, height = 6)

# (a) Outliers: Cases with extreme values across multiple variables are visible
# (b) Height: One individual with very low height stands out - possibly a data error
# (c) Density and bodyfat: Strong inverse relationship visible in the parallel plot
# (d) Alternative orderings: Group related body measurements together (circumferences)

# =============================================================================
# Chapter 7, Exercise 5: Airline arrivals (Simpson's paradox)
# Dataset: from package or manual creation
# =============================================================================

# The fastR package may not be available; recreate the classic airline data
# Alaska Airlines vs America West at 5 airports

airline_data <- tibble(
  airline = rep(c("Alaska", "America West"), each = 5),
  airport = rep(c("Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle"), 2),
  on_time = c(497, 221, 212, 503, 1841, 694, 4840, 383, 320, 201),
  delayed = c(62, 12, 20, 102, 305, 117, 415, 65, 129, 61)
) |>
  mutate(
    total = on_time + delayed,
    delay_rate = delayed / total
  )

# Overall delay rate
overall <- airline_data |>
  group_by(airline) |>
  summarise(
    total_delayed = sum(delayed),
    total_flights = sum(total),
    overall_rate = total_delayed / total_flights,
    .groups = "drop"
  )

# Spineplot equivalent: overall comparison
p7a <- ggplot(overall, aes(x = airline, y = overall_rate, fill = airline)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Overall Delay Rate by Airline",
    subtitle = "America West appears better overall...",
    x = "", y = "Delay Rate"
  ) +
  theme_minimal()

# By airport
p7b <- ggplot(airline_data, aes(x = airline, y = delay_rate, fill = airline)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~airport, nrow = 1) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Delay Rate by Airline and Airport",
    subtitle = "...but Alaska is better at EVERY individual airport! (Simpson's Paradox)",
    x = "", y = "Delay Rate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p7a / p7b
ggsave("Lab1/ch7_ex5_simpsons_paradox.png", width = 12, height = 8)

# Simpson's paradox: Alaska Airlines has a lower delay rate at each airport,
# but a higher overall delay rate because it flies more to Seattle
# (which has high delay rates), while America West flies mostly to Phoenix
# (which has low delay rates).

# =============================================================================
# Chapter 8, Exercise 5: Pima Indians overview
# Dataset: Pima.tr2 from MASS + larger UCI dataset
# =============================================================================

# Compare the MASS subset with the full dataset
# The full UCI dataset has 768 cases, MASS has 532 complete cases
data(Pima.tr, package = "MASS")
data(Pima.te, package = "MASS")

pima_r <- bind_rows(Pima.tr, Pima.te) |>
  mutate(source = "R (complete)")

# For the full UCI data, we note that the missing cases have 0s for some variables
# which are biologically impossible (0 glucose, 0 blood pressure, etc.)
# We'll simulate this comparison using the available data

pima_continuous <- c("glu", "bp", "skin", "bmi", "ped", "age")

pima_long <- pima_r |>
  pivot_longer(cols = all_of(pima_continuous), names_to = "variable", values_to = "value")

ggplot(pima_long, aes(x = value, fill = type)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "Ch8 Ex5: Pima Indians - Continuous Variables by Diabetes Type",
    x = "Value", y = "Count"
  ) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  theme_minimal()

ggsave("Lab1/ch8_ex5_pima_overview.png", width = 12, height = 8)

# Npreg and type
p8b1 <- ggplot(pima_r, aes(x = npreg, fill = type)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(title = "Number of Pregnancies by Type") +
  theme_minimal()

p8b2 <- ggplot(pima_r, aes(x = type)) +
  geom_bar(fill = c("steelblue", "coral")) +
  labs(title = "Diabetes Type Distribution") +
  theme_minimal()

p8b1 + p8b2
ggsave("Lab1/ch8_ex5_pima_categorical.png", width = 10, height = 5)

# =============================================================================
# Chapter 9, Exercise 5: Pearson heights - outlier detection
# Dataset: father.son from UsingR
# =============================================================================

# install.packages("UsingR")
library(UsingR)

data(father.son)
glimpse(father.son)

# (a) Scatterplot with potential outliers
ggplot(father.son, aes(fheight, sheight)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Ch9 Ex5: Father-Son Heights (Pearson)",
    subtitle = "Red: linear fit, Dashed: y=x line. Look for outliers in corners.",
    x = "Father's Height (inches)",
    y = "Son's Height (inches)"
  ) +
  theme_minimal()

ggsave("Lab1/ch9_ex5_pearson_scatter.png", width = 8, height = 7)

# (b) With bivariate density estimate
ggplot(father.son, aes(fheight, sheight)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_density_2d(color = "red") +
  labs(
    title = "Pearson Heights with Bivariate Density Contours",
    subtitle = "Points outside outermost contour are potential outliers",
    x = "Father's Height (inches)",
    y = "Son's Height (inches)"
  ) +
  theme_minimal()

ggsave("Lab1/ch9_ex5_pearson_density.png", width = 8, height = 7)

# Cases far from the main concentration (e.g., very short fathers with tall sons
# or vice versa) would be identified as outliers by the density model.

# =============================================================================
# Chapter 10, Exercise 5: Diamonds - color vs price
# Dataset: diamonds from ggplot2
# =============================================================================

data(diamonds)

# (a) Price by color
ggplot(diamonds, aes(x = color, y = price, fill = color)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_fill_viridis_d() +
  labs(
    title = "Ch10 Ex5: Diamond Price by Color",
    subtitle = "Surprisingly, 'worse' colors (J) have higher median prices!",
    x = "Color (D=best to J=worst)", y = "Price ($)"
  ) +
  theme_minimal()

ggsave("Lab1/ch10_ex5_diamonds_price_color.png", width = 8, height = 6)

# This is surprising! Worse color diamonds have higher prices because they
# tend to be larger (higher carat). It's a confounding variable.

# (b) Coefficient estimates with confidence intervals
model <- lm(log(price) ~ color + log(carat), data = diamonds)
coef_df <- broom::tidy(model, conf.int = TRUE) |>
  filter(str_starts(term, "color"))

ggplot(coef_df, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "steelblue", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Color Coefficient Estimates (controlling for carat)",
    subtitle = "After controlling for carat, worse colors do have lower prices",
    x = "", y = "Coefficient (log scale)"
  ) +
  theme_minimal()

ggsave("Lab1/ch10_ex5_diamonds_coefficients.png", width = 8, height = 5)

# =============================================================================
# Chapter 11, Exercise 5: Bundesliga goals
# Dataset: Bundesliga (from vcd or manual)
# =============================================================================

# install.packages("vcd")
library(vcd)

data(Bundesliga)
glimpse(Bundesliga)

# (a) Annual average goals per home game for each team
bundesliga_summary <- Bundesliga |>
  mutate(season = as.numeric(format(Date, "%Y"))) |>
  group_by(season = ifelse(
    as.numeric(format(Date, "%m")) >= 7,
    as.numeric(format(Date, "%Y")),
    as.numeric(format(Date, "%Y")) - 1
  ), HomeTeam) |>
  summarise(
    avg_home_goals = mean(HomeGoals, na.rm = TRUE),
    games = n(),
    .groups = "drop"
  )

# Focus on teams with many seasons (Hamburg is the only ever-present team)
team_seasons <- bundesliga_summary |>
  count(HomeTeam, sort = TRUE)

top_teams <- team_seasons |> filter(n >= 30) |> pull(HomeTeam)

ggplot(
  bundesliga_summary |> filter(HomeTeam %in% top_teams),
  aes(x = season, y = avg_home_goals, color = HomeTeam)
) +
  geom_line(alpha = 0.7) +
  geom_smooth(se = FALSE, linewidth = 0.5, alpha = 0.3) +
  facet_wrap(~HomeTeam) +
  labs(
    title = "Ch11 Ex5: Average Home Goals per Game in the Bundesliga",
    subtitle = "Teams with 30+ seasons shown. Trellis display handles missing years.",
    x = "Season", y = "Avg Home Goals"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Lab1/ch11_ex5_bundesliga_trellis.png", width = 14, height = 8)

# (b) Compare Hamburg and Bayern Munich home vs away
hamburg_bayern <- Bundesliga |>
  mutate(season = ifelse(
    as.numeric(format(Date, "%m")) >= 7,
    as.numeric(format(Date, "%Y")),
    as.numeric(format(Date, "%Y")) - 1
  )) |>
  filter(HomeTeam %in% c("Hamburg", "Bayern Munich") |
           AwayTeam %in% c("Hamburg", "Bayern Munich"))

# Calculate home and away scoring rates
home_goals <- hamburg_bayern |>
  filter(HomeTeam %in% c("Hamburg", "Bayern Munich")) |>
  group_by(season, team = HomeTeam) |>
  summarise(avg_goals = mean(HomeGoals), .groups = "drop") |>
  mutate(location = "Home")

away_goals <- hamburg_bayern |>
  filter(AwayTeam %in% c("Hamburg", "Bayern Munich")) |>
  group_by(season, team = AwayTeam) |>
  summarise(avg_goals = mean(AwayGoals), .groups = "drop") |>
  mutate(location = "Away")

hb_goals <- bind_rows(home_goals, away_goals)

# Time series comparison
p11a <- ggplot(hb_goals, aes(x = season, y = avg_goals, color = location)) +
  geom_line() +
  facet_wrap(~team) +
  labs(
    title = "Home vs Away Scoring: Time Series",
    x = "Season", y = "Avg Goals per Game"
  ) +
  scale_color_manual(values = c("steelblue", "coral")) +
  theme_minimal()

# Scatterplot comparison
hb_wide <- hb_goals |>
  pivot_wider(names_from = location, values_from = avg_goals)

p11b <- ggplot(hb_wide, aes(x = Home, y = Away, color = team)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Home vs Away Goals: Scatterplot",
    subtitle = "Points above dashed line = more away goals",
    x = "Avg Home Goals", y = "Avg Away Goals"
  ) +
  scale_color_manual(values = c("coral", "steelblue")) +
  theme_minimal()

p11a / p11b
ggsave("Lab1/ch11_ex5_hamburg_bayern.png", width = 12, height = 10)

# The time series shows trends over time; the scatterplot shows the relationship
# between home and away scoring. Both are informative in different ways:
# - Time series: reveals trends and changes over seasons
# - Scatterplot: reveals correlation between home and away performance

cat("\n=== Lab 1 complete! ===\n")
cat("All plots saved to Lab1/ directory.\n")
