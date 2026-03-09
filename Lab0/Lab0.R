# =============================================================================
# Lab 0: Install R, RStudio + Packages
# Visualization methods in data analysis by R
# Group 4, variant 5
# =============================================================================

# --- 0. Setup: R Projects ---
# https://r4ds.hadley.nz/workflow-scripts.html#projects
# Use RStudio Projects (.Rproj) to organize work
# Each lab should be its own project or part of a single course project

# --- 1. Install required packages ---
# Only run once (uncomment if needed):
# install.packages(c(
#   "tidyverse",     # core data science packages
#   "conflicted",    # manage function name conflicts
#   "GDAdata",       # datasets from the textbook
#   "patchwork",     # combine multiple plots
#   "GGally",        # ggplot2 extensions (ggpairs, ggcorr)
#   "ggmosaic",      # mosaic plots
#   "ggrepel",       # non-overlapping text labels
#   "palmerpenguins", # penguin dataset
#   "nycflights13",  # flights dataset
#   "naniar",        # missing data visualization
#   "plotly",        # interactive plots
#   "ggiraph",       # interactive ggplot2
#   "paletteer",     # color palette collection
#   "colorBlindness", # color blindness simulation
#   "colorspace",    # color manipulation + specplot()
#   "lintr",         # static code analysis
#   "styler"         # auto-format code to tidyverse style
# ))

# --- 2. Load core libraries ---
library(conflicted)
library(tidyverse)

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag)

# --- 3. Tidyverse style guide: https://style.tidyverse.org/ ---
# Key rules:
# - Use snake_case for names
# - Use <- for assignment (not =)
# - Spaces around operators: x + y, not x+y
# - Max 80 characters per line
# - Use |> (pipe) for chaining operations
# - Use styler::style_file() to auto-format
# - Use lintr::lint() to check style

# --- 4. Explore built-in datasets ---

# 4.1 SpeedSki from GDAdata
library(GDAdata)

glimpse(SpeedSki)
summary(SpeedSki)

# Histogram of Speed
ggplot(SpeedSki, aes(Speed)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Speed Ski Record Speeds",
    x = "Speed (km/h)",
    y = "Count"
  ) +
  theme_minimal()

ggsave("Lab0/SpeedSki_histogram.png", width = 8, height = 5)

# Faceted by Sex and Event
ggplot(SpeedSki, aes(Speed, fill = Sex)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.7) +
  facet_grid(rows = vars(Event)) +
  labs(
    title = "Speed Distribution by Event and Sex",
    x = "Speed (km/h)",
    y = "Count"
  ) +
  theme_minimal()

ggsave("Lab0/SpeedSki_faceted.png", width = 8, height = 8)

# 4.2 Automated plotting with purrr + patchwork
library(patchwork)
library(purrr)

plot_hist <- function(data, var_name) {
  data |>
    ggplot(aes(x = .data[[var_name]])) +
    geom_histogram(fill = "steelblue", color = "white") +
    labs(title = var_name) +
    theme_minimal()
}

# 4.3 iris dataset
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point(size = 2) +
  labs(
    title = "Iris: Petal Length vs Petal Width",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)"
  ) +
  theme_minimal()

ggsave("Lab0/iris_scatter.png", width = 8, height = 5)

# 4.4 UCBAdmissions
ucb_df <- as.data.frame(UCBAdmissions)

p1 <- ggplot(ucb_df, aes(Dept, weight = Freq)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Applicants by Dept") +
  theme_minimal()

p2 <- ggplot(ucb_df, aes(Gender, weight = Freq)) +
  geom_bar(fill = "coral") +
  labs(title = "Applicants by Gender") +
  theme_minimal()

p3 <- ggplot(ucb_df, aes(Admit, weight = Freq)) +
  geom_bar(fill = "seagreen") +
  labs(title = "Admission Status") +
  theme_minimal()

p1 + p2 + p3 + plot_layout(widths = c(3, 1, 1))

ggsave("Lab0/UCB_overview.png", width = 12, height = 5)

# 4.5 Pima Indians diabetes data
library(MASS)

pima_vars <- c("glu", "bp", "skin", "bmi", "ped", "age")

pima_hists <- map(pima_vars, ~plot_hist(Pima.tr2, .x))
wrap_plots(pima_hists, ncol = 3)

ggsave("Lab0/Pima_histograms.png", width = 12, height = 8)

# Boxplots
plot_box <- function(data, var_name) {
  data |>
    ggplot(aes(y = .data[[var_name]])) +
    geom_boxplot(fill = "steelblue", alpha = 0.5) +
    labs(title = var_name) +
    theme_minimal()
}

pima_boxes <- map(pima_vars, ~plot_box(Pima.tr2, .x))
wrap_plots(pima_boxes, nrow = 1)

ggsave("Lab0/Pima_boxplots.png", width = 12, height = 4)

# 4.6 Titanic
titanic_df <- as.data.frame(Titanic)

tp1 <- ggplot(titanic_df, aes(Class, weight = Freq)) +
  geom_bar(fill = "steelblue") +
  theme_minimal()

tp2 <- ggplot(titanic_df, aes(Sex, weight = Freq)) +
  geom_bar(fill = "coral") +
  theme_minimal()

tp3 <- ggplot(titanic_df, aes(Age, weight = Freq)) +
  geom_bar(fill = "gold3") +
  theme_minimal()

tp4 <- ggplot(titanic_df, aes(Survived, weight = Freq)) +
  geom_bar(fill = "seagreen") +
  theme_minimal()

(tp1 + tp2 + tp3 + tp4) +
  plot_layout(nrow = 1, widths = c(2, 1, 1, 1)) &
  scale_y_continuous(limits = c(0, 2200))

ggsave("Lab0/Titanic_overview.png", width = 12, height = 5)

# 4.7 Old Faithful
ggplot(faithful, aes(eruptions, waiting)) +
  geom_point(alpha = 0.5) +
  geom_density_2d(color = "steelblue") +
  labs(
    title = "Old Faithful Geyser: Eruption Duration vs Waiting Time",
    x = "Eruption duration (min)",
    y = "Waiting time (min)"
  ) +
  theme_minimal()

ggsave("Lab0/faithful_scatter.png", width = 8, height = 6)

# --- 5. London Bikes dataset ---
library(readr)
library(lubridate)

bikes <- read_csv("london-bikes.csv", col_types = "Dcfffilllddddc")

glimpse(bikes)

# Temperature by month
temp_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

ggplot(bikes, aes(x = month(date, label = TRUE), y = temp)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  labs(
    title = "London Bike Sharing: Temperature by Month",
    x = "Month",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

ggsave("Lab0/bikes_temp_month.png", width = 10, height = 5)

Sys.setlocale("LC_TIME", temp_locale)

# --- 6. Color blindness simulation ---
# library(colorBlindness)
# library(colorspace)
# Use specplot() to analyze color palettes:
# specplot(rainbow(10))
# specplot(heat.colors(10))

# --- 7. Style check ---
# Run in RStudio:
# lintr::lint("Lab0/Lab0.R")
# styler::style_file("Lab0/Lab0.R")

# --- 8. Session info ---
sessionInfo()
