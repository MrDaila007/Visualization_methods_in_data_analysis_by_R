# =============================================================================
# Lab 2: Automated Graphical Exploratory Data Analysis (autoGEDA)
# Group 4, variant 5
# =============================================================================

library(conflicted)
library(tidyverse)

conflicts_prefer(dplyr::filter, dplyr::select)

# --- 1. Get TidyTuesday dataset ---
# Using a recent TidyTuesday dataset
# install.packages("tidytuesdayR")

# Option A: Direct download from GitHub
tuesdata <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv"
)

glimpse(tuesdata)
summary(tuesdata)

# Alternative: use a classic dataset if download fails
if (nrow(tuesdata) == 0) {
  tuesdata <- mpg  # fallback
}

# --- 2. autoGEDA with dlookr ---
# install.packages("dlookr")
library(dlookr)

# Overview report
overview(tuesdata)

# Describe numeric variables
describe(tuesdata)

# Diagnose data quality
diagnose(tuesdata)

# Diagnose numeric variables
diagnose_numeric(tuesdata)

# Plot diagnostics
plot_outlier(tuesdata)

# Normality check
plot_normality(tuesdata)

# Generate full EDA report (HTML)
# eda_web_report(tuesdata, output_dir = "Lab2", output_file = "dlookr_report.html")

# --- 3. autoGEDA with brinton ---
# install.packages("brinton")
library(brinton)

# Automatic plot matrix for all variables
# wideplot(tuesdata)
# longplot(tuesdata)

# --- 4. autoGEDA with plotluck ---
# install.packages("plotluck")
library(plotluck)

# Automatic best plot selection
# plotluck(tuesdata, .~1)  # univariate overview

# --- 5. Table summary with gtExtras ---
# install.packages(c("gt", "gtExtras"))
library(gt)
library(gtExtras)

tuesdata |>
  head(20) |>
  gt() |>
  gt_theme_538()

# Summary with sparklines
# gt_plt_summary(tuesdata)

# --- 6. Manual autoGEDA approach ---
# When packages fail, create our own automated exploration

# 6.1 Numeric variables overview
numeric_vars <- tuesdata |> select(where(is.numeric)) |> names()
cat_vars <- tuesdata |> select(where(~is.character(.) | is.factor(.))) |> names()

cat("Numeric variables:", paste(numeric_vars, collapse = ", "), "\n")
cat("Categorical variables:", paste(cat_vars, collapse = ", "), "\n")

# 6.2 Histograms for all numeric variables
if (length(numeric_vars) > 0) {
  plots_hist <- map(numeric_vars, function(var) {
    ggplot(tuesdata, aes(x = .data[[var]])) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30) +
      labs(title = var) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10))
  })

  library(patchwork)
  wrap_plots(plots_hist, ncol = 3)
  ggsave("Lab2/numeric_histograms.png", width = 12, height = 8)
}

# 6.3 Boxplots for all numeric variables
if (length(numeric_vars) > 0) {
  plots_box <- map(numeric_vars, function(var) {
    ggplot(tuesdata, aes(y = .data[[var]])) +
      geom_boxplot(fill = "steelblue", alpha = 0.5) +
      labs(title = var) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10))
  })

  wrap_plots(plots_box, nrow = 1)
  ggsave("Lab2/numeric_boxplots.png", width = 14, height = 4)
}

# 6.4 Bar plots for categorical variables
if (length(cat_vars) > 0) {
  plots_bar <- map(cat_vars, function(var) {
    ggplot(tuesdata, aes(x = fct_infreq(.data[[var]]))) +
      geom_bar(fill = "coral") +
      labs(title = var, x = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        plot.title = element_text(size = 10)
      )
  })

  wrap_plots(plots_bar, ncol = 2)
  ggsave("Lab2/categorical_bars.png", width = 12, height = 6)
}

# 6.5 Correlation matrix for numeric variables
if (length(numeric_vars) >= 2) {
  library(GGally)
  tuesdata |>
    select(all_of(numeric_vars)) |>
    ggcorr(label = TRUE, label_round = 2) +
    labs(title = "Correlation Matrix") +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave("Lab2/correlation_matrix.png", width = 8, height = 8)
}

# 6.6 Missing values visualization
library(naniar)

gg_miss_var(tuesdata) +
  labs(title = "Missing Values by Variable") +
  theme_minimal()

ggsave("Lab2/missing_values.png", width = 8, height = 5)

vis_miss(tuesdata) +
  labs(title = "Missing Data Pattern")

ggsave("Lab2/missing_pattern.png", width = 10, height = 6)

cat("\n=== Lab 2 complete! ===\n")
