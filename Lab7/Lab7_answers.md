# Lab 7: Tamara Munzner - Foundations 1

## Format: PDF (fill in the Answer Template from Google Drive)

## Section 2: Data Abstraction - Attributes

For each variable, classify as:
- **Categorical** (nominal/ordinal) or **Quantitative** (interval/ratio)
- **Sequential** or **Diverging** or **Cyclic**
- **Key** (independent) or **Value** (dependent)

## Section 3: Data Exploration - Palmer Penguin Dataset

Use R to explore the Palmer Penguins dataset:

```r
library(palmerpenguins)
library(tidyverse)

data(penguins)

# Attribute types:
# species    - Categorical, Nominal
# island     - Categorical, Nominal
# bill_length_mm   - Quantitative, Sequential
# bill_depth_mm    - Quantitative, Sequential
# flipper_length_mm - Quantitative, Sequential
# body_mass_g      - Quantitative, Sequential
# sex        - Categorical, Nominal (binary)
# year       - Quantitative, Sequential (ordinal in context)

# Key explorations:
ggplot(penguins, aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point() +
  theme_minimal()

ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point() +
  theme_minimal()
```

## Section 4: Data & Task Abstraction - Foreign Aid

For the Foreign Aid dataset:
1. Identify **what** (data types and attributes)
2. Identify **why** (user tasks - analyze, search, query)
3. Identify **how** (visual encodings and idioms)

### Data abstraction:
- Dataset type: **Table** (rows = countries/years, columns = aid metrics)
- Attribute types: country (categorical/nominal), year (ordered/quantitative), aid amount (quantitative/sequential)

### Task abstraction:
- **Analyze**: Discover trends in aid over time
- **Search**: Lookup specific country's aid
- **Query**: Compare aid between countries/regions

### Recommended idioms:
- Line chart for trends over time
- Choropleth map for geographic distribution
- Bar chart for country comparisons
