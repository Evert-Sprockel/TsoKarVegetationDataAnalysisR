library(tidyverse)

# Summary of most common plots:
# https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# Themes of ggplot2:
# https://ggplot2.tidyverse.org/reference/ggtheme.html


# Create a sample data frame with fake data
set.seed(123)  # For reproducibility
df <- tibble(
  x = rnorm(100, mean = 50, sd = 10),           # Normally distributed variable
  y = rnorm(100, mean = 5, sd = 2),             # Another normally distributed variable
  category = sample(c("A", "B", "C"), 100, replace = TRUE)  # Random categorical variable
)

# View the first few rows of the data frame
head(df)

# Scatter plot
ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  theme_minimal()

# Line plot
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  theme_minimal()

# Bar plot
ggplot(df, aes(x = category)) +
  geom_bar() +
  theme_minimal()

# Histogram
ggplot(df, aes(x = x)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

# Box plot
ggplot(df, aes(x = category, y = y)) +
  geom_boxplot() +
  theme_minimal()

# Density plot
ggplot(df, aes(x = x)) +
  geom_density() +
  theme_minimal()

