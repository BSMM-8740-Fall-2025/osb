# Step 1: Load the required libraries and dataset

# Load the required libraries
library(tidymodels)
library(dplyr)
library(purrr)
library(ggplot2)
require(magrittr)

# Load the Iris dataset
data("iris", package='datasets')

# Step 2: Explore the dataset

# Task 1: Check the structure of the dataset (data types, variable names, etc.)
iris %>% str()

iris %>% dplyr::glimpse()

# Task 2: Display the first few rows of the dataset
iris %>% head()

iris %>% dplyr::slice_head(n = 5)

# Task 3: Check for missing values in the dataset
iris %>% purrr::map_lgl( ~any(is.na(.)))

# Step 3: Data Cleaning

# Task 4: Remove any rows with missing values (NA) from the dataset
cleaned_iris <- iris %>%
  tidyr::drop_na()

# Step 4: Data Summarization and Visualization

# Task 5: Generate a summary of numerical variables (mean, median, min, max, etc.)
summary_stats <- cleaned_iris %>%
  dplyr::select_if(is.numeric) %>%
  purrr::map(summary)

summary_stats <- cleaned_iris %>%
  dplyr::group_by(Species) %>%
  dplyr::summarize(
    dplyr::across(
      where(is.numeric)
      ,list(
        min = min
        ,max = max
      )
    )
  )

# Task 6: Create a scatterplot matrix to visualize relationships between numerical variables
scatterplot_matrix <- cleaned_iris %>%
  dplyr::select_if(is.numeric) %>%
  GGally::ggpairs()

# Task 7: Create a boxplot to compare the distribution of numerical variables across species
boxplot_species <- cleaned_iris %>%
  tidyr::pivot_longer(cols = starts_with("Sepal") | starts_with("Petal"),
               names_to = "Measurement", values_to = "Value") %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  labs(title = "Boxplot - Comparison of Measurements across Species")

# Task 8: Create a bar plot to visualize the distribution of flower species
barplot_species <- cleaned_iris %>%
  dplyr::count(Species) %>%
  ggplot(aes(x = Species, y = n, fill = Species)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot - Distribution of Flower Species")

# Task 9: Create a density plot to visualize the distribution of each numerical variable
density_plots <- cleaned_iris %>%
  dplyr::group_by(Species) %>%
  dplyr::select_if(is.numeric) %>%
  tidyr::pivot_longer(-Species, names_to = "Measurement", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Species)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~Measurement, scales = "free") +
  labs(title = "Density Plot - Distribution of Numerical Variables")

# Task 10: Create a correlation plot to visualize the correlation between numerical variables
correlation_plot <- cleaned_iris %>%
  dplyr::select_if(is.numeric) %>%
  cor() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable1") %>%
  tidyr::pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation") %>%
  ggplot(aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Plot - Correlation Between Numerical Variables")


# Step 5: Putting it all together

# Task 11: Create a function to perform all the tasks above and return the visualizations
perform_exploratory_analysis <- function(data) {
  str(data)
  head(data)
  map_lgl(data, ~any(is.na(.)))
  cleaned_data <- data %>%
    drop_na()

  summary_stats <- cleaned_data %>%
    select_if(is.numeric) %>%
    map(summary)

  scatterplot_matrix <- cleaned_data %>%
    select_if(is.numeric) %>%
    ggpairs()

  boxplot_species <- cleaned_data %>%
    pivot_longer(cols = starts_with("Sepal") | starts_with("Petal"),
                 names_to = "Measurement", values_to = "Value") %>%
    ggplot(aes(x = Species, y = Value, fill = Species)) +
    geom_boxplot() +
    labs(title = "Boxplot - Comparison of Measurements across Species")

  barplot_species <- cleaned_data %>%
    count(Species) %>%
    ggplot(aes(x = Species, y = n, fill = Species)) +
    geom_bar(stat = "identity") +
    labs(title = "Bar Plot - Distribution of Flower Species")

  density_plots <- cleaned_data %>%
    select_if(is.numeric) %>%
    pivot_longer(everything(), names_to = "Measurement", values_to = "Value") %>%
    ggplot(aes(x = Value, fill = Species)) +
    geom_density(alpha = 0.6) +
    facet_wrap(~Measurement, scales = "free") +
    labs(title = "Density Plot - Distribution of Numerical Variables")

  correlation_plot <- cleaned_data %>%
    select_if(is.numeric) %>%
    cor() %>%
    as.data.frame() %>%
    rownames_to_column("Variable1") %>%
    pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation") %>%
    ggplot(aes(x = Variable1, y = Variable2, fill = Correlation)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Plot - Correlation Between Numerical Variables")

  return(list(
    "summary_stats" = summary_stats,
    "scatterplot_matrix" = scatterplot_matrix,
    "boxplot_species" = boxplot_species,
    "barplot_species" = barplot_species,
    "density_plots" = density_plots,
    "correlation_plot" = correlation_plot
  ))
}

# Task 12: Call the function with the Iris dataset
exploratory_analysis_results <- perform_exploratory_analysis(iris)
