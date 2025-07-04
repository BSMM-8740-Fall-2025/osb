# Tree-Based Regression Model Visualization
# Illustrating a continuous response y with two covariates x1, x2 partitioned into three regions

library(tidyverse)
library(ggplot2)
library(viridis)
library(patchwork)

# Set seed for reproducibility
set.seed(42)

# Define the support and partitions for x1 and x2
x1_range <- c(0, 10)
x2_range <- c(0, 8)

# Create a fine grid for visualization
grid_resolution <- 200
x1_grid <- seq(x1_range[1], x1_range[2], length.out = grid_resolution)
x2_grid <- seq(x2_range[1], x2_range[2], length.out = grid_resolution)

# Create all combinations
grid_data <- expand_grid(x1 = x1_grid, x2 = x2_grid)

# Define tree partitions (three regions)
# Region 1: x1 < 4
# Region 2: x1 >= 4 AND x2 < 5
# Region 3: x1 >= 4 AND x2 >= 5

partition_function <- function(x1, x2) {
  case_when(
    x1 < 4 ~ "Region 1",
    x1 >= 4 & x2 < 5 ~ "Region 2",
    x1 >= 4 & x2 >= 5 ~ "Region 3",
    TRUE ~ "Other"
  )
}

# Apply partitioning to grid
grid_data <- grid_data %>%
  mutate(
    region = partition_function(x1, x2),
    region_numeric = case_when(
      region == "Region 1" ~ 1,
      region == "Region 2" ~ 2,
      region == "Region 3" ~ 3,
      TRUE ~ NA_real_
    )
  )

# Define constant y values for each region (estimated by the tree)
region_means <- c(
  "Region 1" = 15.2,
  "Region 2" = 8.7,
  "Region 3" = 22.3
)

# Add predicted y values to grid
grid_data <- grid_data %>%
  mutate(y_predicted = region_means[region])

# Generate sample training data points
n_points <- 150

# Sample points from each region
sample_data <- tibble()

# Region 1: x1 < 4
n1 <- 50
region1_data <- tibble(
  x1 = runif(n1, x1_range[1], 4),
  x2 = runif(n1, x2_range[1], x2_range[2])
) %>%
  mutate(
    region = "Region 1",
    y_true = 15 + 2*x1 - 0.5*x2 + rnorm(n1, 0, 1.5),
    y_predicted = region_means["Region 1"]
  )

# Region 2: x1 >= 4 AND x2 < 5
n2 <- 50
region2_data <- tibble(
  x1 = runif(n2, 4, x1_range[2]),
  x2 = runif(n2, x2_range[1], 5)
) %>%
  mutate(
    region = "Region 2",
    y_true = 8 + 0.8*x1 + 0.3*x2 + rnorm(n2, 0, 1.2),
    y_predicted = region_means["Region 2"]
  )

# Region 3: x1 >= 4 AND x2 >= 5
n3 <- 50
region3_data <- tibble(
  x1 = runif(n3, 4, x1_range[2]),
  x2 = runif(n3, 5, x2_range[2])
) %>%
  mutate(
    region = "Region 3",
    y_true = 20 + 1.2*x1 + 0.8*x2 + rnorm(n3, 0, 1.8),
    y_predicted = region_means["Region 3"]
  )

# Combine all sample data
sample_data <- bind_rows(region1_data, region2_data, region3_data)

# Create the main partition plot
p1 <- ggplot() +
  # Background regions with predicted values
  geom_raster(data = grid_data, aes(x = x1, y = x2, fill = y_predicted), alpha = 0.8) +

  # Add decision boundaries
  geom_vline(xintercept = 4, color = "black", size = 1.2, linetype = "solid") +
  geom_segment(aes(x = 4, y = 5, xend = x1_range[2], yend = 5),
               color = "black", size = 1.2, linetype = "solid") +

  # Sample data points
  geom_point(data = sample_data, aes(x = x1, y = x2, color = region),
             size = 2, alpha = 0.7, stroke = 0.5) +

  # Region labels
  annotate("text", x = 2, y = 6.5, label = "Region 1\nŷ = 15.2",
           size = 5, fontface = "bold", color = "white",
           bbox = list(boxcolor = "black", fill = "black", alpha = 0.7)) +
  annotate("text", x = 7, y = 2.5, label = "Region 2\nŷ = 8.7",
           size = 5, fontface = "bold", color = "white",
           bbox = list(boxcolor = "black", fill = "black", alpha = 0.7)) +
  annotate("text", x = 7, y = 6.5, label = "Region 3\nŷ = 22.3",
           size = 5, fontface = "bold", color = "white",
           bbox = list(boxcolor = "black", fill = "black", alpha = 0.7)) +

  scale_fill_viridis_c(name = "Predicted y", option = "plasma") +
  scale_color_manual(name = "Region",
                     values = c("Region 1" = "#440154",
                                "Region 2" = "#31688e",
                                "Region 3" = "#fde725")) +

  labs(
    title = "Partition View",
    subtitle = "Two covariates (x1, x2) partitioned into three regions",
    x = "x1",
    y = "x2",
    caption = "Black lines show decision boundaries\nPoints show training data\nBackground shows predicted values"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +

  coord_fixed(ratio = 1) +
  xlim(x1_range) + ylim(x2_range)

# Create decision tree diagram
tree_structure <- tibble(
  x = c(0.5, 0.25, 0.75, 0.625, 0.875),
  y = c(0.8, 0.6, 0.6, 0.4, 0.4),
  label = c("x1 < 4?", "Region 1\nŷ = 15.2", "x2 < 5?", "Region 2\nŷ = 8.7", "Region 3\nŷ = 22.3"),
  node_type = c("decision", "leaf", "decision", "leaf", "leaf")
)

p2 <- ggplot(tree_structure, aes(x = x, y = y)) +
  geom_point(aes(color = node_type), size = 8) +
  #geom_text(aes(label = label), size = 3.5, fontface = "bold", nudge_x = 0.11) +
  geom_text(
    data = tree_structure |> dplyr::filter(! stringr::str_starts(label,"Reg"))
    , aes(label = label), size = 3.5, fontface = "bold", nudge_x = 0.11
  ) +
  geom_text(
    data = tree_structure |> dplyr::filter(stringr::str_starts(label,"Reg"))
    , aes(label = label), size = 3.5, fontface = "bold", nudge_y = -0.06
  ) +

  # Add tree connections
  geom_segment(aes(x = 0.5, y = 0.8, xend = 0.25, yend = 0.6), size = 1) +  # Root to Region 1
  geom_segment(aes(x = 0.5, y = 0.8, xend = 0.75, yend = 0.6), size = 1) +   # Root to second decision
  geom_segment(aes(x = 0.75, y = 0.6, xend = 0.625, yend = 0.4), size = 1) + # Second decision to Region 2
  geom_segment(aes(x = 0.75, y = 0.6, xend = 0.875, yend = 0.4), size = 1) + # Second decision to Region 3

  # Add decision labels
  annotate("text", x = 0.35, y = 0.72, label = "Yes", size = 3, color = "blue", fontface = "bold") +
  annotate("text", x = 0.64, y = 0.72, label = "No", size = 3, color = "red", fontface = "bold") +
  annotate("text", x = 0.65, y = 0.52, label = "Yes", size = 3, color = "blue", fontface = "bold") +
  annotate("text", x = 0.84, y = 0.52, label = "No", size = 3, color = "red", fontface = "bold") +

  scale_color_manual(values = c("decision" = "#2E86AB", "leaf" = "#F24236")) +

  labs(
    title = "Decision Tree Structure",
    subtitle = "Binary splits creating three terminal regions"
  ) +

  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none"
  ) +

  xlim(0, 1) + ylim(0.3, 0.9)

# Create residual analysis plot
residual_data <- sample_data %>%
  mutate(residual = y_true - y_predicted)

p3 <- residual_data %>%
  ggplot(aes(x = y_predicted, y = residual, color = region)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +

  scale_color_manual(name = "Region",
                     values = c("Region 1" = "#440154",
                                "Region 2" = "#31688e",
                                "Region 3" = "#fde725")) +

  labs(
    title = "Model Residuals by Region",
    subtitle = "Residuals = True y - Predicted y",
    x = "Predicted y",
    y = "Residuals",
    caption = "Horizontal line at y = 0 shows perfect prediction"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

# Create summary statistics plot
summary_stats <- sample_data %>%
  group_by(region) %>%
  summarise(
    n = n(),
    mean_y_true = mean(y_true),
    mean_y_pred = mean(y_predicted),
    mse = mean((y_true - y_predicted)^2),
    mae = mean(abs(y_true - y_predicted)),
    .groups = "drop"
  )

p4 <- summary_stats %>%
  select(region, mean_y_true, mean_y_pred) %>%
  pivot_longer(cols = c(mean_y_true, mean_y_pred),
               names_to = "type", values_to = "value") %>%
  mutate(type = case_when(
    type == "mean_y_true" ~ "True Mean",
    type == "mean_y_pred" ~ "Predicted"
  )) %>%
  ggplot(aes(x = region, y = value, fill = type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(value, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +

  scale_fill_manual(values = c("True Mean" = "#2E86AB", "Predicted" = "#F24236")) +

  labs(
    title = "True vs Predicted Means by Region",
    subtitle = "Comparison of actual and model-predicted values",
    x = "Region",
    y = "Mean y value",
    fill = "Type"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine all plots
combined_plot <- (p1 | p2) / (p3 | p4)

# Display the combined plot
print(combined_plot)

# Print summary statistics
cat("\n" %+% "="*60 %+% "\n")
cat("TREE-BASED REGRESSION MODEL SUMMARY\n")
cat("="*60 %+% "\n")

cat("\nPartition Rules:\n")
cat("Region 1: x₁ < 4\n")
cat("Region 2: x₁ ≥ 4 AND x₂ < 5\n")
cat("Region 3: x₁ ≥ 4 AND x₂ ≥ 5\n\n")

cat("Model Predictions (constant values):\n")
for (i in 1:nrow(summary_stats)) {
  region <- summary_stats$region[i]
  pred <- summary_stats$mean_y_pred[i]
  cat(sprintf("%s: ŷ = %.1f\n", region, pred))
}

cat("\nModel Performance by Region:\n")
print(summary_stats)

# Overall model performance
overall_mse <- mean((sample_data$y_true - sample_data$y_predicted)^2)
overall_mae <- mean(abs(sample_data$y_true - sample_data$y_predicted))
overall_r2 <- 1 - sum((sample_data$y_true - sample_data$y_predicted)^2) /
  sum((sample_data$y_true - mean(sample_data$y_true))^2)

cat(sprintf("\nOverall Model Performance:\n"))
cat(sprintf("MSE: %.3f\n", overall_mse))
cat(sprintf("MAE: %.3f\n", overall_mae))
cat(sprintf("R²: %.3f\n", overall_r2))

cat("\nModel Interpretation:\n")
cat("- This tree model creates rectangular partitions in the (x₁, x₂) space\n")
cat("- Each partition has a constant predicted value (the mean of training data in that region)\n")
cat("- Decision boundaries are axis-parallel (splits only on individual variables)\n")
cat("- The model is interpretable: prediction depends only on which region a point falls into\n")