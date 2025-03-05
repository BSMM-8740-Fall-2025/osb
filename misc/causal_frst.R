# created February 25, 2025

# Install and load necessary packages
if (!require("grf")) install.packages("grf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("fixest")) install.packages("fixest")
if (!require("plm")) install.packages("plm")
if (!require("reshape2")) install.packages("reshape2")
if (!require("gridExtra")) install.packages("gridExtra")
library(grf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fixest)
library(plm)
library(reshape2)
library(gridExtra)

set.seed(1234)

# Generate synthetic retail panel data
# Parameters
n_stores <- 50
n_products <- 4
n_weeks <- 52
n_obs <- n_stores * n_products * n_weeks

# Store characteristics (time-invariant)
stores <- data.frame(
  store_id = 1:n_stores,
  store_size = runif(n_stores, 10000, 50000),  # Store size in sq ft
  store_age = runif(n_stores, 1, 20),          # Store age in years
  income_area = rnorm(n_stores, 60000, 15000), # Average income in store area
  competition = rpois(n_stores, 3),            # Number of competitors nearby
  urban = rbinom(n_stores, 1, 0.7)             # Urban location flag
)

# Product characteristics (time-invariant)
products <- data.frame(
  product_id = 1:n_products,
  product_name = c("Cola", "Diet Cola", "Orange Soda", "Water"),
  category = c("Soda", "Soda", "Soda", "Water"),
  base_price = c(2.99, 2.99, 2.49, 1.49),      # Base price
  base_cost = c(1.50, 1.60, 1.30, 0.30),       # Base cost
  brand_strength = c(0.9, 0.7, 0.5, 0.4)       # Brand strength (0-1)
)

# Create panel structure
panel <- expand.grid(
  store_id = 1:n_stores,
  product_id = 1:n_products,
  week = 1:n_weeks
)

# Add time component (week of year, seasonality)
panel$month <- ((panel$week - 1) %/% 4) + 1
panel$season <- ((panel$month - 1) %/% 3) + 1
panel$seasonality <- sin(panel$week * 2 * pi / 52)  # Yearly seasonality

# Add store characteristics
panel <- merge(panel, stores, by = "store_id")

# Add product characteristics
panel <- merge(panel, products, by = "product_id")

# Create price variation
# 1. Base store-product specific price
panel$store_product_factor <- rnorm(n_stores * n_products, 0, 0.05)
panel$store_product_factor <- panel$store_product_factor[match(paste(panel$store_id, panel$product_id),
                                                               paste(rep(1:n_stores, each = n_products),
                                                                     rep(1:n_products, times = n_stores)))]

# 2. Temporal price variation - a mix of:
#    - Regular weekly fluctuations
#    - Occasional promotions
#    - Seasonal patterns
#    - Slight upward trend (inflation)

# Regular weekly price variations
panel$weekly_variation <- rnorm(nrow(panel), 0, 0.02)

# Promotions (with more promotions for soda than water)
promotion_prob <- ifelse(panel$category == "Soda", 0.08, 0.04)
panel$promotion <- rbinom(nrow(panel), 1, promotion_prob)
panel$promotion_depth <- ifelse(panel$promotion == 1,
                                runif(nrow(panel), 0.1, 0.3), 0)

# Seasonal adjustments
panel$seasonal_factor <- panel$seasonality * 0.03 * (panel$category == "Soda")

# Price trend (inflation)
panel$trend_factor <- panel$week * 0.0005  # Small upward trend

# Calculate final price
panel$price <- panel$base_price *
  (1 + panel$store_product_factor) *
  (1 + panel$weekly_variation) *
  (1 - panel$promotion_depth) *
  (1 + panel$seasonal_factor) *
  (1 + panel$trend_factor)

# Round prices to realistic values (e.g., $1.99 instead of $2.013)
panel$price <- round(panel$price * 4, 0) / 4  # Round to nearest 25 cents

# Generate true elasticities (for validation)
# Own-price elasticity varies by store and product characteristics
panel$true_own_elasticity <- -1.5 -  # Base elasticity
  0.5 * panel$brand_strength -  # Strong brands are less elastic
  0.3 * (panel$income_area > 70000) +  # Higher income = less elastic
  0.4 * (panel$competition > 3) +  # More competition = more elastic
  0.2 * panel$urban  # Urban areas slightly more elastic

# Generate demand based on price elasticity
# Base demand depends on store and product characteristics
panel$base_demand <- 100 * (panel$store_size / 30000) *  # Larger stores sell more
  (1 + 0.3 * panel$urban) *  # Urban stores sell more
  (1 + 0.1 * panel$seasonality) *  # Seasonal effects
  ifelse(panel$category == "Soda", 1, 1.2) *  # Category adjustment
  (1 + 0.05 * sin(panel$week * 2 * pi / 12))  # Monthly cycles

# Add noise to make it realistic
panel$demand_noise <- rnorm(nrow(panel), 0, 10)

# Generate cross-price elasticities based on product relationships
# Create a matrix of cross-elasticities
cross_elasticity_matrix <- matrix(
  c(
    # Cola  DietCola  OrangeSoda  Water
    0.00,    0.60,      0.30,     0.10,  # Cola
    0.60,    0.00,      0.25,     0.15,  # Diet Cola
    0.30,    0.20,      0.00,     0.05,  # Orange Soda
    0.05,    0.05,      0.02,     0.00   # Water
  ),
  nrow = n_products, byrow = TRUE
)

# For each observation, calculate the cross-price effects
panel$cross_price_effect <- 0

# Loop through each store and week to calculate cross-price effects
for (s in 1:n_stores) {
  for (w in 1:n_weeks) {
    # Get the current store and week observations
    current <- panel[panel$store_id == s & panel$week == w, ]

    # For each product, calculate cross-price effects
    for (p1 in 1:n_products) {
      for (p2 in 1:n_products) {
        if (p1 != p2) {
          # Get price of the other product
          other_price <- current$price[current$product_id == p2]

          # Calculate cross-price effect based on the other product's price
          cross_elasticity <- cross_elasticity_matrix[p1, p2]

          # Update cross-price effect
          idx <- which(panel$store_id == s & panel$week == w & panel$product_id == p1)
          panel$cross_price_effect[idx] <- panel$cross_price_effect[idx] +
            cross_elasticity * (other_price / current$base_price[current$product_id == p2] - 1)
        }
      }
    }
  }
}

# Calculate final quantity based on own-price elasticity and cross-price effects
panel$quantity <- panel$base_demand *
  (panel$price / panel$base_price)^panel$true_own_elasticity *
  (1 + panel$cross_price_effect) +
  panel$demand_noise

# Ensure quantities are positive and realistic
panel$quantity <- pmax(panel$quantity, 0)
panel$quantity <- round(panel$quantity)

# Calculate revenue
panel$revenue <- panel$price * panel$quantity

# Create lag price variables for each product within store
panel <- panel %>%
  arrange(store_id, product_id, week) %>%
  group_by(store_id, product_id) %>%
  mutate(
    lag_price = lag(price, 1),
    price_change = (price / lag_price) - 1
  ) %>%
  ungroup()

# Create competitor prices (other products in same store, same week)
panel_with_comp_prices <- panel %>%
  select(store_id, product_id, week, price) %>%
  pivot_wider(
    names_from = product_id,
    values_from = price,
    names_prefix = "price_prod_"
  ) %>%
  right_join(panel, by = c("store_id", "week"))

# Clean up names and remove self-price
for (p in 1:n_products) {
  panel_with_comp_prices[[paste0("price_prod_", p)]][panel_with_comp_prices$product_id == p] <- NA
}

# Rename for clarity
names(panel_with_comp_prices)[names(panel_with_comp_prices) == "price_prod_1"] <- "price_cola"
names(panel_with_comp_prices)[names(panel_with_comp_prices) == "price_prod_2"] <- "price_diet_cola"
names(panel_with_comp_prices)[names(panel_with_comp_prices) == "price_prod_3"] <- "price_orange_soda"
names(panel_with_comp_prices)[names(panel_with_comp_prices) == "price_prod_4"] <- "price_water"

# Calculate log values for elasticity estimation
panel_with_comp_prices <- panel_with_comp_prices %>%
  mutate(
    log_quantity = log(quantity),
    log_price = log(price),
    log_price_cola = log(price_cola),
    log_price_diet_cola = log(price_diet_cola),
    log_price_orange_soda = log(price_orange_soda),
    log_price_water = log(price_water)
  )

# Remove first week (due to lag creation)
panel_data <- panel_with_comp_prices %>%
  filter(!is.na(lag_price))

# Split data for estimation
train_indices <- sample(1:nrow(panel_data), 0.7 * nrow(panel_data))
train_data <- panel_data[train_indices, ]
test_data <- panel_data[-train_indices, ]

# -----------------------------------------------------
# 1. Traditional Fixed Effects Model for Comparison
# -----------------------------------------------------

# Set up panel data structure
pdata <- pdata.frame(panel_data, index = c("store_id", "product_id", "week"))

# Traditional fixed effects model for own-price elasticity
fe_model_own <- plm(
  log_quantity ~ log_price + seasonality + month,
  data = pdata,
  model = "within",
  effect = "twoways"  # Both individual and time fixed effects
)

summary(fe_model_own)

# Fixed effects model with cross-price effects
fe_model_cross <- plm(
  log_quantity ~ log_price + log_price_cola + log_price_diet_cola +
    log_price_orange_soda + log_price_water +
    seasonality + month,
  data = pdata,
  model = "within",
  effect = "twoways"
)

summary(fe_model_cross)

# Fixed effects model by product
fe_models_by_product <- list()
for (p in 1:n_products) {
  pdata_product <- pdata.frame(panel_data[panel_data$product_id == p, ],
                               index = c("store_id", "week"))

  # Define the formula based on product
  if (p == 1) {  # Cola
    formula <- log_quantity ~ log_price + log_price_diet_cola +
      log_price_orange_soda + log_price_water + seasonality + month
  } else if (p == 2) {  # Diet Cola
    formula <- log_quantity ~ log_price + log_price_cola +
      log_price_orange_soda + log_price_water + seasonality + month
  } else if (p == 3) {  # Orange Soda
    formula <- log_quantity ~ log_price + log_price_cola +
      log_price_diet_cola + log_price_water + seasonality + month
  } else {  # Water
    formula <- log_quantity ~ log_price + log_price_cola +
      log_price_diet_cola + log_price_orange_soda + seasonality + month
  }

  fe_models_by_product[[p]] <- plm(
    formula,
    data = pdata_product,
    model = "within",
    effect = "twoways"
  )
}

# Print results by product
for (p in 1:n_products) {
  cat("\nFixed Effects Results for", products$product_name[p], "\n")
  print(summary(fe_models_by_product[[p]]))
}

# -----------------------------------------------------
# 2. Causal Forest for Own-Price Elasticity
# -----------------------------------------------------

# Prepare data for causal forest
prepare_cf_data <- function(data, product_id = NULL) {
  # Filter for specific product if needed
  if (!is.null(product_id)) {
    data <- data[data$product_id == product_id, ]
  }

  # Select covariates
  X <- data %>%
    select(
      store_size, store_age, income_area, competition, urban,
      brand_strength, seasonality, month, season,
      lag_price  # Include past price
    ) %>%
    as.matrix()

  # Treatment (log price)
  W <- data$log_price

  # Outcome (log quantity)
  Y <- data$log_quantity

  return(list(X = X, W = W, Y = Y, data = data))
}

# Train causal forests for each product
cf_models <- list()
cf_predictions <- list()

for (p in 1:n_products) {
  p = 2
  # Prepare data
  product_name <- products$product_name[p]
  cat("\nTraining causal forest for", product_name, "\n")

  train_cf_data <- prepare_cf_data(train_data, p)

  # Train causal forest
  cf <- causal_forest(
    X = train_cf_data$X,
    Y = train_cf_data$Y,
    W = train_cf_data$W,
    num.trees = 2000,
    clusters = train_cf_data$data$store_id,  # Cluster by store
    honesty = TRUE,
    tune.parameters = "all"
  )

  # Store model
  cf_models[[p]] <- cf

  # Make predictions on test data
  test_cf_data <- prepare_cf_data(test_data, p)
  predictions <- predict(cf, test_cf_data$X, test_cf_data$W)

  # Negative of the predictions are the elasticities
  # (since we used log price and log quantity)
  elasticities <- -predictions$predictions

  # Store predictions with metadata
  test_product_data <- test_data[test_data$product_id == p, ]
  cf_predictions[[p]] <- cbind(
    test_product_data,
    predicted_elasticity = elasticities,
    true_elasticity = test_product_data$true_own_elasticity
  )

  # Print average elasticity
  cat("Average predicted elasticity:", mean(elasticities), "\n")
  cat("Average true elasticity:", mean(test_product_data$true_own_elasticity), "\n")

  # Calculate correlation between predicted and true elasticities
  correlation <- cor(elasticities, test_product_data$true_own_elasticity)
  cat("Correlation between predicted and true elasticities:", correlation, "\n")
}

# -----------------------------------------------------
# 3. Causal Forest for Cross-Price Elasticity
# -----------------------------------------------------

# Function to prepare data for cross-price elasticity
prepare_cross_data <- function(data, focal_product, cross_product) {
  # Filter for focal product
  data_filtered <- data[data$product_id == focal_product, ]

  # Select covariates
  X <- data_filtered %>%
    select(
      store_size, store_age, income_area, competition, urban,
      brand_strength, seasonality, month, season,
      price  # Include own price
    ) %>%
    as.matrix()

  # Treatment is log of cross-product price
  cross_price_col <- paste0("log_price_prod_", cross_product)
  W <- data_filtered[[paste0("log_price_prod_", cross_product)]]

  # Outcome is log quantity
  Y <- data_filtered$log_quantity

  # Filter out missing values (e.g., if products not in same store)
  valid_idx <- !is.na(W)
  X <- X[valid_idx, ]
  W <- W[valid_idx]
  Y <- Y[valid_idx]
  data_filtered <- data_filtered[valid_idx, ]

  return(list(X = X, W = W, Y = Y, data = data_filtered))
}

# Train cross-price elasticity models for key product pairs
cross_cf_models <- list()
cross_cf_predictions <- list()

# Define product pairs to analyze (based on expected cross-effects)
product_pairs <- list(
  c(1, 2),  # Cola vs Diet Cola
  c(2, 1),  # Diet Cola vs Cola
  c(1, 3),  # Cola vs Orange Soda
  c(3, 1)   # Orange Soda vs Cola
)

for (pair in product_pairs) {
  focal_product <- pair[1]
  cross_product <- pair[2]

  focal_name <- products$product_name[focal_product]
  cross_name <- products$product_name[cross_product]

  cat("\nTraining cross-price elasticity model for",
      focal_name, "with respect to", cross_name, "\n")

  # Prepare data
  train_cross_data <- prepare_cross_data(
    train_data, focal_product, cross_product
  )

  # Check if we have enough data
  if (length(train_cross_data$Y) < 100) {
    cat("Not enough data for this product pair. Skipping.\n")
    next
  }

  # Train causal forest
  cf_cross <- causal_forest(
    X = train_cross_data$X,
    Y = train_cross_data$Y,
    W = train_cross_data$W,
    num.trees = 2000,
    clusters = train_cross_data$data$store_id,  # Cluster by store
    honesty = TRUE,
    tune.parameters = "all"
  )

  # Store model
  key <- paste(focal_product, cross_product, sep = "_")
  cross_cf_models[[key]] <- cf_cross

  # Make predictions on test data
  test_cross_data <- prepare_cross_data(
    test_data, focal_product, cross_product
  )

  if (length(test_cross_data$Y) < 10) {
    cat("Not enough test data for this product pair. Skipping.\n")
    next
  }

  cross_predictions <- predict(cf_cross, test_cross_data$X, test_cross_data$W)

  # The predictions are the cross-price elasticities
  # (positive for substitutes, negative for complements)
  cross_elasticities <- cross_predictions$predictions

  # Store predictions
  cross_cf_predictions[[key]] <- cbind(
    test_cross_data$data,
    predicted_cross_elasticity = cross_elasticities,
    true_cross_elasticity = cross_elasticity_matrix[focal_product, cross_product]
  )

  # Print average cross-price elasticity
  cat("Average predicted cross-price elasticity:", mean(cross_elasticities), "\n")
  cat("True cross-price elasticity:",
      cross_elasticity_matrix[focal_product, cross_product], "\n")
}

# -----------------------------------------------------
# 4. Analysis and Visualization
# -----------------------------------------------------

# Combine all own-price elasticity predictions
all_elasticity_predictions <- do.call(rbind, cf_predictions)

# Plot overall distribution of elasticities by product
ggplot(all_elasticity_predictions, aes(x = predicted_elasticity, fill = factor(product_id))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ product_name, scales = "free") +
  labs(title = "Distribution of Predicted Price Elasticities by Product",
       x = "Price Elasticity",
       y = "Density",
       fill = "Product") +
  theme_minimal()

# Plot predicted vs true elasticities
ggplot(all_elasticity_predictions, aes(x = true_elasticity, y = predicted_elasticity, color = factor(product_id))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ product_name) +
  labs(title = "Predicted vs True Price Elasticities",
       x = "True Elasticity",
       y = "Predicted Elasticity",
       color = "Product") +
  theme_minimal()

# Analyze elasticity by store characteristics
store_elasticity <- all_elasticity_predictions %>%
  group_by(store_id, product_id, product_name, store_size, income_area, competition, urban) %>%
  summarize(
    avg_elasticity = mean(predicted_elasticity, na.rm = TRUE),
    .groups = "drop"
  )

# Plot elasticity by store size
ggplot(store_elasticity, aes(x = store_size, y = avg_elasticity, color = factor(product_id))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ product_name) +
  labs(title = "Price Elasticity by Store Size",
       x = "Store Size (sq ft)",
       y = "Average Elasticity",
       color = "Product") +
  theme_minimal()

# Plot elasticity by income area
ggplot(store_elasticity, aes(x = income_area, y = avg_elasticity, color = factor(product_id))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ product_name) +
  labs(title = "Price Elasticity by Area Income",
       x = "Average Area Income ($)",
       y = "Average Elasticity",
       color = "Product") +
  theme_minimal()

# Plot elasticity by competition level
ggplot(store_elasticity, aes(x = factor(competition), y = avg_elasticity, color = factor(product_id))) +
  geom_boxplot() +
  facet_wrap(~ product_name) +
  labs(title = "Price Elasticity by Competition Level",
       x = "Number of Competitors",
       y = "Average Elasticity",
       color = "Product") +
  theme_minimal()

# Plot elasticity by urban/rural
ggplot(store_elasticity, aes(x = factor(urban, labels = c("Rural", "Urban")),
                             y = avg_elasticity, color = factor(product_id))) +
  geom_boxplot() +
  facet_wrap(~ product_name) +
  labs(title = "Price Elasticity by Store Location",
       x = "Location Type",
       y = "Average Elasticity",
       color = "Product") +
  theme_minimal()

# Create elasticity heatmap by store characteristic segments
store_segments <- all_elasticity_predictions %>%
  mutate(
    size_segment = cut(store_size, breaks = c(0, 20000, 35000, Inf),
                       labels = c("Small", "Medium", "Large")),
    income_segment = cut(income_area, breaks = c(0, 45000, 75000, Inf),
                         labels = c("Low", "Medium", "High")),
    competition_segment = factor(competition <= 3, labels = c("High Competition", "Low Competition"))
  ) %>%
  group_by(product_id, product_name, size_segment, income_segment, competition_segment) %>%
  summarize(
    avg_elasticity = mean(predicted_elasticity, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count >= 10)  # Ensure sufficient sample in each segment

# Create heatmap for Cola
cola_heatmap <- store_segments %>%
  filter(product_id == 1) %>%
  ggplot(aes(x = income_segment, y = size_segment, fill = avg_elasticity)) +
  geom_tile() +
  facet_wrap(~ competition_segment) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = -1.5) +
  labs(title = "Cola Price Elasticity by Store Segments",
       x = "Income Level",
       y = "Store Size",
       fill = "Elasticity") +
  theme_minimal()

# Create heatmap for Diet Cola
diet_cola_heatmap <- store_segments %>%
  filter(product_id == 2) %>%
  ggplot(aes(x = income_segment, y = size_segment, fill = avg_elasticity)) +
  geom_tile() +
  facet_wrap(~ competition_segment) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = -1.5) +
  labs(title = "Diet Cola Price Elasticity by Store Segments",
       x = "Income Level",
       y = "Store Size",
       fill = "Elasticity") +
  theme_minimal()

# Combine heatmaps
grid.arrange(cola_heatmap, diet_cola_heatmap, ncol = 1)

# Analyze cross-price elasticities
cross_elasticity_results <- list()
for (key in names(cross_cf_predictions)) {
  parts <- strsplit(key, "_")[[1]]
  focal_id <- as.numeric(parts[1])
  cross_id <- as.numeric(parts[2])

  focal_name <- products$product_name[focal_id]
  cross_name <- products$product_name[cross_id]

  # Calculate average cross-price elasticity
  avg_cross_elasticity <- mean(cross_cf_predictions[[key]]$predicted_cross_elasticity)

  cross_elasticity_results[[key]] <- data.frame(
    focal_product = focal_name,
    cross_product = cross_name,
    avg_cross_elasticity = avg_cross_elasticity
  )
}

# Combine cross-elasticity results
cross_elasticity_df <- do.call(rbind, cross_elasticity_results)
print(cross_elasticity_df)

# Create cross-price elasticity matrix for visualization
cross_matrix <- matrix(0, nrow = n_products, ncol = n_products)
for (key in names(cross_cf_predictions)) {
  parts <- strsplit(key, "_")[[1]]
  focal_id <- as.numeric(parts[1])
  cross_id <- as.numeric(parts[2])

  cross_matrix[focal_id, cross_id] <- mean(cross_cf_predictions[[key]]$predicted_cross_elasticity)
}

# Cross-price elasticity heatmap
cross_df <- as.data.frame(cross_matrix)
colnames(cross_df) <- products$product_name
rownames(cross_df) <- products$product_name

cross_df_long <- cross_df %>%
  as.matrix() %>%
  melt() %>%
  rename(focal_product = Var1, cross_product = Var2, elasticity = value)

# Plot cross-price elasticity heatmap
ggplot(cross_df_long, aes(x = cross_product, y = focal_product, fill = elasticity)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Cross-Price Elasticity Matrix",
       subtitle = "How price of column product affects demand for row product",
       x = "Price of Product",
       y = "Demand of Product",
       fill = "Cross-Price\nElasticity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------
# 5. Store-Level Price Optimization
# -----------------------------------------------------

# Function to predict optimal prices for a store
optimize_store_prices <- function(store_id, week, models, products) {
  # Get store data
  store_data <- panel_data[panel_data$store_id == store_id &
                             panel_data$week == week, ]

  # Current prices and quantities
  current_prices <- sapply(1:n_products, function(p) {
    store_data$price[store_data$product_id == p]
  })

  current_quantities <- sapply(1:n_products, function(p) {
    store_data$quantity[store_data$product_id == p]
  })

  current_revenue <- sum(current_prices * current_quantities)

  # Get store characteristics
  store_chars <- store_data[1, c("store_size", "store_age", "income_area",
                                 "competition", "urban")]

  # Extract current elasticities
  elasticities <- sapply(1:n_products, function(p) {
    # Prepare data for prediction
    pred_data <- cbind(
      store_chars,
      brand_strength = products$brand_strength[p],
      seasonality = store_data$seasonality[1],
      month = store_data$month[1],
      season = store_data$season[1],
      lag_price = current_prices[p],
      price = current_prices[p]
    )

    # Predict using the model
    X_pred <- as.matrix(pred_data[, names(pred_data) %in% colnames(models[[p]]$X.orig)])
    W_pred <- log(current_prices[p])

    # Predict elasticity (note the negative sign since elasticity is negative of the derivative)
    elasticity <- -predict(models[[p]], X_pred, W_pred)$predictions

    return(elasticity)
  })

  # Get cross-price elasticities (simplified - using averages from our analysis)
  cross_elasticities <- matrix(0, nrow = n_products, ncol = n_products)
  for (key in names(cross_cf_predictions)) {
    parts <- strsplit(key, "_")[[1]]
    focal_id <- as.numeric(parts[1])
    cross_id <- as.numeric(parts[2])

    cross_elasticities[focal_id, cross_id] <- mean(cross_cf_predictions[[key]]$predicted_cross_elasticity)
  }

  # Function to simulate revenue at different price points
  simulate_revenue <- function(price_multipliers) {
    # Apply multipliers to current prices
    new_prices <- current_prices * price_multipliers

    # Predict quantities based on own-price and cross-price elasticities
    new_quantities <- current_quantities

    for (p in 1:n_products) {
      # Own-price effect
      price_ratio <- new_prices[p] / current_prices[p]
      own_effect <- price_ratio^elasticities[p]

      # Cross-price effects
      cross_effect <- 1
      for (cp in 1:n_products) {
        if (p != cp) {
          cross_price_ratio <- new_prices[cp] / current_prices[cp]
          cross_effect <- cross_effect * cross_price_ratio^cross_elasticities[p, cp]
        }
      }

      # Update quantity
      new_quantities[p] <- current_quantities[p] * own_effect * cross_effect
    }

    # Calculate revenue
    revenue <- sum(new_prices * new_quantities)
    return(revenue)
  }

  # Grid search for optimal price multipliers
  multiplier_grid <- seq(0.8, 1.2, by = 0.05)

  best_revenue <- current_revenue
  best_multipliers <- rep(1, n_products)

  # For each product, find best price holding others constant
  for (iterations in 1:3) {  # Iterative optimization
    for (p in 1:n_products) {
      max_revenue <- -Inf
      best_mult <- 1

      for (mult in multiplier_grid) {
        test_multipliers <- best_multipliers
        test_multipliers[p] <- mult

        test_revenue <- simulate_revenue(test_multipliers)

        if (test_revenue > max_revenue) {
          max_revenue <- test_revenue
          best_mult <- mult
        }
      }

      best_multipliers[p] <- best_mult
      best_revenue <- max_revenue
    }
  }

  # Results
  result <- data.frame(
    store_id = store_id,
    week = week,
    current_revenue = current_revenue,
    optimized_revenue = best_revenue,
    revenue_lift_pct = (best_revenue / current_revenue - 1) * 100
  )

  # Add current and optimal prices for each product
  for (p in 1:n_products) {
    result[[paste0("current_price_", products$product_name[p])]] <- current_prices[p]
    result[[paste0("optimal_price_", products$product_name[p])]] <- current_prices[p] * best_multipliers[p]
    result[[paste0("price_change_pct_", products$product_name[p])]] <- (best_multipliers[p] - 1) * 100
  }

  return(result)
}

# Optimize prices for a sample of stores
sample_stores <- sample(unique(panel_data$store_id), 10)
optimization_results <- data.frame()

for (s in sample_stores) {
  # Use the latest week
  latest_week <- max(panel_data$week[panel_data$store_id == s])

  # Optimize prices
  result <- optimize_store_prices(s, latest_week, cf_models, products)
  optimization_results <- rbind(optimization_results, result)
}

# Analyze optimization results
print("Price Optimization Results:")
print(optimization_results[, c("store_id", "revenue_lift_pct")])

# Plot revenue lift distribution
ggplot(optimization_results, aes(x = revenue_lift_pct)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "black") +
  geom_vline(xintercept = mean(optimization_results$revenue_lift_pct),
             linetype = "dashed", color = "red", size = 1) +
  labs(title = "Potential Revenue Lift from Price Optimization",
       x = "Revenue Lift (%)",
       y = "Count") +
  theme_minimal()

# Plot price change recommendations
price_changes <- optimization_results %>%
  pivot_longer(
    cols = starts_with("price_change_pct_"),
    names_to = "product",
    values_to = "price_change_pct"
  ) %>%
  mutate(product = gsub("price_change_pct_", "", product))

ggplot(price_changes, aes(x = product, y = price_change_pct, fill = product)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Recommended Price Changes by Product",
       x = "Product",
       y = "Price Change (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------
# 6. Additional Analysis and Applications
# -----------------------------------------------------

# Calculate elasticity seasonal patterns
seasonal_elasticity <- all_elasticity_predictions %>%
  group_by(product_id, product_name, month) %>%
  summarize(
    avg_elasticity = mean(predicted_elasticity),
    .groups = "drop"
  )

# Plot seasonal elasticity patterns
ggplot(seasonal_elasticity, aes(x = month, y = avg_elasticity, color = product_name, group = product_name)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Seasonal Patterns in Price Elasticity",
       x = "Month",
       y = "Average Price Elasticity",
       color = "Product") +
  theme_minimal()

# Create price elasticity segments for targeting
elasticity_segments <- all_elasticity_predictions %>%
  mutate(
    elasticity_segment = cut(predicted_elasticity,
                             breaks = c(-Inf, -2, -1.5, -1, 0),
                             labels = c("Highly Elastic", "Moderately Elastic",
                                        "Less Elastic", "Inelastic"))
  ) %>%
  group_by(store_id, product_id, product_name, elasticity_segment) %>%
  summarize(
    avg_price = mean(price),
    avg_quantity = mean(quantity),
    avg_revenue = mean(revenue),
    count = n(),
    .groups = "drop"
  )

# Plot revenue by elasticity segment
ggplot(elasticity_segments, aes(x = elasticity_segment, y = avg_revenue, fill = product_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Revenue by Elasticity Segment and Product",
       x = "Elasticity Segment",
       y = "Average Revenue",
       fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Example application: Dynamic pricing strategy
# Create a function that recommends price strategy based on elasticity
get_pricing_strategy <- function(elasticity) {
  if (elasticity < -2) {
    return("Value-based pricing - Highly price sensitive customers")
  } else if (elasticity < -1.5) {
    return("Competitive pricing - Track market closely")
  } else if (elasticity < -1) {
    return("Premium pricing - Moderate price sensitivity")
  } else {
    return("Premium-plus pricing - Low price sensitivity")
  }
}

# Apply to store-product combinations
pricing_strategies <- store_elasticity %>%
  mutate(
    recommended_strategy = sapply(avg_elasticity, get_pricing_strategy)
  )

# Count by strategy and product
strategy_counts <- pricing_strategies %>%
  group_by(product_name, recommended_strategy) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  group_by(product_name) %>%
  mutate(
    percentage = count / sum(count) * 100
  )

# Plot strategy distribution
ggplot(strategy_counts, aes(x = product_name, y = percentage, fill = recommended_strategy)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Recommended Pricing Strategies by Product",
       x = "Product",
       y = "Percentage of Stores",
       fill = "Pricing Strategy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Final insights: Elasticity by store type
store_elasticity_summary <- store_elasticity %>%
  mutate(
    store_type = case_when(
      income_area > 70000 & store_size > 30000 ~ "Large High-Income",
      income_area > 70000 & store_size <= 30000 ~ "Small High-Income",
      income_area <= 70000 & store_size > 30000 ~ "Large Average-Income",
      TRUE ~ "Small Average-Income"
    )
  ) %>%
  group_by(store_type, product_name) %>%
  summarize(
    avg_elasticity = mean(avg_elasticity),
    count = n(),
    .groups = "drop"
  )

# Plot elasticity by store type
ggplot(store_elasticity_summary, aes(x = store_type, y = avg_elasticity, fill = product_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Price Elasticity by Store Type and Product",
       x = "Store Type",
       y = "Average Price Elasticity",
       fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print final insights and recommendations
cat("\n--- Key Insights from Price Elasticity Analysis ---\n")

cat("\n1. Product-specific elasticities:\n")
for (p in 1:n_products) {
  product_name <- products$product_name[p]
  avg_elasticity <- mean(all_elasticity_predictions$predicted_elasticity[
    all_elasticity_predictions$product_id == p])

  cat("  - ", product_name, ": Average elasticity =", round(avg_elasticity, 2), "\n")
}

cat("\n2. Cross-price relationships:\n")
for (i in 1:nrow(cross_elasticity_df)) {
  focal <- cross_elasticity_df$focal_product[i]
  cross <- cross_elasticity_df$cross_product[i]
  elasticity <- cross_elasticity_df$avg_cross_elasticity[i]

  relationship <- if (elasticity > 0) "substitute" else "complement"

  cat("  - ", focal, "and", cross, "are", relationship,
      "(cross-elasticity =", round(elasticity, 2), ")\n")
}

cat("\n3. Store characteristics with strongest impact on price sensitivity:\n")
var_imp_df <- data.frame()
for (p in 1:n_products) {
  imp <- data.frame(
    Variable = colnames(cf_models[[p]]$X.orig),
    Importance = variable_importance(cf_models[[p]]),
    Product = products$product_name[p]
  )
  var_imp_df <- rbind(var_imp_df, imp)
}

top_vars <- var_imp_df %>%
  group_by(Product) %>%
  top_n(3, Importance) %>%
  arrange(Product, desc(Importance))

print(top_vars)

cat("\n4. Optimal pricing recommendations:\n")
cat("  - Average potential revenue lift:",
    round(mean(optimization_results$revenue_lift_pct), 1), "%\n")

for (p in 1:n_products) {
  product_name <- products$product_name[p]
  avg_change <- mean(optimization_results[[paste0("price_change_pct_", product_name)]])

  direction <- if (avg_change > 0) "increase" else "decrease"

  cat("  - ", product_name, ": Recommend", direction, "prices by",
      round(abs(avg_change), 1), "% on average\n")
}

cat("\n5. Seasonal pricing opportunities:\n")
seasonal_insights <- seasonal_elasticity %>%
  group_by(product_name) %>%
  summarize(
    highest_elasticity_month = month[which.min(avg_elasticity)],
    lowest_elasticity_month = month[which.max(avg_elasticity)],
    elasticity_range = max(avg_elasticity) - min(avg_elasticity),
    .groups = "drop"
  )

for (i in 1:nrow(seasonal_insights)) {
  product <- seasonal_insights$product_name[i]
  high_month <- seasonal_insights$highest_elasticity_month[i]
  low_month <- seasonal_insights$lowest_elasticity_month[i]
  range <- seasonal_insights$elasticity_range[i]

  cat("  - ", product, ": Most elastic in month", high_month,
      "and least elastic in month", low_month,
      "(range =", round(range, 2), ")\n")
}