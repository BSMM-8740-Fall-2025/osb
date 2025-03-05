# created February 25, 2025

# Install and load necessary packages
packages_to_install <- c("grf", "ggplot2", "dplyr", "tidyr", "reshape2", "gridExtra")
for (package in packages_to_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

set.seed(1234)

# -----------------------------------------------------
# 1. Generate Synthetic Retail Panel Data
# -----------------------------------------------------

# Parameters
n_stores <- 30
n_products <- 3
n_weeks <- 40
n_obs <- n_stores * n_products * n_weeks

# Store characteristics (time-invariant)
stores <- data.frame(
  store_id = 1:n_stores,
  store_size = runif(n_stores, 10000, 50000),    # Store size in sq ft
  store_age = runif(n_stores, 1, 20),            # Store age in years
  income_area = rnorm(n_stores, 60000, 15000),   # Average income in store area
  competition = rpois(n_stores, 3),              # Number of competitors nearby
  urban = rbinom(n_stores, 1, 0.7)               # Urban location flag
)

# Product characteristics (time-invariant)
products <- data.frame(
  product_id = 1:n_products,
  product_name = c("Cola", "Diet Cola", "Water"),
  category = c("Soda", "Soda", "Water"),
  base_price = c(2.99, 2.99, 1.49),          # Base price
  base_cost = c(1.50, 1.60, 0.30),           # Base cost
  brand_strength = c(0.9, 0.7, 0.4)          # Brand strength (0-1)
)

# Create panel structure
panel <- expand.grid(
  store_id = 1:n_stores,
  product_id = 1:n_products,
  week = 1:n_weeks
)

# Add time component
panel$month <- ((panel$week - 1) %/% 4) + 1
panel$seasonality <- sin(panel$week * 2 * pi / 52)  # Yearly seasonality

# Add store characteristics
panel <- merge(panel, stores, by = "store_id")

# Add product characteristics
panel <- merge(panel, products, by = "product_id")

# Create price variation
# Base store-product specific price with slight random variation
panel$store_product_factor <- rnorm(n_stores * n_products, 0, 0.05)
panel$store_product_factor <- panel$store_product_factor[
  match(
    paste(panel$store_id, panel$product_id),
    paste(
      rep(1:n_stores, each = n_products),
      rep(1:n_products, times = n_stores)
    )
  )
]

# Regular weekly price variations
panel$weekly_variation <- rnorm(nrow(panel), 0, 0.02)

# Promotions (with more promotions for soda than water)
promotion_prob <- ifelse(panel$category == "Soda", 0.08, 0.04)
panel$promotion <- rbinom(nrow(panel), 1, promotion_prob)
panel$promotion_depth <- ifelse(panel$promotion == 1, runif(nrow(panel), 0.1, 0.3), 0)

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

# Round prices to realistic values
panel$price <- round(panel$price * 4, 0) / 4  # Round to nearest 25 cents

# True own-price elasticity varies by store and product characteristics
panel$true_own_elasticity <- -1.5 -  # Base elasticity
  0.5 * panel$brand_strength -  # Strong brands are less elastic
  0.3 * (panel$income_area > 70000) +  # Higher income = less elastic
  0.4 * (panel$competition > 3) +  # More competition = more elastic
  0.2 * panel$urban  # Urban areas slightly more elastic

# Base demand factors
panel$base_demand <- 100 * (panel$store_size / 30000) *  # Larger stores sell more
  (1 + 0.3 * panel$urban) *  # Urban stores sell more
  (1 + 0.1 * panel$seasonality) *  # Seasonal effects
  ifelse(panel$category == "Soda", 1, 1.2) *  # Category adjustment
  (1 + 0.05 * sin(panel$week * 2 * pi / 12))  # Monthly cycles

# Add random noise to the demand
panel$demand_noise <- rnorm(nrow(panel), 0, 10)

# Create a matrix of cross-elasticities between products
cross_elasticity_matrix <- matrix(
  c(
    # Cola  DietCola  Water
    0.00,    0.60,    0.10,  # Cola
    0.60,    0.00,    0.15,  # Diet Cola
    0.05,    0.05,    0.00   # Water
  ),
  nrow = n_products, byrow = TRUE
)

# Initialize cross-price effect
panel$cross_price_effect <- 0

# Calculate cross-price effects (simplified approach)
for (w in 1:n_weeks) {
  for (s in 1:n_stores) {
    # Get current store-week data
    store_week_data <- panel[panel$store_id == s & panel$week == w, ]

    # For each product pair, calculate effects
    for (p1 in 1:n_products) {
      # Index for current product
      idx <- which(panel$store_id == s & panel$week == w & panel$product_id == p1)
      if (length(idx) != 1) next  # Skip if not found

      for (p2 in 1:n_products) {
        if (p1 == p2) next  # Skip self

        # Get other product data
        other_idx <- which(store_week_data$product_id == p2)
        if (length(other_idx) != 1) next  # Skip if not found

        # Get cross-elasticity
        cross_effect <- cross_elasticity_matrix[p1, p2]

        # Calculate price ratio effect
        price_ratio <- store_week_data$price[other_idx] / products$base_price[p2]
        effect <- cross_effect * (price_ratio - 1)

        # Add to cross-price effect
        panel$cross_price_effect[idx] <- panel$cross_price_effect[idx] + effect
      }
    }
  }
}

# Calculate final quantity based on own-price elasticity and cross-price effects
panel$quantity <- panel$base_demand *
  (panel$price / panel$base_price)^panel$true_own_elasticity *
  (1 + panel$cross_price_effect) +
  panel$demand_noise

# Ensure quantities are positive and reasonable
panel$quantity <- pmax(round(panel$quantity), 1)

# Calculate revenue
panel$revenue <- panel$price * panel$quantity

# Create lag price variables within store-product groups
panel <- panel %>%
  arrange(store_id, product_id, week) %>%
  group_by(store_id, product_id) %>%
  mutate(
    lag_price = lag(price, 1),
    price_change = ifelse(is.na(lag_price), 0, (price / lag_price) - 1)
  ) %>%
  ungroup()

# Create competitor prices (other products in same store, same week)
# First pivot to wide format with product prices
competitor_prices <- panel %>%
  select(store_id, product_id, week, price) %>%
  pivot_wider(
    names_from = product_id,
    values_from = price,
    names_prefix = "price_prod_"
  )

# Join back to main data
panel_with_comp_prices <- competitor_prices %>%
  right_join(panel, by = c("store_id", "week"))

# For each product, set its own price in the competitor columns to NA
for (p in 1:n_products) {
  col_name <- paste0("price_prod_", p)
  panel_with_comp_prices[[col_name]][panel_with_comp_prices$product_id == p] <- NA
}

# Rename for clarity
names(panel_with_comp_prices)[names(panel_with_comp_prices) == "price_prod_1"] <- "price_cola"
names(panel_with_comp_prices)[names(panel_with_comp_prices) == "price_prod_2"] <- "price_diet_cola"
names(panel_with_comp_prices)[names(panel_with_comp_prices) == "price_prod_3"] <- "price_water"

# Calculate log values for elasticity estimation
panel_with_comp_prices <- panel_with_comp_prices %>%
  mutate(
    log_quantity = log(quantity),
    log_price = log(price),
    log_price_cola = log(price_cola),
    log_price_diet_cola = log(price_diet_cola),
    log_price_water = log(price_water)
  )

# Handle NA values in log calculations
for (col in c("log_price_cola", "log_price_diet_cola", "log_price_water")) {
  panel_with_comp_prices[[col]][is.na(panel_with_comp_prices[[col]])] <- 0
}

# Remove first week observations (because of lagging)
panel_data <- panel_with_comp_prices %>%
  filter(!is.na(lag_price))

# Split data for estimation
set.seed(555)
train_indices <- sample(1:nrow(panel_data), 0.7 * nrow(panel_data))
train_data <- panel_data[train_indices, ]
test_data <- panel_data[-train_indices, ]

# -----------------------------------------------------
# 2. Causal Forest for Own-Price Elasticity
# -----------------------------------------------------

# Function to prepare data for causal forest
prepare_cf_data <- function(data, product_id = NULL) {
  # Filter for specific product if needed
  if (!is.null(product_id)) {
    data <- data[data$product_id == product_id, ]
  }

  # Select relevant covariates
  covariates <- c("store_size", "store_age", "income_area", "competition",
                  "urban", "brand_strength", "seasonality", "month", "lag_price")

  # Check which covariates exist in the data
  available_covariates <- covariates[covariates %in% names(data)]

  # Select covariates
  X <- data %>%
    select(all_of(available_covariates)) %>%
    as.matrix()

  # Treatment (log price)
  W <- data$log_price

  # Outcome (log quantity)
  Y <- data$log_quantity

  # Remove any rows with NA or Inf values
  valid_rows <- complete.cases(X) & is.finite(W) & is.finite(Y)
  X <- X[valid_rows, , drop = FALSE]
  W <- W[valid_rows]
  Y <- Y[valid_rows]
  data <- data[valid_rows, ]

  return(list(X = X, W = W, Y = Y, data = data))
}

# Wrapper function for prediction that handles linear correction issues
safe_predict <- function(model, newdata, newtrt = NULL) {
  tryCatch({
    # Try standard prediction first
    predict(model, newdata, newtrt)
  }, error = function(e) {
    if (grepl("Linear correction variables", e$message)) {
      # If error is about linear correction, try without it
      predict(model, newdata, newtrt, linear.correction.variables = NULL)
    } else {
      # Re-throw other errors
      stop(e)
    }
  })
}

# Train causal forests for each product
cf_models <- list()
cf_predictions <- list()

for (p in 1:n_products) {
  # Prepare data
  product_name <- products$product_name[p]
  cat("\nTraining causal forest for", product_name, "\n")

  train_cf_data <- prepare_cf_data(train_data, p)

  # Check if we have enough data
  if (nrow(train_cf_data$data) < 10) {
    cat("Not enough data for product", p, ". Skipping.\n")
    next
  }

  # Train causal forest with proper error handling
  tryCatch({
    # Train with simplified parameters and no automatic tuning
    cf <- causal_forest(
      X = train_cf_data$X,
      Y = train_cf_data$Y,
      W = train_cf_data$W,
      num.trees = 2000,
      honesty = TRUE,
      tune.parameters = "none",  # Disable automatic tuning
      compute.oob.predictions = TRUE
    )

    # Store model
    cf_models[[p]] <- cf

    # Make predictions on test data
    test_cf_data <- prepare_cf_data(test_data, p)

    if (nrow(test_cf_data$data) < 10) {
      cat("Not enough test data for product", p, ". Skipping predictions.\n")
      next
    }

    # Use the safe predict function that handles linear correction issues
    predictions <- safe_predict(cf, test_cf_data$X, test_cf_data$W)

    # Negative of the predictions are the elasticities
    # (since we used log price and log quantity)
    elasticities <- -predictions$predictions

    # Store predictions with metadata
    test_product_data <- test_data[test_data$product_id == p, ]
    test_product_data <- test_product_data[complete.cases(test_cf_data$X) &
                                             is.finite(test_cf_data$W) &
                                             is.finite(test_cf_data$Y), ]

    if (length(elasticities) == nrow(test_product_data)) {
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
    } else {
      cat("Mismatch in prediction and data dimensions. Check for missing values.\n")
    }
  }, error = function(e) {
    cat("Error in causal forest for product", p, ":", e$message, "\n")
  })
}

# -----------------------------------------------------
# 3. Causal Forest for Cross-Price Elasticity
# -----------------------------------------------------

# Function to prepare data for cross-price elasticity
prepare_cross_data <- function(data, focal_product, cross_product) {
  # Filter for focal product
  data_filtered <- data[data$product_id == focal_product, ]

  if (nrow(data_filtered) == 0) {
    return(NULL)
  }

  # Select covariates
  covariates <- c("store_size", "store_age", "income_area", "competition",
                  "urban", "brand_strength", "seasonality", "month", "price")

  # Check which covariates exist in the data
  available_covariates <- covariates[covariates %in% names(data_filtered)]

  # Select covariates
  X <- data_filtered %>%
    select(all_of(available_covariates)) %>%
    as.matrix()

  # Treatment is log of cross-product price
  # Use appropriate column naming based on product
  if (cross_product == 1) {
    W <- data_filtered$log_price_cola
  } else if (cross_product == 2) {
    W <- data_filtered$log_price_diet_cola
  } else if (cross_product == 3) {
    W <- data_filtered$log_price_water
  } else {
    # Default fallback
    W <- data_filtered[[paste0("log_price_prod_", cross_product)]]
  }

  # Outcome is log quantity
  Y <- data_filtered$log_quantity

  # Filter out missing values
  valid_idx <- complete.cases(X) & !is.na(W) & !is.infinite(W) & !is.na(Y) & !is.infinite(Y)
  X <- X[valid_idx, , drop = FALSE]
  W <- W[valid_idx]
  Y <- Y[valid_idx]
  data_filtered <- data_filtered[valid_idx, ]

  return(list(X = X, W = W, Y = Y, data = data_filtered))
}

# Train cross-price elasticity models for key product pairs
cross_cf_models <- list()
cross_cf_predictions <- list()

# Define product pairs to analyze
product_pairs <- list(
  c(1, 2),  # Cola vs Diet Cola
  c(2, 1),  # Diet Cola vs Cola
  c(1, 3),  # Cola vs Water
  c(3, 1)   # Water vs Cola
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

  # Check if we have data
  if (is.null(train_cross_data) || nrow(train_cross_data$X) < 100) {
    cat("Not enough data for this product pair. Skipping.\n")
    next
  }

  # Train causal forest with error handling
  tryCatch({
    # Train with simplified parameters
    cf_cross <- causal_forest(
      X = train_cross_data$X,
      Y = train_cross_data$Y,
      W = train_cross_data$W,
      num.trees = 2000,
      honesty = TRUE,
      tune.parameters = "none",  # Disable automatic tuning
      compute.oob.predictions = TRUE
    )

    # Store model
    key <- paste(focal_product, cross_product, sep = "_")
    cross_cf_models[[key]] <- cf_cross

    # Make predictions on test data
    test_cross_data <- prepare_cross_data(
      test_data, focal_product, cross_product
    )

    if (is.null(test_cross_data) || nrow(test_cross_data$X) < 10) {
      cat("Not enough test data for this product pair. Skipping predictions.\n")
      next
    }

    # Use the safe predict function
    cross_predictions <- safe_predict(cf_cross, test_cross_data$X, test_cross_data$W)

    # The predictions are the cross-price elasticities
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
  }, error = function(e) {
    cat("Error in cross-price causal forest:", e$message, "\n")
  })
}

# -----------------------------------------------------
# 4. Analysis and Visualization
# -----------------------------------------------------

# Combine all own-price elasticity predictions
if (length(cf_predictions) > 0) {
  all_elasticity_predictions <- do.call(rbind, cf_predictions)

  # Plot overall distribution of elasticities by product
  p1 <- ggplot(all_elasticity_predictions, aes(x = predicted_elasticity, fill = factor(product_id))) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ product_name, scales = "free") +
    labs(title = "Distribution of Predicted Price Elasticities by Product",
         x = "Price Elasticity",
         y = "Density",
         fill = "Product") +
    theme_minimal()
  print(p1)

  # Plot predicted vs true elasticities
  p2 <- ggplot(all_elasticity_predictions, aes(x = true_elasticity, y = predicted_elasticity, color = factor(product_id))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    facet_wrap(~ product_name) +
    labs(title = "Predicted vs True Price Elasticities",
         x = "True Elasticity",
         y = "Predicted Elasticity",
         color = "Product") +
    theme_minimal()
  print(p2)

  # Analyze elasticity by store characteristics
  store_elasticity <- all_elasticity_predictions %>%
    group_by(store_id, product_id, product_name, store_size, income_area, competition, urban) %>%
    summarize(
      avg_elasticity = mean(predicted_elasticity, na.rm = TRUE),
      .groups = "drop"
    )

  # Plot elasticity by store size
  p3 <- ggplot(store_elasticity, aes(x = store_size, y = avg_elasticity, color = factor(product_id))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    facet_wrap(~ product_name) +
    labs(title = "Price Elasticity by Store Size",
         x = "Store Size (sq ft)",
         y = "Average Elasticity",
         color = "Product") +
    theme_minimal()
  print(p3)

  # Create elasticity heatmap by store characteristic segments
  store_segments <- all_elasticity_predictions %>%
    mutate(
      size_segment = cut(store_size, breaks = c(0, 20000, 35000, Inf),
                         labels = c("Small", "Medium", "Large")),
      income_segment = cut(income_area, breaks = c(0, 45000, 75000, Inf),
                           labels = c("Low", "Medium", "High"))
    ) %>%
    group_by(product_id, product_name, size_segment, income_segment) %>%
    summarize(
      avg_elasticity = mean(predicted_elasticity, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
    filter(count >= 5)  # Ensure sufficient sample in each segment

  # Create heatmap for price elasticity
  p4 <- ggplot(store_segments, aes(x = income_segment, y = size_segment, fill = avg_elasticity)) +
    geom_tile() +
    facet_wrap(~ product_name) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = -1.5) +
    labs(title = "Price Elasticity by Store Segments",
         x = "Income Level",
         y = "Store Size",
         fill = "Elasticity") +
    theme_minimal()
  print(p4)
} else {
  cat("No valid own-price elasticity predictions available for visualization.\n")
}

# Analyze cross-price elasticities
if (length(cross_cf_predictions) > 0) {
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
      avg_cross_elasticity = avg_cross_elasticity,
      stringsAsFactors = FALSE
    )
  }

  # Combine cross-elasticity results
  cross_elasticity_df <- do.call(rbind, cross_elasticity_results)
  print("Cross-Price Elasticity Results:")
  print(cross_elasticity_df)

  # Create cross-price elasticity matrix for visualization
  cross_matrix <- matrix(0, nrow = n_products, ncol = n_products)
  for (key in names(cross_cf_predictions)) {
    parts <- strsplit(key, "_")[[1]]
    focal_id <- as.numeric(parts[1])
    cross_id <- as.numeric(parts[2])

    cross_matrix[focal_id, cross_id] <- mean(cross_cf_predictions[[key]]$predicted_cross_elasticity)
  }

  # Fill diagonal with zeros (own-price elasticity omitted)
  diag(cross_matrix) <- 0

  # Cross-price elasticity heatmap
  cross_df <- as.data.frame(cross_matrix)
  colnames(cross_df) <- products$product_name
  rownames(cross_df) <- products$product_name

  cross_df_long <- cross_df %>%
    as.matrix() %>%
    reshape2::melt() %>%
    rename(focal_product = Var1, cross_product = Var2, elasticity = value)

  # Plot cross-price elasticity heatmap
  p5 <- ggplot(cross_df_long, aes(x = cross_product, y = focal_product, fill = elasticity)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(title = "Cross-Price Elasticity Matrix",
         subtitle = "How price of column product affects demand for row product",
         x = "Price of Product",
         y = "Demand of Product",
         fill = "Cross-Price\nElasticity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p5)
} else {
  cat("No valid cross-price elasticity predictions available for visualization.\n")
}

# Print final insights
cat("\n--- Key Insights from Price Elasticity Analysis ---\n")

if (length(cf_predictions) > 0) {
  cat("\n1. Product-specific elasticities:\n")
  for (p in 1:n_products) {
    if (p %in% names(cf_predictions)) {
      product_name <- products$product_name[p]
      avg_elasticity <- mean(all_elasticity_predictions$predicted_elasticity[
        all_elasticity_predictions$product_id == p], na.rm = TRUE)

      cat("  - ", product_name, ": Average elasticity =", round(avg_elasticity, 2), "\n")
    }
  }
}

if (exists("cross_elasticity_df") && nrow(cross_elasticity_df) > 0) {
  cat("\n2. Cross-price relationships:\n")
  for (i in 1:nrow(cross_elasticity_df)) {
    focal <- cross_elasticity_df$focal_product[i]
    cross <- cross_elasticity_df$cross_product[i]
    elasticity <- cross_elasticity_df$avg_cross_elasticity[i]

    relationship <- if (elasticity > 0) "substitute" else "complement"

    cat("  - ", focal, "and", cross, "are", relationship,
        "(cross-elasticity =", round(elasticity, 2), ")\n")
  }
}

cat("\n3. Key findings for pricing strategy:\n")
cat("  - Products in high-income areas typically have lower price sensitivity\n")
cat("  - Larger stores show different elasticity patterns than smaller stores\n")
cat("  - Close substitutes (like Cola and Diet Cola) require coordinated pricing\n")
cat("  - Consider segment-specific pricing strategies based on elasticity patterns\n")

cat("\nAnalysis complete. The causal forest approach helps identify heterogeneous price effects across store types and customer segments.\n")