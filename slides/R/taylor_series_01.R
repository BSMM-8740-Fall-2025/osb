

# Taylor Series Convergence Animation
# This animation shows how the Taylor series for sin(x) converges to the true function

library(tidyverse)
library(gganimate)
library(transformr)

# Define the domain
x <- seq(-2*pi, 2*pi, length.out = 1000)

# Function to calculate Taylor series for sin(x) around x = 0
taylor_sin <- function(x, n_terms) {
  result <- rep(0, length(x))

  for (n in 0:(n_terms-1)) {
    term <- ((-1)^n) * (x^(2*n + 1)) / factorial(2*n + 1)
    result <- result + term
  }

  return(result)
}

# Create data for animation
max_terms <- 10
animation_data <- tibble()

for (n in 1:max_terms) {
  current_data <- tibble(
    x = x,
    true_sin = sin(x),
    taylor_approx = taylor_sin(x, n),
    n_terms = n,
    term_label = paste("n =", n, "terms")
  )

  animation_data <- bind_rows(animation_data, current_data)
}

# Create the animated plot
p <- animation_data %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = true_sin, color = "True sin(x)"),
            size = 1.2, alpha = 0.8) +
  geom_line(aes(y = taylor_approx, color = "Taylor approximation"),
            size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("True sin(x)" = "#2E86AB",
                                "Taylor approximation" = "#F24236")) +
  labs(
    title = "Taylor Series Convergence for sin(x)",
    subtitle = "Terms: {closest_state}",
    x = "x",
    y = "y",
    color = "Function",
    caption = "Taylor series expansion around x = 0"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  coord_cartesian(ylim = c(-2, 2)) +
  transition_states(n_terms, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

# Create animation
anim <- animate(p,
                width = 800,
                height = 600,
                fps = 10,
                duration = 15,
                renderer = gifski_renderer("slides/images/taylor_series_convergence.gif"))

# Display the animation
anim

# Additional analysis: Calculate approximation error
error_data <- animation_data %>%
  mutate(
    absolute_error = abs(true_sin - taylor_approx),
    relative_error = ifelse(true_sin != 0, absolute_error / abs(true_sin), 0)
  ) %>%
  group_by(n_terms) %>%
  summarise(
    max_absolute_error = max(absolute_error),
    mean_absolute_error = mean(absolute_error),
    .groups = "drop"
  )

# Plot error convergence
error_plot <- error_data %>%
  ggplot(aes(x = n_terms)) +
  geom_line(aes(y = max_absolute_error, color = "Maximum Error"),
            size = 1.2) +
  geom_line(aes(y = mean_absolute_error, color = "Mean Error"),
            size = 1.2) +
  geom_point(aes(y = max_absolute_error, color = "Maximum Error"),
             size = 3) +
  geom_point(aes(y = mean_absolute_error, color = "Mean Error"),
             size = 3) +
  scale_color_manual(values = c("Maximum Error" = "#F24236",
                                "Mean Error" = "#2E86AB")) +
  scale_y_log10() +
  labs(
    title = "Taylor Series Approximation Error",
    subtitle = "Error decreases as more terms are added",
    x = "Number of Terms",
    y = "Absolute Error (log scale)",
    color = "Error Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom"
  )

print(error_plot)

# Display error table
cat("Taylor Series Approximation Error Summary:\n")
print(error_data)

# Show the first few Taylor series terms explicitly
cat("\nTaylor series for sin(x) around x = 0:\n")
cat("sin(x) = x - x³/3! + x⁵/5! - x⁷/7! + x⁹/9! - ...\n")
cat("sin(x) = x - x³/6 + x⁵/120 - x⁷/5040 + x⁹/362880 - ...\n")


# version 2 ----
# Taylor Series Convergence Animation for f(x) = 5x - 3exp(-4x)
# This animation shows how the Taylor series converges to the mixed linear-exponential function

library(tidyverse)
library(gganimate)
library(transformr)
library(viridis)
library(patchwork)

# Define the domain - focusing on region where convergence is most interesting
x <- seq(-1, 2, length.out = 1000)

# Original function: f(x) = 5x - 3exp(-4x)
f_original <- function(x) {
  5 * x - 3 * exp(-4 * x)
}

# Calculate derivatives for Taylor series
# f(x) = 5x - 3exp(-4x)
# f'(x) = 5 - 3*(-4)exp(-4x) = 5 + 12exp(-4x)
# f''(x) = 12*(-4)exp(-4x) = -48exp(-4x)
# f'''(x) = -48*(-4)exp(-4x) = 192exp(-4x)
# f^(n)(x) = 5*δ(n,1) + 3*(-4)^n*exp(-4x) for n ≥ 1
# where δ(n,1) is 1 if n=1, 0 otherwise

# Function to calculate nth derivative at a point
nth_derivative <- function(x, n) {
  if (n == 0) {
    return(5 * x - 3 * exp(-4 * x))
  } else if (n == 1) {
    return(5 + 12 * exp(-4 * x))
  } else {
    return(3 * ((-4)^n) * exp(-4 * x))
  }
}

# Taylor series for f(x) = 5x - 3exp(-4x) around any center
taylor_mixed <- function(x, n_terms, center = 0) {
  result <- rep(0, length(x))

  for (n in 0:(n_terms-1)) {
    # nth derivative at x = center
    derivative_at_center <- nth_derivative(center, n)

    # nth term of Taylor series
    term <- derivative_at_center * (x - center)^n / factorial(n)
    result <- result + term
  }

  return(result)
}

# Create data for animation with multiple expansion points
max_terms <- 15
centers <- c(0, 0.5, 1.0)
center_names <- c("x = 0", "x = 0.5", "x = 1.0")

animation_data <- tibble()

for (i in 1:length(centers)) {
  center <- centers[i]
  center_name <- center_names[i]

  for (n in 1:max_terms) {
    current_data <- tibble(
      x = x,
      true_function = f_original(x),
      taylor_approx = taylor_mixed(x, n, center),
      n_terms = n,
      center = center,
      center_name = center_name,
      term_label = paste("n =", n, "terms"),
      expansion_point = paste("Expansion around", center_name)
    )

    animation_data <- bind_rows(animation_data, current_data)
  }
}

# Create the main animated plot
p1 <- animation_data %>%
  filter(center_name == "x = 0") %>%  # Start with expansion around x = 0
  ggplot(aes(x = x)) +
  geom_line(aes(y = true_function, color = "True f(x) = 5x - 3exp(-4x)"),
            size = 1.5, alpha = 0.9) +
  geom_line(aes(y = taylor_approx, color = "Taylor approximation"),
            size = 1.5, linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "gray50", linetype = "dotted", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "gray80", linetype = "solid", alpha = 0.5) +
  scale_color_manual(values = c("True f(x) = 5x - 3exp(-4x)" = "#2E86AB",
                                "Taylor approximation" = "#F24236")) +
  labs(
    title = "Taylor Series Convergence for f(x) = 5x - 3exp(-4x)",
    subtitle = "Terms: {closest_state} | Expansion around x = 0",
    x = "x",
    y = "f(x)",
    color = "Function",
    caption = "Dotted line shows expansion point | Function combines linear and exponential terms"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(-15, 15)) +
  transition_states(n_terms, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

# Create animation
anim1 <- animate(p1,
                 width = 900,
                 height = 600,
                 fps = 8,
                 duration = 20,
                 renderer = gifski_renderer("slides/images/taylor_5x_3exp_convergence.gif"))

# Multi-center comparison plot
p2 <- animation_data %>%
  filter(n_terms %in% c(3, 6, 10, 15)) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = true_function),
            color = "black", size = 1.5, alpha = 0.8) +
  geom_line(aes(y = taylor_approx, color = center_name),
            size = 1.2, linetype = "dashed") +
  geom_vline(data = tibble(center = centers, center_name = center_names),
             aes(xintercept = center, color = center_name),
             linetype = "dotted", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "gray80", linetype = "solid", alpha = 0.3) +
  scale_color_viridis_d(name = "Expansion Point") +
  facet_wrap(~paste("n =", n_terms, "terms"), ncol = 2) +
  labs(
    title = "Taylor Series Convergence: Different Expansion Points",
    subtitle = "f(x) = 5x - 3exp(-4x) with various centers of expansion",
    x = "x",
    y = "f(x)",
    caption = "Black line: true function | Dashed lines: Taylor approximations | Dotted lines: expansion points"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(-10, 15))

# Error analysis
error_data <- animation_data %>%
  mutate(
    absolute_error = abs(true_function - taylor_approx),
    relative_error = ifelse(abs(true_function) > 1e-10, absolute_error / abs(true_function), 0)
  ) %>%
  group_by(n_terms, center_name) %>%
  summarise(
    max_absolute_error = max(absolute_error),
    mean_absolute_error = mean(absolute_error),
    max_relative_error = max(relative_error[is.finite(relative_error)]),
    mean_relative_error = mean(relative_error[is.finite(relative_error)]),
    .groups = "drop"
  )

# Error convergence plot
p3 <- error_data %>%
  ggplot(aes(x = n_terms, color = center_name)) +
  geom_line(aes(y = max_absolute_error), size = 1.2) +
  geom_point(aes(y = max_absolute_error), size = 3) +
  scale_color_viridis_d(name = "Expansion Point") +
  scale_y_log10() +
  labs(
    title = "Taylor Series Approximation Error Convergence",
    subtitle = "Maximum absolute error vs. number of terms",
    x = "Number of Terms",
    y = "Maximum Absolute Error (log scale)",
    caption = "Lower is better - shows how error decreases with more terms"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Component analysis - separate linear and exponential parts
component_data <- tibble(
  x = x,
  linear_part = 5 * x,
  exponential_part = -3 * exp(-4 * x),
  combined = f_original(x)
)

p4 <- component_data %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = linear_part, color = "Linear: 5x"), size = 1.5) +
  geom_line(aes(y = exponential_part, color = "Exponential: -3exp(-4x)"), size = 1.5) +
  geom_line(aes(y = combined, color = "Combined: f(x)"), size = 1.5, linetype = "dashed") +
  scale_color_manual(values = c("Linear: 5x" = "#2E86AB",
                                "Exponential: -3exp(-4x)" = "#F24236",
                                "Combined: f(x)" = "#A23B72")) +
  labs(
    title = "Function Components Analysis",
    subtitle = "f(x) = 5x - 3exp(-4x) = Linear + Exponential",
    x = "x",
    y = "f(x)",
    color = "Component"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0, color = "gray80", linetype = "solid", alpha = 0.3)

# Display all plots
print("Creating main animation...")
anim1

print("Multi-center comparison:")
print(p2)

print("Error analysis:")
print(p3)

print("Component analysis:")
print(p4)

# Mathematical analysis
cat("\n" %+% "="*70 %+% "\n")
cat("TAYLOR SERIES ANALYSIS FOR f(x) = 5x - 3exp(-4x)\n")
cat("="*70 %+% "\n")

cat("\nFunction components:\n")
cat("f(x) = 5x - 3exp(-4x)\n")
cat("Linear part: 5x\n")
cat("Exponential part: -3exp(-4x)\n\n")

cat("Derivatives:\n")
cat("f(x) = 5x - 3exp(-4x)\n")
cat("f'(x) = 5 + 12exp(-4x)\n")
cat("f''(x) = -48exp(-4x)\n")
cat("f'''(x) = 192exp(-4x)\n")
cat("f^(n)(x) = 5*δ(n,1) + 3*(-4)^n*exp(-4x) for n ≥ 1\n\n")

cat("Taylor series coefficients at x = 0:\n")
for (n in 0:8) {
  if (n == 0) {
    coeff_val <- 5 * 0 - 3 * exp(0)
    exact_coeff <- coeff_val / factorial(n)
    cat(sprintf("f^(%d)(0) = %.6f, coefficient = %.6f\n", n, coeff_val, exact_coeff))
  } else if (n == 1) {
    coeff_val <- 5 + 12 * exp(0)
    exact_coeff <- coeff_val / factorial(n)
    cat(sprintf("f^(%d)(0) = %.6f, coefficient = %.6f\n", n, coeff_val, exact_coeff))
  } else {
    coeff_val <- 3 * ((-4)^n) * exp(0)
    exact_coeff <- coeff_val / factorial(n)
    cat(sprintf("f^(%d)(0) = %.6f, coefficient = %.6f\n", n, coeff_val, exact_coeff))
  }
}

cat("\nTaylor series expansion around x = 0:\n")
cat("f(x) = -3 + 17x - 24x² + 32x³ - 32x⁴ + 128x⁵/5 - 64x⁶/15 + ...\n\n")

cat("Special properties:\n")
cat("- f(0) = 5(0) - 3exp(0) = -3\n")
cat("- f'(0) = 5 + 12exp(0) = 17\n")
cat("- As x → ∞: f(x) → 5x (linear dominance)\n")
cat("- As x → -∞: f(x) → -∞ (exponential dominance)\n")
cat("- Zero crossing: f(x) = 0 when 5x = 3exp(-4x)\n\n")

# Find approximate zero
zero_approx <- uniroot(f_original, c(0, 1))$root
cat(sprintf("Approximate zero: x ≈ %.6f\n", zero_approx))
cat(sprintf("Verification: f(%.6f) ≈ %.10f\n", zero_approx, f_original(zero_approx)))

# Display error summary
cat("\nError Summary (expansion around x = 0):\n")
error_summary <- error_data %>%
  filter(center_name == "x = 0", n_terms <= 10) %>%
  select(n_terms, max_absolute_error, mean_absolute_error)

print(error_summary)

cat("\nConvergence characteristics:\n")
cat("- Linear term (5x): Exact after 2 terms\n")
cat("- Exponential term (-3exp(-4x)): Converges exponentially fast\n")
cat("- Combined function: Inherits rapid convergence from exponential component\n")
cat("- Best approximation near expansion point\n")
cat("- For large |x|: more terms needed due to exponential growth/decay\n")