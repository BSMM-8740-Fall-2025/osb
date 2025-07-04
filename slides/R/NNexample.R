# 3-Layer Neural Network Regression Animation
# Shows construction, training, and learning process

library(tidyverse)
library(gganimate)
library(transformr)
library(viridis)
library(patchwork)

# Set seed for reproducibility
set.seed(42)

# Generate synthetic regression data
n_points <- 100
x_data <- seq(-2, 2, length.out = n_points)
true_function <- function(x) 0.5 * x^3 - 2 * x^2 + x + 1
y_data <- true_function(x_data) + rnorm(n_points, 0, 0.3)

# Neural network architecture
input_size <- 1
hidden_size <- 8
output_size <- 1

# Initialize weights and biases
set.seed(123)
W1 <- matrix(rnorm(input_size * hidden_size, 0, 0.5), nrow = input_size, ncol = hidden_size)
b1 <- rnorm(hidden_size, 0, 0.1)
W2 <- matrix(rnorm(hidden_size * output_size, 0, 0.5), nrow = hidden_size, ncol = output_size)
b2 <- rnorm(output_size, 0, 0.1)

# Activation functions
sigmoid <- function(x) 1 / (1 + exp(-pmax(-500, pmin(500, x))))
sigmoid_derivative <- function(x) sigmoid(x) * (1 - sigmoid(x))

# Forward pass function
forward_pass <- function(x, W1, b1, W2, b2) {
  z1 <- as.vector(x %*% W1) + b1
  a1 <- sigmoid(z1)
  z2 <- as.vector(a1 %*% W2) + b2

  return(list(
    z1 = z1,
    a1 = a1,
    z2 = z2,
    output = z2
  ))
}

# Training parameters
learning_rate <- 0.1
epochs <- 150
animation_epochs <- seq(1, epochs, by = 5)  # Sample epochs for animation

# Store training history
training_history <- list()
weight_history <- list()

# Current weights for training
W1_current <- W1
b1_current <- b1
W2_current <- W2
b2_current <- b2

# Training loop
for (epoch in 1:epochs) {
  # Forward pass
  predictions <- rep(0, length(x_data))
  total_loss <- 0

  # Store activations for visualization
  all_z1 <- matrix(0, nrow = length(x_data), ncol = hidden_size)
  all_a1 <- matrix(0, nrow = length(x_data), ncol = hidden_size)

  for (i in 1:length(x_data)) {
    x_input <- matrix(x_data[i], nrow = 1)
    forward_result <- forward_pass(x_input, W1_current, b1_current, W2_current, b2_current)
    predictions[i] <- forward_result$output
    all_z1[i, ] <- forward_result$z1
    all_a1[i, ] <- forward_result$a1

    # Calculate loss (MSE)
    loss <- 0.5 * (y_data[i] - predictions[i])^2
    total_loss <- total_loss + loss

    # Backpropagation
    # Output layer gradients
    dL_dz2 <- predictions[i] - y_data[i]
    dL_dW2 <- as.matrix(forward_result$a1) %*% matrix(dL_dz2, nrow = 1)
    dL_db2 <- dL_dz2

    # Hidden layer gradients
    dL_da1 <- as.vector(W2_current %*% matrix(dL_dz2, ncol = 1))
    dL_dz1 <- dL_da1 * sigmoid_derivative(forward_result$z1)
    dL_dW1 <- matrix(x_data[i], ncol = 1) %*% matrix(dL_dz1, nrow = 1)
    dL_db1 <- dL_dz1

    # Update weights
    W2_current <- W2_current - learning_rate * dL_dW2
    b2_current <- b2_current - learning_rate * dL_db2
    W1_current <- W1_current - learning_rate * dL_dW1
    b1_current <- b1_current - learning_rate * dL_db1
  }

  # Store history for animation
  if (epoch %in% animation_epochs) {
    training_history[[length(training_history) + 1]] <- list(
      epoch = epoch,
      x = x_data,
      y_true = y_data,
      y_pred = predictions,
      loss = total_loss / length(x_data),
      hidden_activations = all_a1
    )

    weight_history[[length(weight_history) + 1]] <- list(
      epoch = epoch,
      W1 = W1_current,
      b1 = b1_current,
      W2 = W2_current,
      b2 = b2_current
    )
  }
}

# Create network architecture visualization data
create_network_viz <- function(epoch_idx) {
  # Node positions
  input_nodes <- tibble(
    x = 1,
    y = 3,
    layer = "Input",
    node_id = "x",
    value = 0
  )

  hidden_nodes <- tibble(
    x = 2,
    y = seq(1, 5, length.out = hidden_size),
    layer = "Hidden",
    node_id = paste0("h", 1:hidden_size),
    value = if(epoch_idx <= length(training_history)) {
      colMeans(training_history[[epoch_idx]]$hidden_activations)
    } else {
      rep(0.5, hidden_size)
    }
  )

  output_nodes <- tibble(
    x = 3,
    y = 3,
    layer = "Output",
    node_id = "y",
    value = 0
  )

  nodes <- bind_rows(input_nodes, hidden_nodes, output_nodes)

  # Create connections
  connections <- tibble()

  # Input to hidden connections
  for (h in 1:hidden_size) {
    weight_val <- if(epoch_idx <= length(weight_history)) {
      weight_history[[epoch_idx]]$W1[1, h]
    } else {
      W1[1, h]
    }

    connections <- bind_rows(connections, tibble(
      x1 = 1, y1 = 3,
      x2 = 2, y2 = seq(1, 5, length.out = hidden_size)[h],
      weight = weight_val,
      connection_type = "input_hidden"
    ))
  }

  # Hidden to output connections
  for (h in 1:hidden_size) {
    weight_val <- if(epoch_idx <= length(weight_history)) {
      weight_history[[epoch_idx]]$W2[h, 1]
    } else {
      W2[h, 1]
    }

    connections <- bind_rows(connections, tibble(
      x1 = 2, y1 = seq(1, 5, length.out = hidden_size)[h],
      x2 = 3, y2 = 3,
      weight = weight_val,
      connection_type = "hidden_output"
    ))
  }

  return(list(nodes = nodes, connections = connections))
}

# Create animation data for predictions
animation_data <- tibble()
for (i in 1:length(training_history)) {
  current_data <- tibble(
    epoch = training_history[[i]]$epoch,
    x = training_history[[i]]$x,
    y_true = training_history[[i]]$y_true,
    y_pred = training_history[[i]]$y_pred,
    loss = training_history[[i]]$loss
  )
  animation_data <- bind_rows(animation_data, current_data)
}

# Animation 1: Learning Progress
p1 <- animation_data %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y_true, color = "True Function"), size = 1.5, alpha = 0.8) +
  geom_point(aes(y = y_true), alpha = 0.3, size = 1) +
  geom_line(aes(y = y_pred, color = "Neural Network"), size = 1.5, linetype = "dashed") +
  scale_color_manual(values = c("True Function" = "#2E86AB", "Neural Network" = "#F24236")) +
  labs(
    title = "Neural Network Learning Progress",
    subtitle = "Epoch: {closest_state} | Loss: {round(animation_data$loss[animation_data$epoch == closest_state][1], 4)}",
    x = "Input (x)",
    y = "Output (y)",
    color = "Function",
    caption = "3-layer neural network learning to approximate the true function"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    legend.position = "bottom"
  ) +
  transition_states(epoch, transition_length = 1, state_length = 1) +
  ease_aes("cubic-in-out")

# Separate nodes and connections for animation
nodes_anim <- map_dfr(1:length(training_history), ~{
  epoch_data <- create_network_viz(.x)
  epoch_data$nodes %>% mutate(epoch = training_history[[.x]]$epoch)
})

connections_anim <- map_dfr(1:length(training_history), ~{
  epoch_data <- create_network_viz(.x)
  epoch_data$connections %>% mutate(epoch = training_history[[.x]]$epoch)
})

# Animation 2: Network Architecture with changing weights
p2 <- ggplot() +
  # Draw connections first (behind nodes)
  geom_segment(data = connections_anim,
               aes(x = x1, y = y1, xend = x2, yend = y2,
                   color = weight, size = abs(weight)),
               alpha = 0.7) +

  # Draw nodes
  geom_point(data = nodes_anim,
             aes(x = x, y = y, fill = value, size = 4),
             shape = 21, stroke = 2, color = "black") +

  # Add node labels
  geom_text(data = nodes_anim,
            aes(x = x, y = y, label = node_id),
            color = "white", fontface = "bold", size = 3) +

  scale_color_gradient2(low = "red", mid = "gray", high = "blue",
                        midpoint = 0, name = "Weight") +
  scale_fill_viridis_c(name = "Activation", option = "plasma") +
  scale_size_continuous(range = c(0.5, 2), guide = "none") +

  labs(
    title = "Neural Network Architecture Evolution",
    subtitle = "Epoch: {closest_state} | Weights and activations changing during training",
    x = "",
    y = "",
    caption = "Node size ∝ activation level | Edge color/thickness ∝ weight magnitude"
  ) +

  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  ) +

  coord_fixed() +
  xlim(0.5, 3.5) + ylim(0.5, 5.5) +

  transition_states(epoch, transition_length = 1, state_length = 1) +
  ease_aes("cubic-in-out")

# Loss curve animation
loss_data <- map_dfr(1:length(training_history), ~{
  tibble(
    epoch = training_history[[.x]]$epoch,
    loss = training_history[[.x]]$loss,
    epoch_index = .x
  )
})

# Create cumulative loss data for animation
loss_cumulative <- map_dfr(1:nrow(loss_data), ~{
  loss_data[1:.x, ] %>% mutate(current_epoch = loss_data$epoch[.x])
})

p3 <- loss_cumulative %>%
  ggplot(aes(x = epoch, y = loss)) +
  geom_line(color = "#F24236", size = 1.5) +
  geom_point(data = . %>% filter(epoch == current_epoch),
             color = "#F24236", size = 4) +
  scale_y_log10() +
  labs(
    title = "Training Loss Over Time",
    subtitle = "Current Epoch: {closest_state}",
    x = "Epoch",
    y = "Loss (log scale)",
    caption = "Mean Squared Error decreasing as network learns"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14)
  ) +
  transition_states(current_epoch, transition_length = 1, state_length = 1) +
  ease_aes("cubic-in-out")

# Create the animations
anim1 <- animate(p1, width = 800, height = 600, fps = 8, duration = 15,
                 renderer = gifski_renderer("slides/images/neural_net_learning.gif"))

anim2 <- animate(p2, width = 800, height = 600, fps = 8, duration = 15,
                 renderer = gifski_renderer("slides/images/neural_net_architecture.gif"))

anim3 <- animate(p3, width = 800, height = 600, fps = 8, duration = 15,
                 renderer = gifski_renderer("slides/images/neural_net_loss.gif"))

# Static summary plots
# Final network architecture
final_viz <- create_network_viz(length(training_history))

p4 <- ggplot() +
  geom_segment(data = final_viz$connections,
               aes(x = x1, y = y1, xend = x2, yend = y2,
                   color = weight, size = abs(weight)),
               alpha = 0.8) +
  geom_point(data = final_viz$nodes,
             aes(x = x, y = y, fill = value),
             size = 8, shape = 21, stroke = 2, color = "black") +
  geom_text(data = final_viz$nodes,
            aes(x = x, y = y, label = node_id),
            color = "white", fontface = "bold", size = 4) +

  # Add layer labels
  annotate("text", x = 1, y = 0.3, label = "Input Layer", size = 5, fontface = "bold") +
  annotate("text", x = 2, y = 0.3, label = "Hidden Layer\n(8 neurons)", size = 5, fontface = "bold") +
  annotate("text", x = 3, y = 0.3, label = "Output Layer", size = 5, fontface = "bold") +

  scale_color_gradient2(low = "red", mid = "gray", high = "blue",
                        midpoint = 0, name = "Weight") +
  scale_fill_viridis_c(name = "Activation", option = "plasma") +
  scale_size_continuous(range = c(0.5, 3), guide = "none") +

  labs(
    title = "Final Neural Network Architecture",
    subtitle = "3-layer network after training completion",
    caption = "Trained to approximate f(x) = 0.5x³ - 2x² + x + 1"
  ) +

  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom"
  ) +

  coord_fixed() +
  xlim(0.5, 3.5) + ylim(0, 5.5)

# Display animations and final plot
print("Animation 1: Learning Progress")
anim1

print("Animation 2: Network Architecture Evolution")
anim2

print("Animation 3: Loss Convergence")
anim3

print("Final Network Architecture:")
print(p4)

# Print network summary
cat("\n" %+% "="*60 %+% "\n")
cat("NEURAL NETWORK REGRESSION SUMMARY\n")
cat("="*60 %+% "\n")

cat(sprintf("Architecture: %d → %d → %d\n", input_size, hidden_size, output_size))
cat(sprintf("Total Parameters: %d\n",
            input_size * hidden_size + hidden_size + hidden_size * output_size + output_size))
cat(sprintf("Training Epochs: %d\n", epochs))
cat(sprintf("Learning Rate: %.2f\n", learning_rate))
cat(sprintf("Final Loss: %.6f\n", training_history[[length(training_history)]]$loss))

cat("\nNetwork Components:\n")
cat("• Input Layer: 1 neuron (receives x)\n")
cat("• Hidden Layer: 8 neurons with sigmoid activation\n")
cat("• Output Layer: 1 neuron (produces ŷ)\n")
cat("• Connections: Fully connected between layers\n")

cat("\nTraining Process:\n")
cat("• Forward Pass: x → hidden → output\n")
cat("• Loss Function: Mean Squared Error\n")
cat("• Backpropagation: Gradient descent weight updates\n")
cat("• Activation: Sigmoid function in hidden layer\n")

cat("\nWhat the animations show:\n")
cat("1. Learning Progress: How predictions improve over time\n")
cat("2. Architecture Evolution: Weight changes during training\n")
cat("3. Loss Convergence: Error reduction through epochs\n")