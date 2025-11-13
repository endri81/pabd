################################################################################
# Classwork 1: Neural Network Fundamentals and CNNs
# Deep Learning Introduction - Lecture 4 (First Half)
# Duration: 30 minutes
# Prof. Asc. Endri Raco, Ph.D.
# November 2025
################################################################################

# INSTRUCTIONS:
# - Complete all exercises marked with TODO
# - Each exercise has allocated time (aim to stay within limits)
# - Run code chunks sequentially
# - Submit completed R script with outputs

# Load required libraries
if (!require("keras")) install.packages("keras")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(keras)
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(2025)

################################################################################
# PART 1: PERCEPTRON AND ACTIVATION FUNCTIONS (8 minutes)
################################################################################

cat("=" %.% rep(50), "\n")
cat("PART 1: PERCEPTRON AND ACTIVATION FUNCTIONS\n")
cat("=" %.% rep(50), "\n\n")

# Exercise 1.1: Implement activation functions (3 minutes)
# TODO: Complete the implementation of these activation functions

sigmoid <- function(z) {
  # TODO: Implement sigmoid activation
  # Hint: 1 / (1 + exp(-z))
  
}

tanh_activation <- function(z) {
  # TODO: Implement hyperbolic tangent
  # Hint: Use built-in tanh() or compute (exp(z) - exp(-z)) / (exp(z) + exp(-z))
  
}

relu <- function(z) {
  # TODO: Implement ReLU
  # Hint: max(0, z) for each element
  
}

leaky_relu <- function(z, alpha = 0.01) {
  # TODO: Implement Leaky ReLU
  # Hint: if z > 0 then z, else alpha * z
  
}

# Test your implementations
test_values <- seq(-3, 3, 0.5)
cat("Testing activation functions on:", test_values, "\n\n")

# TODO: Uncomment after implementing functions
# cat("Sigmoid:", round(sigmoid(test_values), 3), "\n")
# cat("Tanh:", round(tanh_activation(test_values), 3), "\n")
# cat("ReLU:", round(relu(test_values), 3), "\n")
# cat("Leaky ReLU:", round(leaky_relu(test_values), 3), "\n\n")

# Exercise 1.2: Implement gradient functions (3 minutes)
# TODO: Implement derivatives for backpropagation

sigmoid_derivative <- function(z) {
  # TODO: Implement sigmoid derivative
  # Hint: sig(z) * (1 - sig(z))
  
}

relu_derivative <- function(z) {
  # TODO: Implement ReLU derivative
  # Hint: 1 if z > 0, else 0
  
}

# Exercise 1.3: Visualize activations and their derivatives (2 minutes)
# TODO: Create visualization comparing activation functions

plot_activations <- function() {
  z_range <- seq(-5, 5, 0.1)
  
  # TODO: Create dataframe with all activation values
  activation_df <- data.frame(
    z = rep(z_range, 4),
    value = c(
      # TODO: Add activation function values here
    ),
    type = rep(c("Sigmoid", "Tanh", "ReLU", "Leaky_ReLU"), 
               each = length(z_range))
  )
  
  # TODO: Create ggplot visualization
  # p <- ggplot(activation_df, aes(x = z, y = value, color = type)) +
  #   geom_line(size = 1.2) +
  #   labs(title = "Comparison of Activation Functions",
  #        x = "Input (z)", y = "Output") +
  #   theme_minimal() +
  #   theme(legend.position = "bottom")
  # 
  # print(p)
}

# TODO: Uncomment to test visualization
# plot_activations()

################################################################################
# PART 2: SIMPLE NEURAL NETWORK FROM SCRATCH (10 minutes)
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("PART 2: BUILDING A NEURAL NETWORK FROM SCRATCH\n")
cat("=" %.% rep(50), "\n\n")

# Exercise 2.1: Implement forward pass for 2-layer network (5 minutes)
forward_pass <- function(X, W1, b1, W2, b2, activation = "relu") {
  # TODO: Implement forward propagation
  # Network: Input -> Hidden Layer (4 units) -> Output
  
  # Step 1: Linear transformation for hidden layer
  # z1 <- X %*% W1 + matrix(rep(b1, nrow(X)), nrow = nrow(X), byrow = TRUE)
  
  # Step 2: Apply activation function
  # if (activation == "relu") {
  #   a1 <- relu(z1)
  # } else if (activation == "sigmoid") {
  #   a1 <- sigmoid(z1)
  # }
  
  # Step 3: Linear transformation for output layer
  # z2 <- a1 %*% W2 + matrix(rep(b2, nrow(a1)), nrow = nrow(a1), byrow = TRUE)
  
  # Step 4: Apply sigmoid for binary classification
  # a2 <- sigmoid(z2)
  
  # TODO: Return list with all intermediate values (needed for backprop)
  # return(list(z1 = z1, a1 = a1, z2 = z2, a2 = a2))
}

# Exercise 2.2: Implement backward pass (5 minutes)
backward_pass <- function(X, y, cache, W1, W2, activation = "relu") {
  # TODO: Implement backpropagation
  m <- nrow(X)
  
  # Extract cached values from forward pass
  # z1 <- cache$z1
  # a1 <- cache$a1
  # z2 <- cache$z2
  # a2 <- cache$a2
  
  # TODO: Compute gradients
  # Output layer gradients
  # dz2 <- a2 - y  # For binary cross-entropy with sigmoid
  # dW2 <- (1/m) * t(a1) %*% dz2
  # db2 <- (1/m) * colSums(dz2)
  
  # Hidden layer gradients
  # da1 <- dz2 %*% t(W2)
  # if (activation == "relu") {
  #   dz1 <- da1 * relu_derivative(z1)
  # } else if (activation == "sigmoid") {
  #   dz1 <- da1 * sigmoid_derivative(z1)
  # }
  # dW1 <- (1/m) * t(X) %*% dz1
  # db1 <- (1/m) * colSums(dz1)
  
  # TODO: Return gradients
  # return(list(dW1 = dW1, db1 = db1, dW2 = dW2, db2 = db2))
}

# Test on XOR problem
X_xor <- matrix(c(0,0, 0,1, 1,0, 1,1), ncol = 2, byrow = TRUE)
y_xor <- matrix(c(0, 1, 1, 0), ncol = 1)

cat("XOR Problem:\n")
cat("Inputs:\n")
print(X_xor)
cat("\nTargets:\n")
print(y_xor)

# Initialize small network
n_input <- 2
n_hidden <- 4
n_output <- 1

# TODO: Initialize weights and biases with small random values
# W1 <- matrix(rnorm(n_input * n_hidden, 0, 0.5), n_input, n_hidden)
# b1 <- rep(0, n_hidden)
# W2 <- matrix(rnorm(n_hidden * n_output, 0, 0.5), n_hidden, n_output)
# b2 <- rep(0, n_output)

# TODO: Perform one forward and backward pass
# cache <- forward_pass(X_xor, W1, b1, W2, b2)
# grads <- backward_pass(X_xor, y_xor, cache, W1, W2)

################################################################################
# PART 3: CONVOLUTIONAL NEURAL NETWORKS (12 minutes)
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("PART 3: CONVOLUTIONAL NEURAL NETWORKS\n")
cat("=" %.% rep(50), "\n\n")

# Exercise 3.1: Implement 2D convolution (4 minutes)
conv2d <- function(image, kernel) {
  # TODO: Implement basic 2D convolution operation
  
  img_height <- nrow(image)
  img_width <- ncol(image)
  ker_height <- nrow(kernel)
  ker_width <- ncol(kernel)
  
  # Calculate output dimensions (valid convolution, no padding)
  out_height <- img_height - ker_height + 1
  out_width <- img_width - ker_width + 1
  
  # Initialize output matrix
  output <- matrix(0, out_height, out_width)
  
  # TODO: Implement convolution loops
  # for (i in 1:out_height) {
  #   for (j in 1:out_width) {
  #     # Extract image patch
  #     patch <- image[i:(i + ker_height - 1), j:(j + ker_width - 1)]
  #     
  #     # Compute element-wise multiplication and sum
  #     output[i, j] <- sum(patch * kernel)
  #   }
  # }
  
  return(output)
}

# Test convolution with edge detection
# Create simple test image with vertical edge
test_image <- matrix(0, 10, 10)
test_image[, 6:10] <- 1

# Sobel vertical edge detector
sobel_vertical <- matrix(c(-1, 0, 1,
                           -2, 0, 2,
                           -1, 0, 1), nrow = 3, byrow = TRUE)

# TODO: Apply convolution and visualize
# edge_detected <- conv2d(test_image, sobel_vertical)
# cat("Original image shape:", dim(test_image), "\n")
# cat("Output shape after convolution:", dim(edge_detected), "\n")

# Exercise 3.2: Implement max pooling (3 minutes)
max_pool2d <- function(image, pool_size = 2, stride = 2) {
  # TODO: Implement max pooling operation
  
  img_height <- nrow(image)
  img_width <- ncol(image)
  
  # Calculate output dimensions
  out_height <- floor((img_height - pool_size) / stride) + 1
  out_width <- floor((img_width - pool_size) / stride) + 1
  
  # Initialize output
  output <- matrix(0, out_height, out_width)
  
  # TODO: Implement pooling loops
  # for (i in 1:out_height) {
  #   for (j in 1:out_width) {
  #     # Calculate starting position
  #     row_start <- (i - 1) * stride + 1
  #     col_start <- (j - 1) * stride + 1
  #     
  #     # Extract patch and find maximum
  #     patch <- image[row_start:(row_start + pool_size - 1),
  #                    col_start:(col_start + pool_size - 1)]
  #     output[i, j] <- max(patch)
  #   }
  # }
  
  return(output)
}

# Test pooling
test_feature_map <- matrix(rnorm(8*8), 8, 8)
# TODO: Apply max pooling
# pooled <- max_pool2d(test_feature_map)
# cat("Feature map shape before pooling:", dim(test_feature_map), "\n")
# cat("Feature map shape after pooling:", dim(pooled), "\n")

# Exercise 3.3: Build and train CNN with Keras (5 minutes)
build_cnn_mnist <- function() {
  # TODO: Build CNN architecture for MNIST
  
  model <- keras_model_sequential() %>%
    # First convolutional block
    # TODO: Add Conv2D layer with 32 filters, 3x3 kernel, relu activation
    # layer_conv_2d(filters = 32, kernel_size = c(3, 3), 
    #               activation = "relu", input_shape = c(28, 28, 1)) %>%
    
    # TODO: Add max pooling layer 2x2
    # layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    
    # Second convolutional block
    # TODO: Add Conv2D layer with 64 filters
    # layer_conv_2d(filters = 64, kernel_size = c(3, 3), 
    #               activation = "relu") %>%
    
    # TODO: Add max pooling
    # layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    
    # Flatten and dense layers
    # TODO: Add flatten layer
    # layer_flatten() %>%
    
    # TODO: Add dense layer with 128 units
    # layer_dense(units = 128, activation = "relu") %>%
    
    # TODO: Add dropout for regularization
    # layer_dropout(rate = 0.5) %>%
    
    # TODO: Add output layer with 10 units (for 10 digits)
    # layer_dense(units = 10, activation = "softmax")
    
    return(model)
}

# Load MNIST subset for quick training
mnist <- dataset_mnist()
x_train <- mnist$train$x[1:1000,,] / 255  # Normalize and use subset
y_train <- to_categorical(mnist$train$y[1:1000], 10)
x_test <- mnist$test$x[1:200,,] / 255
y_test <- to_categorical(mnist$test$y[1:200], 10)

# Reshape for CNN (add channel dimension)
x_train <- array_reshape(x_train, c(1000, 28, 28, 1))
x_test <- array_reshape(x_test, c(200, 28, 28, 1))

# TODO: Build and compile model
# cnn_model <- build_cnn_mnist()
# cnn_model %>% compile(
#   optimizer = "adam",
#   loss = "categorical_crossentropy",
#   metrics = c("accuracy")
# )

# TODO: Display model architecture
# summary(cnn_model)

# TODO: Train for 2 epochs (quick training for classwork)
# history <- cnn_model %>% fit(
#   x_train, y_train,
#   epochs = 2,
#   batch_size = 32,
#   validation_split = 0.2,
#   verbose = 1
# )

################################################################################
# BONUS CHALLENGE (if time permits)
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("BONUS CHALLENGE: GRADIENT DESCENT VISUALIZATION\n")
cat("=" %.% rep(50), "\n\n")

# Implement simple gradient descent on quadratic function
gradient_descent_demo <- function(learning_rates = c(0.01, 0.1, 0.5), 
                                  n_iterations = 50) {
  # TODO: Implement gradient descent for f(w) = (w - 3)^2
  
  # Define function and its derivative
  f <- function(w) (w - 3)^2
  df_dw <- function(w) 2 * (w - 3)
  
  # TODO: Track optimization paths for different learning rates
  results <- list()
  
  # for (lr in learning_rates) {
  #   w <- -2  # Starting point
  #   path <- numeric(n_iterations)
  #   
  #   for (i in 1:n_iterations) {
  #     path[i] <- w
  #     # TODO: Update w using gradient descent
  #     # w <- w - lr * df_dw(w)
  #   }
  #   
  #   results[[as.character(lr)]] <- path
  # }
  
  # TODO: Visualize convergence paths
  # Create plot showing how different learning rates converge
  
  return(results)
}

# TODO: Run gradient descent demo
# gd_results <- gradient_descent_demo()

################################################################################
# SUBMISSION CHECKLIST
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("CLASSWORK 1 COMPLETION CHECKLIST\n")
cat("=" %.% rep(50), "\n\n")

cat("Before submitting, ensure you have:\n")
cat("[ ] Implemented all activation functions\n")
cat("[ ] Completed forward and backward pass\n")
cat("[ ] Implemented 2D convolution\n")
cat("[ ] Implemented max pooling\n")
cat("[ ] Built CNN architecture with Keras\n")
cat("[ ] All code runs without errors\n")
cat("[ ] Added comments explaining your implementation\n")

cat("\n", "=" %.% rep(50), "\n")
cat("END OF CLASSWORK 1\n")
cat("Estimated completion time: 30 minutes\n")
cat("=" %.% rep(50), "\n")

# Save your workspace
save.image("classwork1_neural_networks.RData")
cat("\nWorkspace saved as 'classwork1_neural_networks.RData'\n")