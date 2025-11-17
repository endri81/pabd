###############################################################################
# PREDICTIVE MODELING COURSEWORK
# MSc Data Science - November 2025
# Duration: 1 Week Assignment
# Lectures: 4 (Neural Networks & XAI) and 5 (Time Series & Unsupervised)
###############################################################################

# INSTRUCTIONS:
# - Complete all sections with your code and analysis
# - Use provided datasets (built-in R datasets for efficiency)
# - Focus on understanding concepts rather than complex implementations
# - Each section should take approximately 1-2 hours
# - Total estimated time: 8-10 hours over one week

# Required libraries (all free and lightweight)
required_packages <- c("nnet", "MASS", "cluster", "forecast", 
                      "tseries", "arules", "e1071")

# Install packages if not present
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load all required packages
invisible(lapply(required_packages, install_if_missing))

###############################################################################
# PART 1: NEURAL NETWORK FUNDAMENTALS (Based on Lecture 4)
# Estimated time: 2-3 hours
###############################################################################

# Section 1.1: Perceptron Implementation
# Implement a simple perceptron for binary classification

perceptron <- function(X, y, learning_rate = 0.01, epochs = 100) {
  # X: input matrix (n_samples x n_features)
  # y: binary target vector (0 or 1)
  # Returns: trained weights and training history
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  
  # Initialize weights and bias
  weights <- runif(n_features, -0.1, 0.1)
  bias <- 0
  
  # Store accuracy history
  accuracy_history <- numeric(epochs)
  
  for (epoch in 1:epochs) {
    correct <- 0
    
    for (i in 1:n_samples) {
      # Forward pass
      z <- sum(X[i,] * weights) + bias
      y_pred <- ifelse(z >= 0, 1, 0)
      
      # Update weights if prediction is wrong
      error <- y[i] - y_pred
      if (error != 0) {
        weights <- weights + learning_rate * error * X[i,]
        bias <- bias + learning_rate * error
      } else {
        correct <- correct + 1
      }
    }
    
    accuracy_history[epoch] <- correct / n_samples
  }
  
  return(list(
    weights = weights,
    bias = bias,
    accuracy_history = accuracy_history
  ))
}

# TASK 1.1: Test perceptron on linearly separable data
# Generate synthetic linearly separable data
set.seed(42)
n_samples <- 200

# Class 0: centered around (-2, -2)
class_0 <- matrix(rnorm(n_samples, mean = -2, sd = 0.8), ncol = 2)
# Class 1: centered around (2, 2)
class_1 <- matrix(rnorm(n_samples, mean = 2, sd = 0.8), ncol = 2)

X_linear <- rbind(class_0, class_1)
y_linear <- c(rep(0, n_samples/2), rep(1, n_samples/2))

# TODO: Train perceptron and visualize decision boundary
# Your code here:




# Section 1.2: Multi-Layer Perceptron with Backpropagation
# Implement activation functions and their derivatives

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

sigmoid_derivative <- function(z) {
  s <- sigmoid(z)
  s * (1 - s)
}

relu <- function(z) {
  pmax(0, z)
}

relu_derivative <- function(z) {
  ifelse(z > 0, 1, 0)
}

tanh_activation <- function(z) {
  tanh(z)
}

tanh_derivative <- function(z) {
  1 - tanh(z)^2
}

# TASK 1.2: Visualize activation functions and their derivatives
# TODO: Create plots comparing sigmoid, tanh, and ReLU
# Your code here:




# Section 1.3: XOR Problem - Demonstrating Need for Non-linearity
# The classic XOR problem that single perceptron cannot solve

# XOR dataset
X_xor <- matrix(c(0,0, 0,1, 1,0, 1,1), ncol=2, byrow=TRUE)
y_xor <- c(0, 1, 1, 0)

# TASK 1.3: Show that single perceptron fails on XOR
# Then use nnet package to solve with hidden layer
# TODO: Implement and compare results
# Your code here:




# Section 1.4: Feature Learning Visualization
# Demonstrate how neural networks learn hierarchical features

# Load iris dataset for multi-class classification
data(iris)
X_iris <- as.matrix(iris[, 1:4])
y_iris <- as.integer(iris$Species) - 1  # Convert to 0, 1, 2

# Normalize features
X_iris_scaled <- scale(X_iris)

# TASK 1.4: Train neural network and extract hidden layer representations
# TODO: Use nnet to train a network with one hidden layer
# Extract and visualize the hidden layer activations
# Your code here:




###############################################################################
# PART 2: TIME SERIES ANALYSIS (Based on Lecture 5)
# Estimated time: 3 hours
###############################################################################

# Section 2.1: Time Series Decomposition
# Analyze components of time series data

# Load AirPassengers dataset
data(AirPassengers)

# TASK 2.1: Perform time series decomposition
# TODO: Decompose into trend, seasonal, and residual components
# Compare additive vs multiplicative decomposition
# Your code here:




# Section 2.2: Stationarity Testing and Transformation

test_stationarity <- function(ts_data, significance = 0.05) {
  # Performs ADF test and returns interpretation
  adf_result <- adf.test(ts_data)
  
  interpretation <- ifelse(
    adf_result$p.value < significance,
    "Series is stationary",
    "Series is non-stationary"
  )
  
  return(list(
    test_statistic = adf_result$statistic,
    p_value = adf_result$p.value,
    interpretation = interpretation
  ))
}

# TASK 2.2: Make AirPassengers series stationary
# TODO: Apply differencing and/or log transformation
# Test stationarity before and after transformations
# Your code here:




# Section 2.3: ACF and PACF Analysis

analyze_correlations <- function(ts_data, max_lag = 40) {
  # Compute and interpret ACF/PACF
  
  acf_values <- acf(ts_data, lag.max = max_lag, plot = FALSE)
  pacf_values <- pacf(ts_data, lag.max = max_lag, plot = FALSE)
  
  # Find significant lags (outside confidence bounds)
  ci <- qnorm(0.975) / sqrt(length(ts_data))
  
  significant_acf <- which(abs(acf_values$acf[-1]) > ci)
  significant_pacf <- which(abs(pacf_values$acf) > ci)
  
  return(list(
    acf = acf_values,
    pacf = pacf_values,
    significant_acf_lags = significant_acf,
    significant_pacf_lags = significant_pacf
  ))
}

# TASK 2.3: Analyze correlation structure
# TODO: Generate ACF/PACF plots for original and differenced series
# Interpret patterns for ARIMA model selection
# Your code here:




# Section 2.4: ARIMA Model Building

build_arima_model <- function(ts_data, train_ratio = 0.8) {
  # Split data into train/test
  n <- length(ts_data)
  train_size <- floor(n * train_ratio)
  
  train_data <- window(ts_data, end = time(ts_data)[train_size])
  test_data <- window(ts_data, start = time(ts_data)[train_size + 1])
  
  # Automatic ARIMA selection
  model_auto <- auto.arima(train_data)
  
  # Forecast
  forecast_result <- forecast(model_auto, h = length(test_data))
  
  # Calculate accuracy metrics
  mape <- mean(abs((test_data - forecast_result$mean) / test_data)) * 100
  rmse <- sqrt(mean((test_data - forecast_result$mean)^2))
  
  return(list(
    model = model_auto,
    forecast = forecast_result,
    actual = test_data,
    mape = mape,
    rmse = rmse
  ))
}

# TASK 2.4: Build and evaluate ARIMA model
# TODO: Train ARIMA model and evaluate forecasting performance
# Your code here:




###############################################################################
# PART 3: UNSUPERVISED LEARNING (Based on Lecture 5)
# Estimated time: 3 hours
###############################################################################

# Section 3.1: K-Means Clustering Implementation

kmeans_from_scratch <- function(X, k, max_iter = 100, seed = 42) {
  # Simple k-means implementation for understanding
  set.seed(seed)
  n <- nrow(X)
  p <- ncol(X)
  
  # Initialize centers randomly
  centers <- X[sample(n, k), ]
  
  # Store cluster assignments
  clusters <- integer(n)
  
  for (iter in 1:max_iter) {
    old_clusters <- clusters
    
    # Assignment step: assign each point to nearest center
    for (i in 1:n) {
      distances <- apply(centers, 1, function(c) sum((X[i,] - c)^2))
      clusters[i] <- which.min(distances)
    }
    
    # Update step: recalculate centers
    for (j in 1:k) {
      if (sum(clusters == j) > 0) {
        centers[j, ] <- colMeans(X[clusters == j, , drop = FALSE])
      }
    }
    
    # Check convergence
    if (all(clusters == old_clusters)) {
      break
    }
  }
  
  # Calculate within-cluster sum of squares
  wcss <- sum(sapply(1:k, function(j) {
    if (sum(clusters == j) > 0) {
      sum(apply(X[clusters == j, , drop = FALSE], 1, 
                function(x) sum((x - centers[j,])^2)))
    } else {
      0
    }
  }))
  
  return(list(
    clusters = clusters,
    centers = centers,
    wcss = wcss,
    iterations = iter
  ))
}

# TASK 3.1: Apply k-means to iris dataset
# TODO: Compare your implementation with built-in kmeans
# Determine optimal number of clusters using elbow method
# Your code here:




# Section 3.2: Hierarchical Clustering

perform_hierarchical_clustering <- function(data, n_clusters = 3, 
                                           method = "complete") {
  # Compute distance matrix
  dist_matrix <- dist(data)
  
  # Perform hierarchical clustering
  hc <- hclust(dist_matrix, method = method)
  
  # Cut tree to get clusters
  clusters <- cutree(hc, k = n_clusters)
  
  return(list(
    hclust_object = hc,
    clusters = clusters,
    dist_matrix = dist_matrix
  ))
}

# TASK 3.2: Compare clustering methods
# TODO: Apply hierarchical clustering with different linkage methods
# Compare results with k-means clustering
# Your code here:




# Section 3.3: Association Rules Mining

# Generate synthetic transaction data for market basket analysis
generate_transaction_data <- function(n_transactions = 1000, 
                                     items = c("milk", "bread", "butter", 
                                              "beer", "diapers", "eggs",
                                              "cheese", "juice", "coffee")) {
  
  transactions <- lapply(1:n_transactions, function(i) {
    # Random number of items per transaction
    n_items <- sample(1:5, 1)
    
    # Select items with some patterns
    if (runif(1) > 0.7) {
      # Pattern: milk often with bread
      sample(c("milk", "bread", sample(items, n_items - 2)), n_items)
    } else if (runif(1) > 0.6) {
      # Pattern: beer often with diapers
      sample(c("beer", "diapers", sample(items, n_items - 2)), n_items)
    } else {
      sample(items, n_items)
    }
  })
  
  return(transactions)
}

# TASK 3.3: Find association rules
# TODO: Convert to transaction format and mine association rules
# Identify interesting patterns with high confidence
# Your code here:




# Section 3.4: Dimensionality Reduction with PCA

apply_pca <- function(data, n_components = 2) {
  # Standardize data
  data_scaled <- scale(data)
  
  # Apply PCA
  pca_result <- prcomp(data_scaled)
  
  # Calculate variance explained
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  cum_var_explained <- cumsum(var_explained)
  
  # Get transformed data
  transformed_data <- pca_result$x[, 1:n_components]
  
  return(list(
    pca = pca_result,
    transformed_data = transformed_data,
    var_explained = var_explained,
    cum_var_explained = cum_var_explained,
    loadings = pca_result$rotation[, 1:n_components]
  ))
}

# TASK 3.4: Apply PCA for visualization
# TODO: Reduce iris dataset to 2D and visualize clusters
# Interpret principal component loadings
# Your code here:




###############################################################################
# PART 4: INTEGRATED ANALYSIS
# Estimated time: 2 hours
###############################################################################

# Section 4.1: Combining Neural Networks with Time Series

# TASK 4.1: Use neural network for time series prediction
# TODO: Create lagged features from AirPassengers
# Train neural network for one-step-ahead prediction
# Compare with ARIMA results

create_lagged_features <- function(ts_data, n_lags = 12) {
  # Create matrix with lagged values for neural network input
  n <- length(ts_data)
  X <- matrix(NA, n - n_lags, n_lags)
  y <- numeric(n - n_lags)
  
  for (i in 1:n_lags) {
    X[, i] <- ts_data[(n_lags - i + 1):(n - i)]
  }
  y <- ts_data[(n_lags + 1):n]
  
  return(list(X = X, y = y))
}

# Your code here:




# Section 4.2: Clustering Time Series Patterns

# TASK 4.2: Cluster different time series patterns
# TODO: Generate multiple synthetic time series with different patterns
# Use DTW (Dynamic Time Warping) or feature-based clustering

generate_synthetic_patterns <- function(n_series = 20, length = 100) {
  patterns <- list()
  
  for (i in 1:n_series) {
    t <- 1:length
    
    # Random pattern type
    pattern_type <- sample(1:4, 1)
    
    if (pattern_type == 1) {
      # Trend
      patterns[[i]] <- 0.5 * t + rnorm(length, sd = 5)
    } else if (pattern_type == 2) {
      # Seasonal
      patterns[[i]] <- 10 * sin(2 * pi * t / 12) + rnorm(length, sd = 2)
    } else if (pattern_type == 3) {
      # Trend + Seasonal
      patterns[[i]] <- 0.3 * t + 5 * sin(2 * pi * t / 12) + rnorm(length, sd = 3)
    } else {
      # Random walk
      patterns[[i]] <- cumsum(rnorm(length))
    }
  }
  
  return(patterns)
}

# Your code here:




###############################################################################
# PART 5: QUESTIONS AND CONCEPTUAL UNDERSTANDING
# Answer these questions based on your implementation and analysis
###############################################################################

# Q1: Explain why the perceptron fails on XOR problem and how adding a hidden
#     layer solves this issue. Reference your visualization from Task 1.3.

# Q2: Compare the vanishing gradient problem between sigmoid and ReLU 
#     activation functions. Which performed better in your neural network?

# Q3: What transformation(s) were necessary to make AirPassengers stationary?
#     Explain why each transformation was needed.

# Q4: Based on ACF/PACF analysis, what ARIMA order did you select and why?
#     How did auto.arima's selection compare to your manual selection?

# Q5: Compare k-means and hierarchical clustering results on iris dataset.
#     Which method better captured the natural species groupings and why?

# Q6: What interesting association rules did you discover in the transaction
#     data? How would you use these in a real retail setting?

# Q7: How much variance was explained by first two principal components in PCA?
#     What features contributed most to PC1 and PC2?

# Q8: Compare neural network vs ARIMA for time series forecasting.
#     What are advantages/disadvantages of each approach?

###############################################################################
# DELIVERABLES CHECKLIST
###############################################################################

# [ ] Completed code for all tasks with comments
# [ ] Visualizations for each major section
# [ ] Answered all conceptual questions
# [ ] Summary report (1-2 pages) with key findings
# [ ] Performance metrics for all models
# [ ] Interpretation of results in business context


###############################################################################
# END OF COURSEWORK
###############################################################################
