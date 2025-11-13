################################################################################
# Classwork 2: Explainable AI (XAI) Techniques
# Deep Learning Introduction - Lecture 4 (Second Half)
# Duration: 30 minutes
# Prof. Asc. Endri Raco, Ph.D.
# November 2025
################################################################################

# INSTRUCTIONS:
# - Complete all exercises marked with TODO
# - Each exercise has allocated time (aim to stay within limits)
# - Focus on understanding XAI concepts through implementation
# - Submit completed R script with outputs and interpretations

# Load required libraries
if (!require("keras")) install.packages("keras")
if (!require("lime")) install.packages("lime")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(keras)
library(lime)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set seed for reproducibility
set.seed(2025)

################################################################################
# PART 1: FEATURE IMPORTANCE AND PERMUTATION IMPORTANCE (8 minutes)
################################################################################

cat("=" %.% rep(50), "\n")
cat("PART 1: FEATURE IMPORTANCE TECHNIQUES\n")
cat("=" %.% rep(50), "\n\n")

# Generate synthetic dataset for XAI demonstrations
generate_xai_data <- function(n = 1000) {
  # Create dataset where we know feature importance
  set.seed(42)
  
  # Features with known importance
  x1 <- rnorm(n)  # Very important
  x2 <- rnorm(n)  # Moderately important
  x3 <- rnorm(n)  # Slightly important
  x4 <- rnorm(n)  # Not important (noise)
  x5 <- rnorm(n)  # Not important (noise)
  
  # Target with known relationship
  y <- 3*x1 + 1.5*x2 + 0.5*x3 + rnorm(n, 0, 0.5)
  y <- ifelse(y > median(y), 1, 0)  # Binary classification
  
  data <- data.frame(
    feature1 = x1,
    feature2 = x2,
    feature3 = x3,
    feature4 = x4,
    feature5 = x5,
    target = y
  )
  
  return(data)
}

# Generate data
xai_data <- generate_xai_data()
X <- as.matrix(xai_data[, 1:5])
y <- xai_data$target

# Split data
train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

cat("Dataset created with known feature importance:\n")
cat("- feature1: Very important (weight = 3)\n")
cat("- feature2: Moderately important (weight = 1.5)\n")
cat("- feature3: Slightly important (weight = 0.5)\n")
cat("- feature4 & feature5: Not important (noise)\n\n")

# Exercise 1.1: Implement permutation importance (4 minutes)
permutation_importance <- function(model, X, y, n_repeats = 10, metric_func = NULL) {
  # TODO: Implement permutation importance algorithm
  
  # Default metric: accuracy for classification
  if (is.null(metric_func)) {
    metric_func <- function(y_true, y_pred) {
      mean(y_true == y_pred)
    }
  }
  
  # Get baseline score
  # TODO: Compute baseline performance
  # y_pred_baseline <- predict(model, X)
  # if (ncol(y_pred_baseline) > 1) {
  #   y_pred_baseline <- max.col(y_pred_baseline) - 1
  # } else {
  #   y_pred_baseline <- ifelse(y_pred_baseline > 0.5, 1, 0)
  # }
  # baseline_score <- metric_func(y, y_pred_baseline)
  
  # Initialize importance scores
  n_features <- ncol(X)
  importance_scores <- numeric(n_features)
  names(importance_scores) <- colnames(X)
  
  # TODO: For each feature, permute and measure performance drop
  # for (feat_idx in 1:n_features) {
  #   scores <- numeric(n_repeats)
  #   
  #   for (rep in 1:n_repeats) {
  #     # Create copy and permute feature
  #     X_permuted <- X
  #     X_permuted[, feat_idx] <- sample(X_permuted[, feat_idx])
  #     
  #     # Get predictions with permuted feature
  #     y_pred_perm <- predict(model, X_permuted)
  #     if (ncol(y_pred_perm) > 1) {
  #       y_pred_perm <- max.col(y_pred_perm) - 1
  #     } else {
  #       y_pred_perm <- ifelse(y_pred_perm > 0.5, 1, 0)
  #     }
  #     
  #     # Calculate performance
  #     scores[rep] <- metric_func(y, y_pred_perm)
  #   }
  #   
  #   # Importance = drop in performance
  #   importance_scores[feat_idx] <- baseline_score - mean(scores)
  # }
  
  return(importance_scores)
}

# Exercise 1.2: Train a neural network model (2 minutes)
# Build and train simple neural network
build_nn_model <- function(input_dim) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu", input_shape = c(input_dim)) %>%
    layer_dropout(0.3) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
  
  return(model)
}

# TODO: Build and train model
# nn_model <- build_nn_model(ncol(X_train))
# history <- nn_model %>% fit(
#   X_train, y_train,
#   epochs = 20,
#   batch_size = 32,
#   validation_split = 0.2,
#   verbose = 0
# )

# Exercise 1.3: Compute and visualize permutation importance (2 minutes)
# TODO: Apply permutation importance
# importance_scores <- permutation_importance(nn_model, X_test, y_test, n_repeats = 20)

# TODO: Create visualization
visualize_importance <- function(importance_scores) {
  # TODO: Create bar plot of feature importance
  # importance_df <- data.frame(
  #   Feature = names(importance_scores),
  #   Importance = importance_scores
  # ) %>%
  #   arrange(desc(Importance))
  # 
  # p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), 
  #                                 y = Importance)) +
  #   geom_col(fill = "steelblue") +
  #   coord_flip() +
  #   labs(title = "Permutation Feature Importance",
  #        subtitle = "Higher values indicate more important features",
  #        x = "Feature", y = "Importance (Accuracy Drop)") +
  #   theme_minimal()
  # 
  # print(p)
}

# TODO: Visualize results
# visualize_importance(importance_scores)

################################################################################
# PART 2: LIME - LOCAL INTERPRETABLE MODEL-AGNOSTIC EXPLANATIONS (10 minutes)
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("PART 2: LIME EXPLANATIONS\n")
cat("=" %.% rep(50), "\n\n")

# Exercise 2.1: Prepare data for LIME (2 minutes)
prepare_lime_data <- function() {
  # Use iris dataset for LIME demonstration
  data(iris)
  
  # Prepare data
  iris_modified <- iris %>%
    mutate(Species = as.numeric(Species) - 1)  # Convert to 0, 1, 2
  
  # Split features and target
  X_iris <- as.matrix(iris_modified[, 1:4])
  y_iris <- iris_modified$Species
  
  # Create train/test split
  set.seed(123)
  train_idx <- sample(1:nrow(X_iris), 0.8 * nrow(X_iris))
  
  return(list(
    X_train = X_iris[train_idx, ],
    y_train = y_iris[train_idx],
    X_test = X_iris[-train_idx, ],
    y_test = y_iris[-train_idx]
  ))
}

# TODO: Prepare data
# iris_data <- prepare_lime_data()

# Exercise 2.2: Build model for LIME explanation (3 minutes)
build_iris_model <- function(iris_data) {
  # TODO: Build and train a model for iris classification
  
  # Convert to categorical for multi-class
  # y_train_cat <- to_categorical(iris_data$y_train, 3)
  
  # model <- keras_model_sequential() %>%
  #   layer_dense(units = 10, activation = "relu", input_shape = c(4)) %>%
  #   layer_dropout(0.2) %>%
  #   layer_dense(units = 3, activation = "softmax")
  # 
  # model %>% compile(
  #   optimizer = "adam",
  #   loss = "categorical_crossentropy",
  #   metrics = c("accuracy")
  # )
  # 
  # # Train model
  # model %>% fit(
  #   iris_data$X_train, y_train_cat,
  #   epochs = 50,
  #   batch_size = 16,
  #   verbose = 0
  # )
  # 
  # return(model)
}

# TODO: Build model
# iris_model <- build_iris_model(iris_data)

# Exercise 2.3: Apply LIME explanation (5 minutes)
apply_lime_explanation <- function(model, train_data, test_instance, n_features = 4) {
  # TODO: Create LIME explainer and explain a prediction
  
  # Create explainer
  # explainer <- lime(
  #   x = as.data.frame(train_data),
  #   model = model,
  #   bin_continuous = TRUE,
  #   n_bins = 4
  # )
  # 
  # # Explain single prediction
  # explanation <- explain(
  #   x = as.data.frame(test_instance),
  #   explainer = explainer,
  #   n_labels = 1,
  #   n_features = n_features,
  #   n_permutations = 1000
  # )
  # 
  # return(explanation)
}

# TODO: Select an instance to explain
# instance_idx <- 5
# test_instance <- iris_data$X_test[instance_idx, , drop = FALSE]

# TODO: Get LIME explanation
# lime_explanation <- apply_lime_explanation(
#   iris_model, 
#   iris_data$X_train, 
#   test_instance
# )

# TODO: Visualize LIME explanation
visualize_lime <- function(explanation) {
  # TODO: Create visualization of LIME explanation
  # plot_features(explanation)
}

# TODO: Visualize
# visualize_lime(lime_explanation)

################################################################################
# PART 3: SHAP-INSPIRED ANALYSIS (8 minutes)
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("PART 3: SHAPLEY VALUE-INSPIRED ANALYSIS\n")
cat("=" %.% rep(50), "\n\n")

# Exercise 3.1: Implement simplified Shapley value calculation (4 minutes)
# Note: This is a simplified version for educational purposes
calculate_shapley_values <- function(model, X, instance_idx, n_samples = 100) {
  # TODO: Implement simplified Shapley value calculation
  
  # Get the instance to explain
  instance <- X[instance_idx, , drop = FALSE]
  n_features <- ncol(X)
  
  # Initialize Shapley values
  shapley_values <- numeric(n_features)
  names(shapley_values) <- colnames(X)
  
  # Get baseline prediction (mean of all predictions)
  # baseline_pred <- mean(predict(model, X))
  
  # TODO: For each feature, estimate its contribution
  # for (feat_idx in 1:n_features) {
  #   contributions <- numeric(n_samples)
  #   
  #   for (s in 1:n_samples) {
  #     # Randomly select subset of features
  #     other_features <- sample(setdiff(1:n_features, feat_idx), 
  #                            sample(1:(n_features-1), 1))
  #     
  #     # Create two versions: with and without the feature
  #     X_with <- X_without <- X[sample(nrow(X), 1), , drop = FALSE]
  #     
  #     # With feature: use instance value
  #     X_with[, c(feat_idx, other_features)] <- instance[, c(feat_idx, other_features)]
  #     
  #     # Without feature: use only other features
  #     X_without[, other_features] <- instance[, other_features]
  #     
  #     # Calculate marginal contribution
  #     pred_with <- predict(model, X_with)
  #     pred_without <- predict(model, X_without)
  #     
  #     contributions[s] <- pred_with - pred_without
  #   }
  #   
  #   shapley_values[feat_idx] <- mean(contributions)
  # }
  
  return(shapley_values)
}

# TODO: Calculate Shapley values for an instance
# instance_to_explain <- 1
# shapley_vals <- calculate_shapley_values(nn_model, X_test, instance_to_explain)

# Exercise 3.2: Create SHAP-style force plot (4 minutes)
create_force_plot <- function(shapley_values, base_value, prediction) {
  # TODO: Create a force plot visualization
  
  # Sort features by absolute Shapley value
  # sorted_idx <- order(abs(shapley_values), decreasing = TRUE)
  # sorted_features <- names(shapley_values)[sorted_idx]
  # sorted_values <- shapley_values[sorted_idx]
  # 
  # # Create cumulative sum for stacked bar effect
  # cumsum_values <- cumsum(c(base_value, sorted_values))
  # 
  # # Create dataframe for plotting
  # force_df <- data.frame(
  #   feature = c("Base", sorted_features),
  #   value = c(base_value, sorted_values),
  #   cumulative = cumsum_values,
  #   color = c("Base", ifelse(sorted_values > 0, "Positive", "Negative"))
  # )
  # 
  # # TODO: Create visualization
  # p <- ggplot(force_df, aes(x = 1, y = value, fill = color)) +
  #   geom_col(position = "stack") +
  #   coord_flip() +
  #   scale_fill_manual(values = c("Base" = "gray", 
  #                               "Positive" = "red", 
  #                               "Negative" = "blue")) +
  #   labs(title = "SHAP-style Force Plot",
  #        subtitle = sprintf("Prediction: %.3f", prediction),
  #        x = "", y = "Feature Contribution") +
  #   theme_minimal() +
  #   theme(axis.text.y = element_blank(),
  #         axis.ticks.y = element_blank())
  # 
  # print(p)
}

# TODO: Create force plot
# base_value <- 0.5  # Example base value
# prediction <- predict(nn_model, X_test[instance_to_explain, , drop = FALSE])
# create_force_plot(shapley_vals, base_value, prediction)

################################################################################
# PART 4: PRACTICAL XAI IMPLEMENTATION (4 minutes)
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("PART 4: PRACTICAL XAI IMPLEMENTATION\n")
cat("=" %.% rep(50), "\n\n")

# Exercise 4.1: Create comprehensive XAI analysis function (2 minutes)
comprehensive_xai_analysis <- function(model, X_train, X_test, y_test, instance_idx) {
  # TODO: Combine multiple XAI techniques
  
  cat("=" %.% rep(40), "\n")
  cat("COMPREHENSIVE XAI ANALYSIS\n")
  cat("=" %.% rep(40), "\n\n")
  
  # 1. Global feature importance
  cat("1. GLOBAL FEATURE IMPORTANCE (Permutation)\n")
  # TODO: Calculate and display permutation importance
  # perm_imp <- permutation_importance(model, X_test, y_test, n_repeats = 10)
  # print(round(perm_imp, 4))
  
  cat("\n2. LOCAL EXPLANATION (Instance", instance_idx, ")\n")
  
  # 2. Prediction for specific instance
  # instance <- X_test[instance_idx, , drop = FALSE]
  # prediction <- predict(model, instance)
  # cat("Prediction:", round(prediction, 3), "\n")
  
  # 3. Shapley values for instance
  cat("\n3. SHAPLEY VALUES\n")
  # TODO: Calculate and display Shapley values
  # shap_vals <- calculate_shapley_values(model, X_test, instance_idx, n_samples = 50)
  # print(round(shap_vals, 4))
  
  # 4. Feature contribution summary
  cat("\n4. FEATURE CONTRIBUTION SUMMARY\n")
  # TODO: Create summary of which features push prediction up/down
  # contribution_df <- data.frame(
  #   Feature = names(shap_vals),
  #   Contribution = shap_vals,
  #   Direction = ifelse(shap_vals > 0, "Increases", "Decreases")
  # )
  # print(contribution_df)
  
  return(list(
    permutation_importance = perm_imp,
    shapley_values = shap_vals,
    prediction = prediction
  ))
}

# TODO: Run comprehensive analysis
# xai_results <- comprehensive_xai_analysis(
#   nn_model, X_train, X_test, y_test, instance_idx = 1
# )

# Exercise 4.2: XAI validation check (2 minutes)
validate_xai_explanations <- function(model, X, explanations) {
  # TODO: Implement sanity checks for XAI explanations
  
  cat("\nVALIDATION CHECKS:\n")
  
  # Check 1: Do important features actually affect predictions?
  # TODO: Remove top feature and check prediction change
  
  # Check 2: Are explanations consistent across similar instances?
  # TODO: Find similar instances and compare explanations
  
  # Check 3: Do random features have low importance?
  # TODO: Add random feature and verify low importance
  
  return(TRUE)
}

################################################################################
# BONUS CHALLENGE: COUNTERFACTUAL EXPLANATIONS
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("BONUS: COUNTERFACTUAL EXPLANATIONS\n")
cat("=" %.% rep(50), "\n\n")

# Find minimal change to flip prediction
find_counterfactual <- function(model, instance, target_class, max_iterations = 100) {
  # TODO: Implement simple counterfactual search
  
  # Start with original instance
  counterfactual <- instance
  
  # TODO: Iteratively modify features to reach target class
  # for (iter in 1:max_iterations) {
  #   current_pred <- predict(model, counterfactual)
  #   
  #   # Check if we reached target
  #   if ((current_pred > 0.5) == target_class) {
  #     cat("Counterfactual found after", iter, "iterations\n")
  #     return(counterfactual)
  #   }
  #   
  #   # TODO: Modify features slightly toward target
  #   # (Simple gradient-based approach)
  # }
  
  return(NULL)
}

################################################################################
# XAI BEST PRACTICES CHECKLIST
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("XAI BEST PRACTICES CHECKLIST\n")
cat("=" %.% rep(50), "\n\n")

cat("Remember these XAI principles:\n")
cat("1. [ ] Use multiple XAI methods for robustness\n")
cat("2. [ ] Validate explanations with domain experts\n")
cat("3. [ ] Check explanation stability across similar instances\n")
cat("4. [ ] Consider both global and local explanations\n")
cat("5. [ ] Test explanations with perturbation analysis\n")
cat("6. [ ] Document XAI methodology and limitations\n")
cat("7. [ ] Ensure explanations are actionable for users\n")
cat("8. [ ] Address potential biases revealed by XAI\n")

################################################################################
# SUBMISSION REQUIREMENTS
################################################################################

cat("\n", "=" %.% rep(50), "\n")
cat("CLASSWORK 2 COMPLETION CHECKLIST\n")
cat("=" %.% rep(50), "\n\n")

cat("Before submitting, ensure you have:\n")
cat("[ ] Implemented permutation importance\n")
cat("[ ] Applied LIME to explain predictions\n")
cat("[ ] Calculated Shapley values\n")
cat("[ ] Created XAI visualizations\n")
cat("[ ] Completed comprehensive XAI analysis\n")
cat("[ ] Added interpretations of results\n")
cat("[ ] All code runs without errors\n")

cat("\n", "=" %.% rep(50), "\n")
cat("END OF CLASSWORK 2\n")
cat("Estimated completion time: 30 minutes\n")
cat("=" %.% rep(50), "\n")

# Save your workspace
save.image("classwork2_explainable_ai.RData")
cat("\nWorkspace saved as 'classwork2_explainable_ai.RData'\n")

# Generate summary report
cat("\n", "=" %.% rep(50), "\n")
cat("SUMMARY: KEY TAKEAWAYS FROM XAI EXERCISES\n")
cat("=" %.% rep(50), "\n\n")

cat("1. Permutation importance reveals global feature relevance\n")
cat("2. LIME provides local, interpretable explanations\n")
cat("3. Shapley values offer theoretically grounded attributions\n")
cat("4. Multiple XAI methods should be used for validation\n")
cat("5. XAI helps debug models and build trust\n")
cat("6. Explanations must be validated and actionable\n")