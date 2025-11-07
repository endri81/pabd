# ==============================================================================
# CLASSWORK 4: Feature Engineering and Preprocessing Pipeline
# ==============================================================================
# Course: Intermediate Predictive Analytics
# Instructor: Prof. Asc. Endri Raco, Ph.D.
# Topic: Basetable Construction and Feature Engineering
# Duration: 3 hours
# ==============================================================================

# ==============================================================================
# SETUP: Load Required Libraries
# ==============================================================================

# Install packages if needed (uncomment to install)
# install.packages(c("tidyverse", "lubridate", "fastDummies", "caret", 
#                    "recipes", "pROC", "corrplot"))

# Load libraries
library(tidyverse)
library(lubridate)
library(fastDummies)
library(caret)
library(recipes)
library(pROC)

cat("\n==============================================================================")
cat("\n                    CLASSWORK 4: FEATURE ENGINEERING")
cat("\n==============================================================================\n\n")

# ==============================================================================
# PART 1: DATA GENERATION
# ==============================================================================

cat("PART 1: Generating Synthetic Data...\n")

# Set seed for reproducibility
set.seed(123)

# Define data dimensions
n_donors <- 1000
n_transactions <- 5000

# Create donor demographics
donors <- tibble(
  donor_id = 1:n_donors,
  birth_date = as.Date("1950-01-01") + sample(0:25000, n_donors, replace = TRUE),
  gender = sample(c("M", "F", NA), n_donors, replace = TRUE, 
                  prob = c(0.45, 0.45, 0.10)),
  country = sample(c("USA", "UK", "Canada", "Germany", "France", NA), 
                   n_donors, replace = TRUE, 
                   prob = c(0.40, 0.25, 0.15, 0.10, 0.05, 0.05)),
  first_donation_date = as.Date("2015-01-01") + sample(0:2500, n_donors, 
                                                       replace = TRUE),
  email = ifelse(runif(n_donors) > 0.15, 
                 paste0("donor", 1:n_donors, "@email.com"), NA)
)

# Generate donation transactions
gifts <- tibble(
  donor_id = sample(1:n_donors, n_transactions, replace = TRUE),
  date = as.Date("2018-01-01") + sample(0:2190, n_transactions, replace = TRUE),
  amount = pmax(10, rnorm(n_transactions, mean = 75, sd = 150))
) %>%
  arrange(donor_id, date)

cat("✓ Generated", n_donors, "donors\n")
cat("✓ Generated", n_transactions, "transactions\n\n")

# ==============================================================================
# EXERCISE 1: TIMELINE DEFINITION (10 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 1: TIMELINE DEFINITION\n")
cat("==============================================================================\n")

# TODO 1.1: Define observation date
observation_date <- as.Date("2024-01-01")

# TODO 1.2: Define target period (3 months)
target_start <- observation_date
target_end <- observation_date + months(3)

# TODO 1.3: Define historical window (2 years)
history_start <- observation_date - years(2)
history_end <- observation_date

# TODO 1.4: Partition gifts data
gifts_historical <- gifts %>%
  filter(date >= history_start & date < history_end)

gifts_target <- gifts %>%
  filter(date >= target_start & date < target_end)

# Print summary
cat("\nTimeline Summary:\n")
cat("  Observation date:", as.character(observation_date), "\n")
cat("  Target period:", as.character(target_start), "to", 
    as.character(target_end), "\n")
cat("  Historical period:", as.character(history_start), "to", 
    as.character(history_end), "\n")
cat("  Historical transactions:", nrow(gifts_historical), "\n")
cat("  Target transactions:", nrow(gifts_target), "\n\n")

# ==============================================================================
# EXERCISE 2: POPULATION DEFINITION (10 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 2: POPULATION DEFINITION\n")
cat("==============================================================================\n")

# TODO 2.1: Identify donors with history
donors_with_history <- gifts_historical %>%
  distinct(donor_id)

cat("\nDonors with history:", nrow(donors_with_history), "\n")

# TODO 2.2: Exclude buffer period
buffer_start <- observation_date - months(1)
buffer_donations <- gifts %>%
  filter(date >= buffer_start & date < observation_date) %>%
  distinct(donor_id)

cat("Donors in buffer period:", nrow(buffer_donations), "\n")

# TODO 2.3: Create final population
population <- setdiff(donors_with_history$donor_id, buffer_donations$donor_id)

cat("Final population size:", length(population), "\n")

# TODO 2.4: Create basetable
basetable <- tibble(donor_id = population)

cat("✓ Basetable initialized with", nrow(basetable), "donors\n\n")

# ==============================================================================
# EXERCISE 3: TARGET VARIABLE (10 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 3: TARGET VARIABLE CREATION\n")
cat("==============================================================================\n")

# TODO 3.1: Aggregate target period donations
target_donations <- gifts_target %>%
  group_by(donor_id) %>%
  summarize(
    target_amount = sum(amount),
    target_count = n(),
    .groups = "drop"
  )

# TODO 3.2: Join to basetable
basetable <- basetable %>%
  left_join(target_donations, by = "donor_id")

# TODO 3.3: Create binary target
basetable <- basetable %>%
  mutate(
    target_amount = replace_na(target_amount, 0),
    target_count = replace_na(target_count, 0),
    target = as.integer(target_amount >= 50)
  )

# TODO 3.4: Check target distribution
target_summary <- basetable %>%
  count(target) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

cat("\nTarget Distribution:\n")
print(target_summary)

target_rate <- mean(basetable$target)
cat("\nTarget rate:", round(target_rate * 100, 2), "%\n")

# Note: Target rate is 0% because synthetic data doesn't extend to 2024
cat("(Note: 0% is expected with synthetic data not extending to target period)\n\n")

# ==============================================================================
# EXERCISE 4: RFM FEATURES (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 4: RFM FEATURES\n")
cat("==============================================================================\n")

# TODO 4.1: Calculate RFM features
rfm_features <- gifts_historical %>%
  group_by(donor_id) %>%
  summarize(
    # Recency
    days_since_last = as.numeric(observation_date - max(date)),
    days_since_first = as.numeric(observation_date - min(date)),
    
    # Frequency
    donation_count = n(),
    unique_months = n_distinct(floor_date(date, "month")),
    unique_years = n_distinct(year(date)),
    
    # Monetary
    total_donated = sum(amount),
    mean_donation = mean(amount),
    median_donation = median(amount),
    max_donation = max(amount),
    min_donation = min(amount),
    sd_donation = sd(amount),
    
    .groups = "drop"
  )

# TODO 4.2: Calculate coefficient of variation
rfm_features <- rfm_features %>%
  mutate(cv_donation = sd_donation / mean_donation)

# TODO 4.3: Join to basetable
basetable <- basetable %>%
  left_join(rfm_features, by = "donor_id")

cat("\n✓ RFM features created:", ncol(rfm_features) - 1, "features\n")
cat("\nRFM Summary Statistics:\n")
print(summary(basetable %>% 
                select(days_since_last, donation_count, mean_donation)))
cat("\n")

# ==============================================================================
# EXERCISE 5: TREND FEATURES (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 5: TREND FEATURES\n")
cat("==============================================================================\n")

# TODO 5.1: Calculate recent 3 months
recent_3m <- gifts_historical %>%
  filter(date >= observation_date - months(3)) %>%
  group_by(donor_id) %>%
  summarize(
    donations_3m = sum(amount),
    count_3m = n(),
    .groups = "drop"
  )

# TODO 5.2: Calculate previous 3 months
previous_3m <- gifts_historical %>%
  filter(date >= observation_date - months(6),
         date < observation_date - months(3)) %>%
  group_by(donor_id) %>%
  summarize(
    donations_prev3m = sum(amount),
    count_prev3m = n(),
    .groups = "drop"
  )

# TODO 5.3: Calculate last 12 months
donations_12m <- gifts_historical %>%
  filter(date >= observation_date - months(12)) %>%
  group_by(donor_id) %>%
  summarize(
    donations_12m = sum(amount),
    count_12m = n(),
    .groups = "drop"
  )

# TODO 5.4: Join trend features
basetable <- basetable %>%
  left_join(recent_3m, by = "donor_id") %>%
  left_join(previous_3m, by = "donor_id") %>%
  left_join(donations_12m, by = "donor_id")

# TODO 5.5: Calculate trend metrics
basetable <- basetable %>%
  mutate(
    across(c(donations_3m, count_3m, donations_prev3m, 
             count_prev3m, donations_12m, count_12m), 
           ~replace_na(., 0)),
    
    trend_absolute = donations_3m - donations_prev3m,
    trend_percent = (donations_3m - donations_prev3m) / 
      (donations_prev3m + 1) * 100,
    ratio_3m_to_12m = donations_3m / (donations_12m + 1)
  )

cat("\n✓ Trend features created\n")
cat("\nTrend Summary:\n")
print(summary(basetable %>% 
                select(trend_absolute, trend_percent, ratio_3m_to_12m)))
cat("\n")

# ==============================================================================
# EXERCISE 6: DEMOGRAPHIC FEATURES (10 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 6: DEMOGRAPHIC FEATURES\n")
cat("==============================================================================\n")

# TODO 6.1: Calculate age and tenure
donors_enhanced <- donors %>%
  mutate(
    age = as.numeric(observation_date - birth_date) / 365.25,
    tenure_days = as.numeric(observation_date - first_donation_date)
  )

# TODO 6.2: Join demographics
basetable <- basetable %>%
  left_join(
    donors_enhanced %>% 
      select(donor_id, age, gender, country, tenure_days, email),
    by = "donor_id"
  )

cat("\n✓ Demographic features added\n")
cat("\nAge Summary:\n")
print(summary(basetable$age))
cat("\nGender Distribution:\n")
print(table(basetable$gender, useNA = "ifany"))
cat("\nCountry Distribution:\n")
print(table(basetable$country, useNA = "ifany"))
cat("\n")

# ==============================================================================
# EXERCISE 7: MISSING VALUE HANDLING (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 7: MISSING VALUE HANDLING\n")
cat("==============================================================================\n")

# TODO 7.1: Check missing values
missing_summary <- basetable %>%
  summarize(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", 
               values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

cat("\nMissing Values by Feature:\n")
print(missing_summary, n = 20)

# TODO 7.2: Create missing indicators
basetable <- basetable %>%
  mutate(
    missing_gender = as.integer(is.na(gender)),
    missing_country = as.integer(is.na(country)),
    missing_email = as.integer(is.na(email)),
    missing_age = as.integer(is.na(age))
  )

# TODO 7.3: Impute age with median
age_median <- median(basetable$age, na.rm = TRUE)
basetable <- basetable %>%
  mutate(age = replace_na(age, age_median))

# TODO 7.4: Replace missing donation features with 0
basetable <- basetable %>%
  mutate(
    across(c(days_since_last, donation_count, total_donated, 
             mean_donation, median_donation, max_donation, 
             min_donation, sd_donation, cv_donation),
           ~replace_na(., 0))
  )

cat("\n✓ Missing values handled\n")
cat("Remaining NA in age:", sum(is.na(basetable$age)), "\n")
cat("Remaining NA in donation features:", 
    sum(is.na(basetable$mean_donation)), "\n\n")

# ==============================================================================
# EXERCISE 8: OUTLIER HANDLING (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 8: OUTLIER HANDLING\n")
cat("==============================================================================\n")

cat("\nBefore Outlier Handling:\n")
cat("Mean donation range:", 
    round(min(basetable$mean_donation), 2), "to", 
    round(max(basetable$mean_donation), 2), "\n")

# TODO 8.1: Winsorize mean_donation at 5th and 95th percentiles
p95_mean <- quantile(basetable$mean_donation, 0.95, na.rm = TRUE)
p05_mean <- quantile(basetable$mean_donation, 0.05, na.rm = TRUE)

basetable <- basetable %>%
  mutate(
    mean_donation_capped = pmin(pmax(mean_donation, p05_mean), p95_mean)
  )

# TODO 8.2: Winsorize max_donation
p95_max <- quantile(basetable$max_donation, 0.95, na.rm = TRUE)
p05_max <- quantile(basetable$max_donation, 0.05, na.rm = TRUE)

basetable <- basetable %>%
  mutate(
    max_donation_capped = pmin(pmax(max_donation, p05_max), p95_max)
  )

cat("\nAfter Outlier Handling:\n")
cat("Mean donation capped range:", 
    round(min(basetable$mean_donation_capped), 2), "to", 
    round(max(basetable$mean_donation_capped), 2), "\n")

cat("\nValues changed:\n")
cat("  Mean donation:", 
    sum(basetable$mean_donation != basetable$mean_donation_capped), "\n")
cat("  Max donation:", 
    sum(basetable$max_donation != basetable$max_donation_capped), "\n\n")

# ==============================================================================
# EXERCISE 9: FEATURE TRANSFORMATIONS (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 9: FEATURE TRANSFORMATIONS\n")
cat("==============================================================================\n")

# TODO 9.1: Log transformations
basetable <- basetable %>%
  mutate(
    log_mean_donation = log1p(mean_donation_capped),
    log_total_donated = log1p(total_donated),
    log_max_donation = log1p(max_donation_capped),
    log_tenure = log1p(tenure_days)
  )

# TODO 9.2: Interaction features
basetable <- basetable %>%
  mutate(
    rfm_score = (1 / (days_since_last + 1)) * 
      donation_count * mean_donation_capped,
    freq_recency = donation_count / (days_since_last + 1),
    trend_strength = abs(trend_percent) * total_donated,
    consistency_score = 1 - pmin(cv_donation / 2, 1)
  )

# TODO 9.3: Ratio features
basetable <- basetable %>%
  mutate(
    max_to_total_ratio = max_donation_capped / (total_donated + 1),
    annual_frequency = donation_count / (tenure_days / 365.25 + 1),
    lifetime_value_rate = total_donated / (tenure_days + 1) * 365
  )

cat("\n✓ Transformations complete\n")
cat("  Log transformations: 4 features\n")
cat("  Interaction features: 4 features\n")
cat("  Ratio features: 3 features\n\n")

# ==============================================================================
# EXERCISE 10: CATEGORICAL ENCODING (10 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 10: CATEGORICAL ENCODING\n")
cat("==============================================================================\n")

# TODO 10.1: Replace NA in categoricals
basetable <- basetable %>%
  mutate(
    gender = replace_na(gender, "Unknown"),
    country = replace_na(country, "Unknown")
  )

cat("\nCategorical Variables:\n")
cat("  Gender levels:", 
    paste(unique(basetable$gender), collapse = ", "), "\n")
cat("  Country levels:", length(unique(basetable$country)), "countries\n")

# TODO 10.2: Create dummy variables
basetable <- dummy_cols(
  basetable,
  select_columns = c("gender", "country"),
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

cat("\n✓ Dummy variables created\n")
cat("Total features now:", ncol(basetable), "\n\n")

# ==============================================================================
# EXERCISE 11: FEATURE SCALING (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 11: FEATURE SCALING\n")
cat("==============================================================================\n")

# TODO 11.1: Split train/test
set.seed(42)
train_index <- createDataPartition(
  basetable$target,
  p = 0.7,
  list = FALSE
)

train_data <- basetable[train_index, ]
test_data <- basetable[-train_index, ]

cat("\nData Split:\n")
cat("  Training set:", nrow(train_data), "observations\n")
cat("  Test set:", nrow(test_data), "observations\n")
cat("  Target rate (train):", round(mean(train_data$target) * 100, 2), "%\n")
cat("  Target rate (test):", round(mean(test_data$target) * 100, 2), "%\n")

# TODO 11.2: Identify numeric features to scale
numeric_features <- c(
  "days_since_last", "donation_count", "total_donated",
  "mean_donation_capped", "max_donation_capped", "log_mean_donation",
  "log_total_donated", "rfm_score", "freq_recency", "age",
  "tenure_days", "trend_percent", "ratio_3m_to_12m"
)

# TODO 11.3: Calculate scaling parameters
scaling_params <- train_data %>%
  summarize(across(
    all_of(numeric_features),
    list(mean = ~mean(., na.rm = TRUE),
         sd = ~sd(., na.rm = TRUE))
  ))

# TODO 11.4: Save parameters
saveRDS(scaling_params, "scaling_parameters.rds")
cat("\n✓ Scaling parameters saved to 'scaling_parameters.rds'\n")

# TODO 11.5: Apply standardization
train_scaled <- train_data
test_scaled <- test_data

for (feat in numeric_features) {
  mean_col <- paste0(feat, "_mean")
  sd_col <- paste0(feat, "_sd")
  
  train_scaled[[feat]] <- (train_data[[feat]] - 
                             scaling_params[[mean_col]]) /
    (scaling_params[[sd_col]] + 1e-10)
  
  test_scaled[[feat]] <- (test_data[[feat]] - 
                            scaling_params[[mean_col]]) /
    (scaling_params[[sd_col]] + 1e-10)
}

cat("✓ Features scaled successfully\n\n")

# ==============================================================================
# EXERCISE 12: FEATURE SELECTION (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 12: FEATURE SELECTION\n")
cat("==============================================================================\n")

# TODO 12.1: Calculate correlations with target
feature_cols <- setdiff(names(train_scaled), 
                        c("donor_id", "target", "target_amount", 
                          "target_count", "email"))

numeric_cols <- feature_cols[sapply(train_scaled[feature_cols], is.numeric)]

correlations <- sapply(numeric_cols, function(col) {
  cor(train_scaled[[col]], train_scaled$target, use = "complete.obs")
})

cor_df <- tibble(
  feature = names(correlations),
  correlation = correlations
) %>%
  arrange(desc(abs(correlation)))

cat("\nTop 15 Features by Correlation with Target:\n")
print(head(cor_df, 15))

# TODO 12.2: Check multicollinearity
cor_matrix <- cor(train_scaled %>% select(all_of(numeric_cols)), 
                  use = "complete.obs")

high_cor_pairs <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, 
                        arr.ind = TRUE)

if (nrow(high_cor_pairs) > 0) {
  cat("\nHighly Correlated Pairs (|r| > 0.9):\n")
  for (i in 1:min(5, nrow(high_cor_pairs))) {
    row_idx <- high_cor_pairs[i, 1]
    col_idx <- high_cor_pairs[i, 2]
    cat("  ", rownames(cor_matrix)[row_idx], "-", 
        colnames(cor_matrix)[col_idx], ":", 
        round(cor_matrix[row_idx, col_idx], 3), "\n")
  }
}

# TODO 12.3: Select top features
top_features <- head(cor_df, 20)$feature

cat("\n✓ Selected", length(top_features), "features for modeling\n\n")

# ==============================================================================
# EXERCISE 13: BUILD MODEL (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 13: BUILD LOGISTIC REGRESSION MODEL\n")
cat("==============================================================================\n")

# TODO 13.1: Prepare model data
model_features <- c(top_features, "target")

train_model <- train_scaled %>%
  select(all_of(model_features)) %>%
  na.omit()

test_model <- test_scaled %>%
  select(all_of(model_features)) %>%
  na.omit()

cat("\nModel Data:\n")
cat("  Training observations:", nrow(train_model), "\n")
cat("  Test observations:", nrow(test_model), "\n")

# TODO 13.2: Train logistic regression
cat("\nTraining logistic regression model...\n")

logistic_model <- glm(
  target ~ .,
  data = train_model,
  family = "binomial"
)

cat("✓ Model trained successfully\n")

# TODO 13.3: Make predictions
train_predictions <- predict(logistic_model, 
                             newdata = train_model, 
                             type = "response")

test_predictions <- predict(logistic_model, 
                            newdata = test_model, 
                            type = "response")

# TODO 13.4: Evaluate performance
train_roc <- roc(train_model$target, train_predictions, quiet = TRUE)
test_roc <- roc(test_model$target, test_predictions, quiet = TRUE)

cat("\nModel Performance:\n")
cat("  Training AUC:", round(auc(train_roc), 3), "\n")
cat("  Test AUC:", round(auc(test_roc), 3), "\n")

# TODO 13.5: Confusion matrix
threshold <- 0.5
test_pred_binary <- as.integer(test_predictions > threshold)

conf_matrix <- table(Predicted = test_pred_binary, 
                     Actual = test_model$target)

cat("\nConfusion Matrix (threshold = 0.5):\n")
print(conf_matrix)

if (all(dim(conf_matrix) == c(2, 2))) {
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
  recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
  f1 <- 2 * precision * recall / (precision + recall)
  
  cat("\nMetrics:\n")
  cat("  Accuracy:", round(accuracy, 3), "\n")
  cat("  Precision:", round(precision, 3), "\n")
  cat("  Recall:", round(recall, 3), "\n")
  cat("  F1 Score:", round(f1, 3), "\n")
}

cat("\n")

# ==============================================================================
# EXERCISE 14: FEATURE IMPORTANCE (10 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 14: FEATURE IMPORTANCE\n")
cat("==============================================================================\n")

# TODO 14.1: Extract coefficients
coefficients <- coef(logistic_model)

coef_df <- tibble(
  feature = names(coefficients),
  coefficient = as.vector(coefficients),
  abs_coefficient = abs(as.vector(coefficients))
) %>%
  filter(feature != "(Intercept)") %>%
  arrange(desc(abs_coefficient))

cat("\nTop 10 Most Important Features:\n")
print(head(coef_df, 10))

cat("\n")

# ==============================================================================
# EXERCISE 15: PRODUCTION PIPELINE (15 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 15: CREATE PRODUCTION PIPELINE\n")
cat("==============================================================================\n")

# TODO 15.1: Create feature engineering function
engineer_all_features <- function(gifts_data, donors_data, reference_date) {
  
  # Historical period
  history_start <- reference_date - years(2)
  gifts_hist <- gifts_data %>%
    filter(date >= history_start & date < reference_date)
  
  # RFM features
  rfm <- gifts_hist %>%
    group_by(donor_id) %>%
    summarize(
      days_since_last = as.numeric(reference_date - max(date)),
      donation_count = n(),
      total_donated = sum(amount),
      mean_donation = mean(amount),
      max_donation = max(amount),
      sd_donation = sd(amount),
      .groups = "drop"
    ) %>%
    mutate(cv_donation = sd_donation / mean_donation)
  
  # Trend features
  recent_3m <- gifts_hist %>%
    filter(date >= reference_date - months(3)) %>%
    group_by(donor_id) %>%
    summarize(donations_3m = sum(amount), .groups = "drop")
  
  previous_3m <- gifts_hist %>%
    filter(date >= reference_date - months(6),
           date < reference_date - months(3)) %>%
    group_by(donor_id) %>%
    summarize(donations_prev3m = sum(amount), .groups = "drop")
  
  # Demographics
  demographics <- donors_data %>%
    mutate(
      age = as.numeric(reference_date - birth_date) / 365.25,
      tenure_days = as.numeric(reference_date - first_donation_date)
    ) %>%
    select(donor_id, age, gender, country, tenure_days)
  
  # Combine
  basetable <- demographics %>%
    left_join(rfm, by = "donor_id") %>%
    left_join(recent_3m, by = "donor_id") %>%
    left_join(previous_3m, by = "donor_id")
  
  # Handle missing and transformations
  basetable <- basetable %>%
    mutate(
      age = replace_na(age, median(age, na.rm = TRUE)),
      across(c(days_since_last, donation_count, total_donated, 
               mean_donation, donations_3m, donations_prev3m),
             ~replace_na(., 0)),
      
      log_mean_donation = log1p(mean_donation),
      log_total_donated = log1p(total_donated),
      rfm_score = (1 / (days_since_last + 1)) * 
        donation_count * mean_donation,
      trend_percent = (donations_3m - donations_prev3m) / 
        (donations_prev3m + 1) * 100
    )
  
  return(basetable)
}

# TODO 15.2: Test pipeline
cat("\nTesting production pipeline...\n")
test_features <- engineer_all_features(gifts, donors, observation_date)

cat("✓ Pipeline test successful\n")
cat("  Features created:", ncol(test_features), "\n")
cat("  Observations:", nrow(test_features), "\n")

# TODO 15.3: Save pipeline
saveRDS(engineer_all_features, "feature_engineering_pipeline.rds")
cat("✓ Pipeline saved to 'feature_engineering_pipeline.rds'\n\n")

# ==============================================================================
# EXERCISE 16: VALIDATION (10 minutes)
# ==============================================================================

cat("==============================================================================\n")
cat("EXERCISE 16: VALIDATION AND TESTING\n")
cat("==============================================================================\n")

# TODO 16.1: Create validation function
validate_features <- function(basetable) {
  
  checks <- list()
  
  # Check missing values
  key_features <- c("donor_id", "age", "days_since_last", 
                    "donation_count", "mean_donation")
  missing_count <- sum(is.na(basetable[key_features]))
  checks$missing_values <- missing_count == 0
  
  # Check infinite values
  numeric_cols <- names(basetable)[sapply(basetable, is.numeric)]
  infinite_count <- sum(sapply(basetable[numeric_cols], 
                               function(x) sum(is.infinite(x))))
  checks$no_infinite <- infinite_count == 0
  
  # Check unique IDs
  checks$unique_ids <- length(unique(basetable$donor_id)) == nrow(basetable)
  
  # Check value ranges
  checks$age_range <- all(basetable$age >= 0 & basetable$age <= 120, 
                          na.rm = TRUE)
  checks$donation_positive <- all(basetable$mean_donation >= 0, 
                                  na.rm = TRUE)
  
  return(checks)
}

# TODO 16.2: Run validation
validation_results <- validate_features(basetable)

cat("\nValidation Results:\n")
for (check_name in names(validation_results)) {
  status <- if(validation_results[[check_name]]) "✓ PASS" else "✗ FAIL"
  cat("  ", check_name, ":", status, "\n")
}

# TODO 16.3: Data quality report
data_quality_report <- tibble(
  metric = c("Total observations", "Total features", 
             "Target rate", "Missing values",
             "Infinite values", "Duplicate IDs"),
  value = c(
    nrow(basetable),
    ncol(basetable),
    paste0(round(mean(basetable$target, na.rm = TRUE) * 100, 2), "%"),
    sum(is.na(basetable)),
    sum(sapply(basetable %>% select(where(is.numeric)), 
               function(x) sum(is.infinite(x)))),
    sum(duplicated(basetable$donor_id))
  )
)

cat("\nData Quality Report:\n")
print(data_quality_report, n = Inf)

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n==============================================================================")
cat("\n                         CLASSWORK 4 COMPLETED!")
cat("\n==============================================================================\n\n")

cat("Exercises Completed:\n")
cat("  ✓ Exercise 1: Timeline Definition\n")
cat("  ✓ Exercise 2: Population Definition\n")
cat("  ✓ Exercise 3: Target Variable Creation\n")
cat("  ✓ Exercise 4: RFM Features\n")
cat("  ✓ Exercise 5: Trend Features\n")
cat("  ✓ Exercise 6: Demographic Features\n")
cat("  ✓ Exercise 7: Missing Value Handling\n")
cat("  ✓ Exercise 8: Outlier Handling\n")
cat("  ✓ Exercise 9: Feature Transformations\n")
cat("  ✓ Exercise 10: Categorical Encoding\n")
cat("  ✓ Exercise 11: Feature Scaling\n")
cat("  ✓ Exercise 12: Feature Selection\n")
cat("  ✓ Exercise 13: Model Building\n")
cat("  ✓ Exercise 14: Feature Importance\n")
cat("  ✓ Exercise 15: Production Pipeline\n")
cat("  ✓ Exercise 16: Validation and Testing\n\n")

cat("Files Created:\n")
cat("  • scaling_parameters.rds\n")
cat("  • feature_engineering_pipeline.rds\n\n")

cat("Final Statistics:\n")
cat("  • Training observations:", nrow(train_model), "\n")
cat("  • Test observations:", nrow(test_model), "\n")
cat("  • Total features:", ncol(basetable), "\n")
cat("  • Test AUC:", round(auc(test_roc), 3), "\n\n")

cat("==============================================================================\n")
cat("🎉 Congratulations! You've completed Feature Engineering Classwork 4! 🎉\n")
cat("==============================================================================\n\n")

# Export final basetable (optional)
# write_csv(basetable, "final_basetable.csv")
# cat("Final basetable exported to 'final_basetable.csv'\n")