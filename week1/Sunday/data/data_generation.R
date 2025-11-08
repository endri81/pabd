# ============================================================================
# DATA GENERATION SCRIPT FOR LECTURE 3
# Classification Trees and Network-Based Prediction
# Prof. Asc. Endri Raco, Ph.D.
# Departamenti i Inxhinierisë Matematike, UPT
# ============================================================================
#
# Ky script gjeneron dataset-et e nevojshme për Lecture 3:
# 1. diabetes.csv - Dataset për classification trees (binary outcome)
# 2. chocolate.csv - Dataset për regression trees (continuous outcome)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("  LECTURE 3: CLASSIFICATION AND REGRESSION TREES\n")
cat("  Machine Learning with Tree-Based Models\n")
cat("============================================================================\n\n")

# Vendosim seed për riprodhueshmëri
set.seed(2023)

# ============================================================================
# PJESA 1: LIBRARITË DHE PARAMETRAT
# ============================================================================

cat("STEP 1: Loading required libraries...\n")

required_packages <- c("tidyverse")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste0("  Installing ", pkg, "...\n"))
    install.packages(pkg, repos = "http://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

library(tidyverse)

cat("  Libraries loaded successfully!\n\n")

# ============================================================================
# PJESA 2: GJENERIMI I DIABETES DATASET
# ============================================================================

cat("============================================================================\n")
cat("STEP 2: Generating DIABETES dataset (Classification)\n")
cat("============================================================================\n")

cat("  Creating diabetes classification dataset...\n")

# Parametrat
n_diabetes <- 768  # Madhësia e dataset (si Pima Indians Diabetes)

# Variablat bazë
diabetes <- tibble(
  # Numeric predictors
  pregnancies = rpois(n_diabetes, lambda = 3.5),
  glucose = round(pmax(0, rnorm(n_diabetes, mean = 120, sd = 32))),
  blood_pressure = round(pmax(40, pmin(130, rnorm(n_diabetes, mean = 72, sd = 12)))),
  skin_thickness = round(pmax(0, rnorm(n_diabetes, mean = 20, sd = 15))),
  insulin = round(pmax(0, rgamma(n_diabetes, shape = 2, scale = 60))),
  bmi = round(pmax(15, pmin(60, rnorm(n_diabetes, mean = 32, sd = 8))), 1),
  age = round(pmax(21, pmin(81, rnorm(n_diabetes, mean = 33, sd = 12))))
)

# Përcaktoni outcome bazuar në një logistic model të fshehur
# Diabetes është më e mundur me:
# - High glucose
# - High BMI  
# - More pregnancies
# - Older age
# - High blood pressure

log_odds <- -8 +
  0.04 * diabetes$glucose +
  0.08 * diabetes$bmi +
  0.15 * diabetes$pregnancies +
  0.03 * diabetes$age +
  0.02 * diabetes$blood_pressure

# Add some random noise
log_odds <- log_odds + rnorm(n_diabetes, mean = 0, sd = 1)

# Convert to probability
prob_diabetes <- 1 / (1 + exp(-log_odds))

# Generate outcome
diabetes <- diabetes %>%
  mutate(
    outcome = factor(ifelse(rbinom(n_diabetes, 1, prob_diabetes) == 1, "yes", "no"),
                     levels = c("no", "yes"))
  )

# Reorder columns to put outcome first
diabetes <- diabetes %>%
  select(outcome, everything())

cat("  Diabetes dataset created!\n")
cat("    Total observations:", nrow(diabetes), "\n")
cat("    Diabetes prevalence:", round(mean(diabetes$outcome == "yes") * 100, 1), "%\n")
cat("    Variables:\n")
cat("      - outcome (factor): yes/no\n")
cat("      - pregnancies (numeric)\n")
cat("      - glucose (numeric)\n")
cat("      - blood_pressure (numeric)\n")
cat("      - skin_thickness (numeric)\n")
cat("      - insulin (numeric)\n")
cat("      - bmi (numeric)\n")
cat("      - age (numeric)\n\n")

# Summary statistics
cat("  Key statistics:\n")
cat("    Glucose: mean =", round(mean(diabetes$glucose), 1), 
    ", range = [", min(diabetes$glucose), "-", max(diabetes$glucose), "]\n")
cat("    BMI: mean =", round(mean(diabetes$bmi), 1),
    ", range = [", round(min(diabetes$bmi), 1), "-", round(max(diabetes$bmi), 1), "]\n")
cat("    Age: mean =", round(mean(diabetes$age), 1),
    ", range = [", min(diabetes$age), "-", max(diabetes$age), "]\n\n")

# ============================================================================
# PJESA 3: GJENERIMI I CHOCOLATE DATASET
# ============================================================================

cat("============================================================================\n")
cat("STEP 3: Generating CHOCOLATE dataset (Regression)\n")
cat("============================================================================\n")

cat("  Creating chocolate ratings dataset...\n")

# Parametrat
n_chocolate <- 1795  # Madhësia e dataset

# Company locations (weighted distribution)
company_locations <- c("U.S.A.", "France", "Canada", "U.K.", "Italy", 
                       "Ecuador", "Australia", "Belgium", "Switzerland",
                       "Germany", "Peru", "Madagascar", "Colombia", 
                       "Guatemala", "Venezuela", "Other")

company_location_probs <- c(0.20, 0.12, 0.08, 0.08, 0.06,
                            0.05, 0.04, 0.04, 0.04,
                            0.03, 0.03, 0.03, 0.03,
                            0.03, 0.03, 0.11)

# Bean types
bean_types <- c("Trinitario", "Criollo", "Forastero", 
                "Criollo, Trinitario", "Trinitario, Forastero",
                "Blend", "")

bean_type_probs <- c(0.35, 0.20, 0.25, 0.08, 0.05, 0.05, 0.02)

# Broad bean origins
broad_bean_origins <- c("Venezuela", "Ecuador", "Peru", "Dominican Republic",
                        "Madagascar", "Ghana", "Colombia", "Bolivia",
                        "Nicaragua", "Papua New Guinea", "Belize",
                        "Trinidad", "Costa Rica", "Vietnam", "Other")

bean_origin_probs <- c(0.12, 0.10, 0.09, 0.08, 0.08, 0.07, 0.06, 0.05,
                       0.05, 0.05, 0.04, 0.04, 0.03, 0.03, 0.11)

# Generate base data
chocolate <- tibble(
  review_date = sample(2006:2021, n_chocolate, replace = TRUE),
  company_location = sample(company_locations, n_chocolate, 
                            replace = TRUE, prob = company_location_probs),
  cocoa_percent = round(runif(n_chocolate, min = 0.42, max = 1.00), 2),
  bean_type = sample(bean_types, n_chocolate, 
                     replace = TRUE, prob = bean_type_probs),
  broad_bean_origin = sample(broad_bean_origins, n_chocolate,
                             replace = TRUE, prob = bean_origin_probs)
)

# Generate rating based on hidden relationships
# Higher quality tends to be associated with:
# - Certain origins (Madagascar, Ecuador, Venezuela)
# - Certain bean types (Criollo, Trinitario)
# - Specific cocoa percentages (70-75%)
# - More recent reviews

base_rating <- 3.0

# Effect of cocoa percentage (optimal around 70-75%)
cocoa_effect <- -2 * abs(chocolate$cocoa_percent - 0.72)

# Effect of bean type
bean_effect <- case_when(
  chocolate$bean_type == "Criollo" ~ 0.3,
  chocolate$bean_type == "Trinitario" ~ 0.2,
  chocolate$bean_type == "Criollo, Trinitario" ~ 0.25,
  chocolate$bean_type == "Forastero" ~ 0,
  chocolate$bean_type == "Blend" ~ -0.1,
  TRUE ~ 0.1
)

# Effect of origin
origin_effect <- case_when(
  chocolate$broad_bean_origin %in% c("Madagascar", "Ecuador", "Venezuela") ~ 0.25,
  chocolate$broad_bean_origin %in% c("Peru", "Dominican Republic") ~ 0.15,
  chocolate$broad_bean_origin %in% c("Ghana", "Colombia") ~ 0.10,
  TRUE ~ 0
)

# Effect of company location (reputation effect)
company_effect <- case_when(
  chocolate$company_location %in% c("France", "Switzerland", "Belgium") ~ 0.15,
  chocolate$company_location %in% c("Italy", "U.K.") ~ 0.10,
  chocolate$company_location == "U.S.A." ~ 0.05,
  TRUE ~ 0
)

# Time trend (slight improvement over years)
year_effect <- (chocolate$review_date - 2006) * 0.02

# Combine effects
chocolate <- chocolate %>%
  mutate(
    final_grade = base_rating + cocoa_effect + bean_effect + 
      origin_effect + company_effect + year_effect +
      rnorm(n_chocolate, 0, 0.3)
  ) %>%
  mutate(
    # Clip to valid range
    final_grade = pmax(1, pmin(5, final_grade)),
    # Round to quarters (1.0, 1.25, 1.5, 1.75, 2.0, etc.)
    final_grade = round(final_grade * 4) / 4
  )

# Reorder columns
chocolate <- chocolate %>%
  select(final_grade, review_date, cocoa_percent, company_location, 
         bean_type, broad_bean_origin)

# Convert to factors
chocolate <- chocolate %>%
  mutate(
    company_location = factor(company_location),
    bean_type = factor(bean_type),
    broad_bean_origin = factor(broad_bean_origin)
  )

cat("  Chocolate dataset created!\n")
cat("    Total observations:", nrow(chocolate), "\n")
cat("    Mean rating:", round(mean(chocolate$final_grade), 2), "\n")
cat("    Rating range:", min(chocolate$final_grade), "to", max(chocolate$final_grade), "\n")
cat("    Variables:\n")
cat("      - final_grade (numeric): 1-5 scale\n")
cat("      - review_date (numeric): year\n")
cat("      - cocoa_percent (numeric): 0-1\n")
cat("      - company_location (factor): country\n")
cat("      - bean_type (factor): type of cocoa bean\n")
cat("      - broad_bean_origin (factor): origin country\n\n")

# Summary statistics
cat("  Key statistics:\n")
cat("    Rating distribution:\n")
print(table(chocolate$final_grade))
cat("\n    Cocoa percent: mean =", round(mean(chocolate$cocoa_percent), 2),
    ", range = [", min(chocolate$cocoa_percent), "-", max(chocolate$cocoa_percent), "]\n")
cat("    Review years:", min(chocolate$review_date), "to", max(chocolate$review_date), "\n")
cat("    Number of bean types:", length(unique(chocolate$bean_type)), "\n")
cat("    Number of origins:", length(unique(chocolate$broad_bean_origin)), "\n\n")

# ============================================================================
# PJESA 4: TRAIN/TEST SPLITS (EXAMPLE)
# ============================================================================

cat("============================================================================\n")
cat("STEP 4: Creating example TRAIN/TEST splits\n")
cat("============================================================================\n")

# Note: Studentët do të përdorin tidymodels::initial_split() në leksion
# Por krijojmë splits për referim

# Diabetes split (90/10)
set.seed(2023)
diabetes_indices <- sample(1:nrow(diabetes))
train_size_diabetes <- round(0.9 * nrow(diabetes))

diabetes_train <- diabetes[diabetes_indices[1:train_size_diabetes], ]
diabetes_test <- diabetes[diabetes_indices[(train_size_diabetes + 1):nrow(diabetes)], ]

cat("  Diabetes split:\n")
cat("    Training:", nrow(diabetes_train), "observations\n")
cat("    Test:", nrow(diabetes_test), "observations\n")
cat("    Training prevalence:", 
    round(mean(diabetes_train$outcome == "yes") * 100, 1), "%\n")
cat("    Test prevalence:", 
    round(mean(diabetes_test$outcome == "yes") * 100, 1), "%\n\n")

# Chocolate split (80/20)
set.seed(2023)
chocolate_indices <- sample(1:nrow(chocolate))
train_size_chocolate <- round(0.8 * nrow(chocolate))

chocolate_train <- chocolate[chocolate_indices[1:train_size_chocolate], ]
chocolate_test <- chocolate[chocolate_indices[(train_size_chocolate + 1):nrow(chocolate)], ]

cat("  Chocolate split:\n")
cat("    Training:", nrow(chocolate_train), "observations\n")
cat("    Test:", nrow(chocolate_test), "observations\n")
cat("    Training mean rating:", round(mean(chocolate_train$final_grade), 2), "\n")
cat("    Test mean rating:", round(mean(chocolate_test$final_grade), 2), "\n\n")

# ============================================================================
# PJESA 5: RUAJTJA E DATASET-EVE
# ============================================================================

cat("============================================================================\n")
cat("STEP 5: Saving all datasets\n")
cat("============================================================================\n")

output_dir <- getwd()

cat("  Saving to directory:", output_dir, "\n\n")

# Ruajmë dataset-et kryesore
cat("  1. Saving diabetes.csv...\n")
write.csv(diabetes, file.path(output_dir, "diabetes.csv"), row.names = FALSE)

cat("  2. Saving chocolate.csv...\n")
write.csv(chocolate, file.path(output_dir, "chocolate.csv"), row.names = FALSE)

# Ruajmë splits për referim
cat("  3. Saving diabetes_train.csv...\n")
write.csv(diabetes_train, file.path(output_dir, "diabetes_train.csv"), row.names = FALSE)

cat("  4. Saving diabetes_test.csv...\n")
write.csv(diabetes_test, file.path(output_dir, "diabetes_test.csv"), row.names = FALSE)

cat("  5. Saving chocolate_train.csv...\n")
write.csv(chocolate_train, file.path(output_dir, "chocolate_train.csv"), row.names = FALSE)

cat("  6. Saving chocolate_test.csv...\n")
write.csv(chocolate_test, file.path(output_dir, "chocolate_test.csv"), row.names = FALSE)

# Ruajmë edhe në RDS format
cat("  7. Saving diabetes.rds...\n")
saveRDS(diabetes, file.path(output_dir, "diabetes.rds"))

cat("  8. Saving chocolate.rds...\n")
saveRDS(chocolate, file.path(output_dir, "chocolate.rds"))

cat("\n  All datasets saved successfully!\n\n")

# ============================================================================
# PJESA 6: DATASET COMPARISON
# ============================================================================

cat("============================================================================\n")
cat("STEP 6: DATASET COMPARISON SUMMARY\n")
cat("============================================================================\n\n")

cat("DIABETES (Classification):\n")
cat("  Use case: Binary classification - predict diabetes\n")
cat("  Target variable: outcome (factor: yes/no)\n")
cat("  Observations:", nrow(diabetes), "\n")
cat("  Predictors: 7 numeric variables\n")
cat("  Class balance:", round(mean(diabetes$outcome == "yes") * 100, 1), "% yes,",
    round(mean(diabetes$outcome == "no") * 100, 1), "% no\n")
cat("  Appropriate for:\n")
cat("    - Classification trees\n")
cat("    - Confusion matrix\n")
cat("    - Accuracy, sensitivity, specificity\n")
cat("    - ROC curves and AUC\n\n")

cat("CHOCOLATE (Regression):\n")
cat("  Use case: Regression - predict chocolate quality rating\n")
cat("  Target variable: final_grade (numeric: 1-5)\n")
cat("  Observations:", nrow(chocolate), "\n")
cat("  Predictors: 5 variables (2 numeric, 3 categorical)\n")
cat("  Rating distribution: mean =", round(mean(chocolate$final_grade), 2),
    ", sd =", round(sd(chocolate$final_grade), 2), "\n")
cat("  Appropriate for:\n")
cat("    - Regression trees\n")
cat("    - MAE and RMSE metrics\n")
cat("    - Cross-validation\n")
cat("    - Hyperparameter tuning\n\n")

# ============================================================================
# PJESA 7: TIDYMODELS EXAMPLE CODE
# ============================================================================

cat("============================================================================\n")
cat("STEP 7: Example TIDYMODELS code snippets\n")
cat("============================================================================\n\n")

cat("Example 1: Classification Tree with Diabetes\n")
cat("---------------------------------------------\n")
cat("library(tidymodels)\n\n")
cat("# Load data\n")
cat("diabetes <- read_csv('diabetes.csv')\n\n")
cat("# Split data\n")
cat("set.seed(2023)\n")
cat("diabetes_split <- initial_split(diabetes, prop = 0.9, strata = outcome)\n")
cat("diabetes_train <- training(diabetes_split)\n")
cat("diabetes_test <- testing(diabetes_split)\n\n")
cat("# Create model specification\n")
cat("tree_spec <- decision_tree() %>%\n")
cat("  set_engine('rpart') %>%\n")
cat("  set_mode('classification')\n\n")
cat("# Fit model\n")
cat("tree_fit <- tree_spec %>%\n")
cat("  fit(outcome ~ ., data = diabetes_train)\n\n")
cat("# Make predictions\n")
cat("predictions <- predict(tree_fit, new_data = diabetes_test, type = 'class')\n\n")
cat("# Evaluate\n")
cat("pred_combined <- predictions %>%\n")
cat("  bind_cols(diabetes_test %>% select(outcome))\n\n")
cat("accuracy(pred_combined, truth = outcome, estimate = .pred_class)\n\n")

cat("Example 2: Regression Tree with Chocolate\n")
cat("------------------------------------------\n")
cat("library(tidymodels)\n\n")
cat("# Load data\n")
cat("chocolate <- read_csv('chocolate.csv')\n\n")
cat("# Split data\n")
cat("set.seed(2023)\n")
cat("chocolate_split <- initial_split(chocolate, prop = 0.8)\n")
cat("chocolate_train <- training(chocolate_split)\n")
cat("chocolate_test <- testing(chocolate_split)\n\n")
cat("# Create model specification\n")
cat("tree_spec <- decision_tree() %>%\n")
cat("  set_engine('rpart') %>%\n")
cat("  set_mode('regression')\n\n")
cat("# Fit model\n")
cat("tree_fit <- tree_spec %>%\n")
cat("  fit(final_grade ~ ., data = chocolate_train)\n\n")
cat("# Make predictions\n")
cat("predictions <- predict(tree_fit, new_data = chocolate_test)\n\n")
cat("# Evaluate\n")
cat("pred_combined <- predictions %>%\n")
cat("  bind_cols(chocolate_test %>% select(final_grade))\n\n")
cat("rmse(pred_combined, truth = final_grade, estimate = .pred)\n")
cat("mae(pred_combined, truth = final_grade, estimate = .pred)\n\n")

# ============================================================================
# PËRFUNDIMI
# ============================================================================

cat("============================================================================\n")
cat("  DATA GENERATION COMPLETED SUCCESSFULLY!\n")
cat("============================================================================\n\n")

cat("Generated files:\n")
cat("  1. diabetes.csv - Classification dataset (768 obs)\n")
cat("  2. chocolate.csv - Regression dataset (1,795 obs)\n")
cat("  3. diabetes_train.csv - Training split (90%)\n")
cat("  4. diabetes_test.csv - Test split (10%)\n")
cat("  5. chocolate_train.csv - Training split (80%)\n")
cat("  6. chocolate_test.csv - Test split (20%)\n")
cat("  7-8. RDS versions of main datasets\n\n")

cat("Key Concepts for Lecture 3:\n")
cat("  ✓ Classification vs Regression trees\n")
cat("  ✓ Binary outcomes (diabetes) vs continuous (chocolate)\n")
cat("  ✓ Train/test splitting with stratification\n")
cat("  ✓ tidymodels workflow\n")
cat("  ✓ Model evaluation metrics (accuracy, RMSE, MAE)\n")
cat("  ✓ Hyperparameter tuning\n")
cat("  ✓ Cross-validation\n\n")

cat("Ready for Lecture 3 exercises!\n\n")
cat("============================================================================\n\n")