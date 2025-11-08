# ============================================================================
# COMPREHENSIVE DATA GENERATION SCRIPT
# Predictive Analytics in R - Lecture 1
# Prof. Asc. Endri Raco, Ph.D.
# Departamenti i Inxhinierisë Matematike, UPT
# ============================================================================
# 
# Ky script gjeneron të gjitha datasetet e nevojshme për studentët:
# 1. basetable.csv - Dataset i plotë për trajnim dhe analizë
# 2. train_data.csv - Training set (60% e të dhënave)
# 3. test_data.csv - Test/validation set (40% e të dhënave)
# 4. current_campaign.csv - Dataset për scoring/predictions (pa Target)
# 5. basetable.rds - Versioni R i plotë i dataset-it
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("  DATA GENERATION FOR PREDICTIVE ANALYTICS LECTURE\n")
cat("  Polytechnic University of Tirana\n")
cat("============================================================================\n\n")

# Vendosim seed për riprodhueshmëri
set.seed(123)

# ============================================================================
# PJESA 1: PARAMETRAT DHE LIBRARITË
# ============================================================================

cat("Loading required libraries...\n")

# Kontrollojmë dhe instalojmë libraritë e nevojshme
required_packages <- c("tidyverse", "caret")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste0("Installing ", pkg, "...\n"))
    install.packages(pkg, repos = "http://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

library(tidyverse)
library(caret)

cat("Libraries loaded successfully.\n\n")

# Parametrat kryesorë
n_basetable <- 10000      # Madhësia e basetable
n_scoring <- 5000         # Madhësia e scoring dataset
train_proportion <- 0.6   # Proporcioni për training set

cat("Parameters:\n")
cat("  Basetable size:", n_basetable, "\n")
cat("  Scoring dataset size:", n_scoring, "\n")
cat("  Training proportion:", train_proportion, "\n\n")

# ============================================================================
# PJESA 2: GJENERIMI I BASETABLE (Dataset kryesor)
# ============================================================================

cat("============================================================================\n")
cat("STEP 1: Generating BASETABLE (main dataset)\n")
cat("============================================================================\n")

# ---------------------------------------------------------------------------
# 2.1 Variablat bazë demografike
# ---------------------------------------------------------------------------

cat("  Generating demographic variables...\n")

# Gjinia (kategorik binare)
gender <- sample(c("M", "F"), n_basetable, replace = TRUE, prob = c(0.48, 0.52))

# Mosha (numerik kontinuë, 18-95 vjeç)
age <- round(pmax(18, pmin(95, rnorm(n_basetable, mean = 55, sd = 18))))

# Statusi martesor
marital_status <- sample(c("Single", "Married", "Divorced", "Widowed"),
                         n_basetable, replace = TRUE,
                         prob = c(0.25, 0.50, 0.15, 0.10))

# ---------------------------------------------------------------------------
# 2.2 Variablat socio-ekonomike
# ---------------------------------------------------------------------------

cat("  Generating socio-economic variables...\n")

# Të ardhurat (kategorik ordinal)
income_category <- sample(c("Low", "Medium", "High", "Very High"), 
                          n_basetable, replace = TRUE, 
                          prob = c(0.25, 0.40, 0.25, 0.10))

# Balanca bankare (numerik kontinuë)
balance <- round(pmax(0, rnorm(n_basetable, mean = 1500, sd = 800)), 2)

# Niveli arsimor
education <- sample(c("High School", "Bachelor", "Master", "PhD"), 
                    n_basetable, replace = TRUE,
                    prob = c(0.30, 0.45, 0.20, 0.05))

# ---------------------------------------------------------------------------
# 2.3 Variablat e historisë së dhurimit
# ---------------------------------------------------------------------------

cat("  Generating donation history variables...\n")

# Numri i dhurimit të mëparshëm
previous_gifts <- rpois(n_basetable, lambda = 2.5)
previous_gifts <- pmin(previous_gifts, 15)

# Dhurata mesatare
mean_gift <- ifelse(previous_gifts > 0, 
                    round(rgamma(n_basetable, shape = 2, scale = 25), 2),
                    0)
mean_gift <- pmin(mean_gift, 500)

# Dhurata maksimale
max_gift <- ifelse(previous_gifts > 0,
                   round(mean_gift * runif(n_basetable, min = 1, max = 2.5), 2),
                   0)
max_gift <- pmin(max_gift, 1000)

# Dhurata minimale
min_gift <- ifelse(previous_gifts > 0,
                   round(mean_gift * runif(n_basetable, min = 0.3, max = 0.8), 2),
                   0)

# Mediana e dhurimit
median_gift <- ifelse(previous_gifts > 0,
                      round((mean_gift + max_gift) / 2, 2),
                      0)

# Kohë nga dhurata e parë (ditë)
days_since_first_gift <- ifelse(previous_gifts > 0,
                                sample(100:3000, n_basetable, replace = TRUE),
                                NA)

# Kohë nga dhurata e fundit (ditë) - recency
recency <- ifelse(previous_gifts > 0,
                  pmin(sample(1:1000, n_basetable, replace = TRUE), 
                       days_since_first_gift, na.rm = TRUE),
                  NA)

# Frekuenca e dhurimit (për ata që kanë dhënë)
frequency <- ifelse(previous_gifts > 0,
                    previous_gifts / pmax(1, days_since_first_gift / 365),
                    0)

# ---------------------------------------------------------------------------
# 2.4 Variablat e angazhimit
# ---------------------------------------------------------------------------

cat("  Generating engagement variables...\n")

# Email të hapur
emails_opened <- rpois(n_basetable, lambda = 8)
emails_opened <- pmin(emails_opened, 50)

# Vizita në website
website_visits <- rpois(n_basetable, lambda = 5)
website_visits <- pmin(website_visits, 100)

# Pjesëmarrje në evente
attended_event <- sample(c(0, 1), n_basetable, replace = TRUE, 
                         prob = c(0.85, 0.15))

# Abonuar në newsletter
newsletter_subscriber <- sample(c(0, 1), n_basetable, replace = TRUE, 
                                prob = c(0.40, 0.60))

# ---------------------------------------------------------------------------
# 2.5 Variablat gjeografike
# ---------------------------------------------------------------------------

cat("  Generating geographic variables...\n")

# Regjioni
region <- sample(c("Tiranë", "Durrës", "Vlorë", "Shkodër", "Elbasan", "Other"),
                 n_basetable, replace = TRUE,
                 prob = c(0.35, 0.15, 0.12, 0.10, 0.10, 0.18))

# Tip zone
area_type <- sample(c("Urban", "Suburban", "Rural"),
                    n_basetable, replace = TRUE,
                    prob = c(0.55, 0.30, 0.15))

# Distanca nga zyra (km)
distance_to_office <- round(abs(rnorm(n_basetable, mean = 25, sd = 20)), 1)
distance_to_office <- pmin(distance_to_office, 200)

# ---------------------------------------------------------------------------
# 2.6 Gjenerimi i TARGET variablit
# ---------------------------------------------------------------------------

cat("  Generating TARGET variable...\n")

# Krijojmë një model logjistik të fshehur
log_odds <- -3.5

# Efekte të ndryshme
log_odds <- log_odds + 0.025 * age
log_odds <- log_odds + 0.35 * previous_gifts
log_odds <- log_odds - 0.0015 * ifelse(is.na(recency), 365, recency)
log_odds <- log_odds + 0.25 * (gender == "F")
log_odds <- log_odds + 0.0008 * balance

# Efekti i të ardhurave
income_effect <- c("Low" = 0, "Medium" = 0.3, "High" = 0.6, "Very High" = 0.9)
log_odds <- log_odds + income_effect[income_category]

# Efekti i arsimimit
education_effect <- c("High School" = 0, "Bachelor" = 0.2, 
                      "Master" = 0.4, "PhD" = 0.6)
log_odds <- log_odds + education_effect[education]

# Efekti i angazhimit
log_odds <- log_odds + 0.02 * emails_opened
log_odds <- log_odds + 0.03 * website_visits
log_odds <- log_odds + 0.5 * attended_event
log_odds <- log_odds + 0.3 * newsletter_subscriber

# Efekti i max_gift (për ata që kanë dhënë më parë)
log_odds <- log_odds + 0.008 * max_gift

# Konvertojmë në probabilitet dhe gjenerojmë Target
prob_donate <- 1 / (1 + exp(-log_odds))
Target <- rbinom(n_basetable, 1, prob_donate)

# ---------------------------------------------------------------------------
# 2.7 Krijimi i dataframe për basetable
# ---------------------------------------------------------------------------

cat("  Creating basetable dataframe...\n")

basetable <- data.frame(
  # ID
  donor_id = 1:n_basetable,
  
  # Target variable
  donated = Target,
  
  # Variablat demografike
  age = age,
  gender = gender,
  marital_status = marital_status,
  
  # Variablat socio-ekonomike
  balance = balance,
  income_category = income_category,
  education = education,
  
  # Variablat e historisë së dhurimit
  previous_gifts = previous_gifts,
  mean_gift = mean_gift,
  max_gift = max_gift,
  min_gift = min_gift,
  median_gift = median_gift,
  days_since_first_gift = days_since_first_gift,
  recency = recency,
  frequency = frequency,
  
  # Variablat e angazhimit
  emails_opened = emails_opened,
  website_visits = website_visits,
  attended_event = attended_event,
  newsletter_subscriber = newsletter_subscriber,
  
  # Variablat gjeografike
  region = region,
  area_type = area_type,
  distance_to_office = distance_to_office,
  
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# 2.8 Krijimi i dummy variables
# ---------------------------------------------------------------------------

cat("  Creating dummy variables...\n")

# Gender dummy
basetable$gender_F <- ifelse(basetable$gender == "F", 1, 0)
basetable$gender_M <- ifelse(basetable$gender == "M", 1, 0)

# Income dummies (baseline: Low)
basetable$income_low <- ifelse(basetable$income_category == "Low", 1, 0)
basetable$income_medium <- ifelse(basetable$income_category == "Medium", 1, 0)
basetable$income_high <- ifelse(basetable$income_category == "High", 1, 0)
basetable$income_veryhigh <- ifelse(basetable$income_category == "Very High", 1, 0)

# ---------------------------------------------------------------------------
# 2.9 Variablat e diskretizuara
# ---------------------------------------------------------------------------

cat("  Creating discretized variables...\n")

# Funksion për diskretizim
discretize_variable <- function(x, n_bins = 5, var_name = "var") {
  if (all(is.na(x))) {
    return(factor(rep(NA, length(x))))
  }
  
  # Heqim NA për të llogaritur quantiles
  non_na_x <- x[!is.na(x)]
  
  if (length(unique(non_na_x)) < n_bins) {
    # Nëse ka më pak vlera unike se bins, përdorim ato
    breaks <- sort(unique(non_na_x))
    if (min(breaks) > min(non_na_x, na.rm = TRUE)) {
      breaks <- c(min(non_na_x, na.rm = TRUE), breaks)
    }
    if (max(breaks) < max(non_na_x, na.rm = TRUE)) {
      breaks <- c(breaks, max(non_na_x, na.rm = TRUE))
    }
  } else {
    breaks <- quantile(non_na_x, probs = seq(0, 1, 1/n_bins), na.rm = TRUE)
    breaks <- unique(breaks)
  }
  
  result <- cut(x, 
                breaks = breaks,
                include.lowest = TRUE,
                labels = paste0(var_name, "_", 1:(length(breaks)-1)))
  
  return(result)
}

# Aplikojmë diskretizimin
basetable$age_disc <- discretize_variable(basetable$age, 5, "age")
basetable$balance_disc <- discretize_variable(basetable$balance, 5, "balance")
basetable$mean_gift_disc <- discretize_variable(basetable$mean_gift[basetable$mean_gift > 0], 5, "mean_gift")[match(basetable$mean_gift, basetable$mean_gift[basetable$mean_gift > 0])]
basetable$max_gift_disc <- discretize_variable(basetable$max_gift[basetable$max_gift > 0], 5, "max_gift")[match(basetable$max_gift, basetable$max_gift[basetable$max_gift > 0])]
basetable$recency_disc <- discretize_variable(basetable$recency, 5, "recency")

cat("  Basetable created successfully!\n\n")

# ============================================================================
# PJESA 3: TRAIN/TEST SPLIT
# ============================================================================

cat("============================================================================\n")
cat("STEP 2: Creating TRAIN and TEST datasets (60/40 split)\n")
cat("============================================================================\n")

# Krijoni train/test split duke përdorur caret
set.seed(123)
train_index <- createDataPartition(
  basetable$donated,
  p = train_proportion,
  list = FALSE
)

train_data <- basetable[train_index, ]
test_data <- basetable[-train_index, ]

cat("  Training set size:", nrow(train_data), "observations\n")
cat("  Test set size:", nrow(test_data), "observations\n")

# Kontrollojmë balancën e Target
train_rate <- mean(train_data$donated)
test_rate <- mean(test_data$donated)

cat("  Training donation rate:", round(train_rate * 100, 2), "%\n")
cat("  Test donation rate:", round(test_rate * 100, 2), "%\n\n")

# ============================================================================
# PJESA 4: SCORING DATASET (Current Campaign)
# ============================================================================

cat("============================================================================\n")
cat("STEP 3: Creating SCORING dataset (current campaign - no Target)\n")
cat("============================================================================\n")

cat("  Generating new donor pool for scoring...\n")

# Përdorim një seed të ndryshëm për të krijuar donatorë të rinj
set.seed(456)

# Gjenerojmë variablat me të njëjtat distribucione
scoring_gender <- sample(c("M", "F"), n_scoring, replace = TRUE, prob = c(0.48, 0.52))
scoring_age <- round(pmax(18, pmin(95, rnorm(n_scoring, mean = 55, sd = 18))))
scoring_balance <- round(pmax(0, rnorm(n_scoring, mean = 1500, sd = 800)), 2)

scoring_income <- sample(c("Low", "Medium", "High", "Very High"), 
                         n_scoring, replace = TRUE, 
                         prob = c(0.25, 0.40, 0.25, 0.10))

scoring_education <- sample(c("High School", "Bachelor", "Master", "PhD"), 
                            n_scoring, replace = TRUE,
                            prob = c(0.30, 0.45, 0.20, 0.05))

scoring_previous <- rpois(n_scoring, lambda = 2.5)
scoring_previous <- pmin(scoring_previous, 15)

scoring_mean_gift <- ifelse(scoring_previous > 0, 
                            round(rgamma(n_scoring, shape = 2, scale = 25), 2),
                            0)
scoring_mean_gift <- pmin(scoring_mean_gift, 500)

scoring_max_gift <- ifelse(scoring_previous > 0,
                           round(scoring_mean_gift * runif(n_scoring, min = 1, max = 2.5), 2),
                           0)
scoring_max_gift <- pmin(scoring_max_gift, 1000)

scoring_min_gift <- ifelse(scoring_previous > 0,
                           round(scoring_mean_gift * runif(n_scoring, min = 0.3, max = 0.8), 2),
                           0)

scoring_recency <- ifelse(scoring_previous > 0,
                          sample(1:1000, n_scoring, replace = TRUE),
                          NA)

scoring_emails <- rpois(n_scoring, lambda = 8)
scoring_emails <- pmin(scoring_emails, 50)

scoring_website <- rpois(n_scoring, lambda = 5)
scoring_website <- pmin(scoring_website, 100)

scoring_event <- sample(c(0, 1), n_scoring, replace = TRUE, prob = c(0.85, 0.15))
scoring_newsletter <- sample(c(0, 1), n_scoring, replace = TRUE, prob = c(0.40, 0.60))

scoring_region <- sample(c("Tiranë", "Durrës", "Vlorë", "Shkodër", "Elbasan", "Other"),
                         n_scoring, replace = TRUE,
                         prob = c(0.35, 0.15, 0.12, 0.10, 0.10, 0.18))

scoring_area <- sample(c("Urban", "Suburban", "Rural"),
                       n_scoring, replace = TRUE,
                       prob = c(0.55, 0.30, 0.15))

# Krijojmë scoring dataset PA Target variable
current_campaign <- data.frame(
  donor_id = (n_basetable + 1):(n_basetable + n_scoring),
  age = scoring_age,
  gender = scoring_gender,
  balance = scoring_balance,
  income_category = scoring_income,
  education = scoring_education,
  previous_gifts = scoring_previous,
  mean_gift = scoring_mean_gift,
  max_gift = scoring_max_gift,
  min_gift = scoring_min_gift,
  recency = scoring_recency,
  emails_opened = scoring_emails,
  website_visits = scoring_website,
  attended_event = scoring_event,
  newsletter_subscriber = scoring_newsletter,
  region = scoring_region,
  area_type = scoring_area,
  stringsAsFactors = FALSE
)

# Shtojmë dummy variables
current_campaign$gender_F <- ifelse(current_campaign$gender == "F", 1, 0)
current_campaign$income_low <- ifelse(current_campaign$income_category == "Low", 1, 0)
current_campaign$income_medium <- ifelse(current_campaign$income_category == "Medium", 1, 0)
current_campaign$income_high <- ifelse(current_campaign$income_category == "High", 1, 0)

cat("  Scoring dataset created successfully!\n")
cat("  Size:", nrow(current_campaign), "observations\n")
cat("  NOTE: This dataset has NO TARGET variable (for prediction purposes)\n\n")

# ============================================================================
# PJESA 5: STATISTIKAT PËRSHKRUESE
# ============================================================================

cat("============================================================================\n")
cat("STEP 4: Generating SUMMARY STATISTICS\n")
cat("============================================================================\n\n")

cat("BASETABLE SUMMARY:\n")
cat("------------------\n")
cat("Total observations:", nrow(basetable), "\n")
cat("Number of donors (donated=1):", sum(basetable$donated), "\n")
cat("Overall response rate:", round(mean(basetable$donated) * 100, 2), "%\n")
cat("Total variables:", ncol(basetable), "\n\n")

cat("Variable types:\n")
cat("  Numeric:", sum(sapply(basetable, is.numeric)), "\n")
cat("  Character/Factor:", sum(sapply(basetable, function(x) is.factor(x) || is.character(x))), "\n\n")

cat("KEY DESCRIPTIVE STATISTICS:\n")
cat("---------------------------\n")
cat("Age: mean =", round(mean(basetable$age), 1), 
    ", range = [", min(basetable$age), ",", max(basetable$age), "]\n")
cat("Balance: mean =", round(mean(basetable$balance), 2), 
    ", median =", round(median(basetable$balance), 2), "\n")
cat("Previous gifts: mean =", round(mean(basetable$previous_gifts), 1), "\n")
cat("  Donors with 0 previous gifts:", sum(basetable$previous_gifts == 0), 
    "(", round(mean(basetable$previous_gifts == 0) * 100, 1), "%)\n\n")

cat("Gender distribution:\n")
print(table(basetable$gender))
cat("\n")

cat("Income distribution:\n")
print(table(basetable$income_category))
cat("\n")

# ============================================================================
# PJESA 6: RUAJTJA E DATASET-EVE
# ============================================================================

cat("============================================================================\n")
cat("STEP 5: Saving all datasets to files\n")
cat("============================================================================\n")

# Ruajmë në të njëjtën direktori ku ndodhet skripta
output_dir <- getwd()  # Direktoria aktuale e punës

cat("  Saving to directory:", output_dir, "\n\n")

# Ruajmë dataset-et
cat("  1. Saving basetable.csv...\n")
write.csv(basetable, file.path(output_dir, "basetable.csv"), row.names = FALSE)

cat("  2. Saving train_data.csv...\n")
write.csv(train_data, file.path(output_dir, "train_data.csv"), row.names = FALSE)

cat("  3. Saving test_data.csv...\n")
write.csv(test_data, file.path(output_dir, "test_data.csv"), row.names = FALSE)

cat("  4. Saving current_campaign.csv...\n")
write.csv(current_campaign, file.path(output_dir, "current_campaign.csv"), row.names = FALSE)

cat("  5. Saving basetable.rds (R format)...\n")
saveRDS(basetable, file.path(output_dir, "basetable.rds"))

cat("  6. Saving train_data.rds (R format)...\n")
saveRDS(train_data, file.path(output_dir, "train_data.rds"))

cat("  7. Saving test_data.rds (R format)...\n")
saveRDS(test_data, file.path(output_dir, "test_data.rds"))

cat("  8. Saving current_campaign.rds (R format)...\n")
saveRDS(current_campaign, file.path(output_dir, "current_campaign.rds"))

cat("\n  All datasets saved successfully!\n\n")

# ============================================================================
# PJESA 7: KRIJIMI I DOKUMENTACIONIT
# ============================================================================

cat("============================================================================\n")
cat("STEP 6: Creating README documentation\n")
cat("============================================================================\n")

readme_content <- paste0(
  "# PREDICTIVE ANALYTICS DATASETS
## Generated on: ", Sys.Date(), "
## Polytechnic University of Tirana - Prof. Asc. Endri Raco, Ph.D.

## AVAILABLE DATASETS

### 1. basetable.csv / basetable.rds
- **Description**: Main dataset with all donors and their characteristics
- **Size**: ", nrow(basetable), " observations, ", ncol(basetable), " variables
- **Target variable**: 'donated' (0 = did not donate, 1 = donated)
- **Response rate**: ", round(mean(basetable$donated) * 100, 2), "%
- **Usage**: Complete dataset for exploration and analysis

### 2. train_data.csv / train_data.rds
- **Description**: Training subset (", round(train_proportion * 100), "% of basetable)
- **Size**: ", nrow(train_data), " observations
- **Response rate**: ", round(mean(train_data$donated) * 100, 2), "%
- **Usage**: Use this for model building and variable selection

### 3. test_data.csv / test_data.rds
- **Description**: Test/validation subset (", round((1-train_proportion) * 100), "% of basetable)
- **Size**: ", nrow(test_data), " observations
- **Response rate**: ", round(mean(test_data$donated) * 100, 2), "%
- **Usage**: Use this for model evaluation (never for training!)

### 4. current_campaign.csv / current_campaign.rds
- **Description**: New donor pool for scoring/predictions
- **Size**: ", nrow(current_campaign), " observations
- **Target variable**: NOT INCLUDED (this is for prediction)
- **Usage**: Make predictions using your trained model

## VARIABLE DESCRIPTIONS

### Target Variable
- **donated**: Binary (0/1) - Whether donor contributed in the campaign

### Demographic Variables
- **age**: Numeric (18-95) - Age in years
- **gender**: Character (M/F) - Gender
- **marital_status**: Character - Single/Married/Divorced/Widowed

### Socio-Economic Variables
- **balance**: Numeric - Bank account balance (in euros)
- **income_category**: Character - Low/Medium/High/Very High
- **education**: Character - High School/Bachelor/Master/PhD

### Donation History Variables
- **previous_gifts**: Numeric (0-15) - Number of previous donations
- **mean_gift**: Numeric - Average donation amount (euros)
- **max_gift**: Numeric - Maximum donation amount (euros)
- **min_gift**: Numeric - Minimum donation amount (euros)
- **median_gift**: Numeric - Median donation amount (euros)
- **days_since_first_gift**: Numeric - Days since first donation
- **recency**: Numeric - Days since most recent donation
- **frequency**: Numeric - Donation frequency (gifts per year)

### Engagement Variables
- **emails_opened**: Numeric (0-50) - Number of marketing emails opened
- **website_visits**: Numeric (0-100) - Number of website visits
- **attended_event**: Binary (0/1) - Whether attended fundraising event
- **newsletter_subscriber**: Binary (0/1) - Newsletter subscription status

### Geographic Variables
- **region**: Character - Tiranë/Durrës/Vlorë/Shkodër/Elbasan/Other
- **area_type**: Character - Urban/Suburban/Rural
- **distance_to_office**: Numeric - Distance to main office (km)

### Dummy Variables (for regression)
- **gender_F**, **gender_M**: Binary indicators for gender
- **income_low**, **income_medium**, **income_high**, **income_veryhigh**: Income indicators

### Discretized Variables (for PIG analysis)
- **age_disc**, **balance_disc**, **mean_gift_disc**, **max_gift_disc**, **recency_disc**

## QUICK START GUIDE

### Loading the data in R:
```r
# Option 1: Load from CSV
basetable <- read.csv('basetable.csv')

# Option 2: Load from RDS (preserves R structure)
basetable <- readRDS('basetable.rds')

# Load training and test sets
train_data <- read.csv('train_data.csv')
test_data <- read.csv('test_data.csv')

# Load scoring dataset
current_campaign <- read.csv('current_campaign.csv')
```

### Example: Build a simple logistic regression model
```r
library(pROC)

# Train model on training data
model <- glm(donated ~ age + balance + previous_gifts, 
             data = train_data, 
             family = binomial)

# Evaluate on test data
test_predictions <- predict(model, newdata = test_data, type = 'response')
test_auc <- auc(roc(test_data$donated, test_predictions))
cat('Test AUC:', round(test_auc, 3))

# Make predictions on current campaign
campaign_predictions <- predict(model, 
                               newdata = current_campaign, 
                               type = 'response')
```

## IMPORTANT NOTES

1. **Never use test_data for training or variable selection**
2. **current_campaign has no Target variable - it's for making predictions**
3. **All datasets use the same variable structure (except current_campaign has no Target)**
4. **Missing values (NA) appear only in donation history variables for non-donors**
5. **Random seed = 123 for reproducibility**

## CONTACT
Prof. Asc. Endri Raco, Ph.D.
Department of Mathematical Engineering
Polytechnic University of Tirana
endri.raco@upt.al
")

writeLines(readme_content, file.path(output_dir, "README.txt"))
cat("  README.txt created successfully!\n\n")

# ============================================================================
# PJESA 8: PËRFUNDIMI
# ============================================================================

cat("============================================================================\n")
cat("  DATA GENERATION COMPLETED SUCCESSFULLY!\n")
cat("============================================================================\n\n")

cat("Generated files:\n")
cat("  1. basetable.csv (", format(file.size(file.path(output_dir, "basetable.csv")), big.mark=","), " bytes)\n")
cat("  2. train_data.csv\n")
cat("  3. test_data.csv\n")
cat("  4. current_campaign.csv\n")
cat("  5. basetable.rds\n")
cat("  6. train_data.rds\n")
cat("  7. test_data.rds\n")
cat("  8. current_campaign.rds\n")
cat("  9. README.txt\n\n")

cat("All datasets are ready for use in the Predictive Analytics lecture!\n")
cat("Students can now begin working with the data.\n\n")

cat("============================================================================\n\n")