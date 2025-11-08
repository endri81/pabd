# PREDICTIVE ANALYTICS DATASETS
## Generated on: 2025-11-08
## Polytechnic University of Tirana - Prof. Asc. Endri Raco, Ph.D.

## AVAILABLE DATASETS

### 1. basetable.csv / basetable.rds
- **Description**: Main dataset with all donors and their characteristics
- **Size**: 10000 observations, 34 variables
- **Target variable**: 'donated' (0 = did not donate, 1 = donated)
- **Response rate**: 71.22%
- **Usage**: Complete dataset for exploration and analysis

### 2. train_data.csv / train_data.rds
- **Description**: Training subset (60% of basetable)
- **Size**: 6000 observations
- **Response rate**: 71.18%
- **Usage**: Use this for model building and variable selection

### 3. test_data.csv / test_data.rds
- **Description**: Test/validation subset (40% of basetable)
- **Size**: 4000 observations
- **Response rate**: 71.28%
- **Usage**: Use this for model evaluation (never for training!)

### 4. current_campaign.csv / current_campaign.rds
- **Description**: New donor pool for scoring/predictions
- **Size**: 5000 observations
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

