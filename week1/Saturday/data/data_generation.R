# ============================================================================
# DATA GENERATION SCRIPT FOR LECTURE 2
# The Basetable Timeline - Intermediate Predictive Analytics
# Prof. Asc. Endri Raco, Ph.D.
# Departamenti i Inxhinierisë Matematike, UPT
# ============================================================================
#
# Ky script gjeneron dataset-et e nevojshme për Lecture 2:
# 1. gifts.csv - Transaksione dhurimi me timeline (ID, date, amount)
# 2. donors.csv - Informacion bazë për donatorët (demographics)
# 3. basetable_2017.csv - Basetable për observation date May 2017
# 4. basetable_2018.csv - Basetable për observation date May 2018
# 5. feature_catalog.csv - Katalog i features të krijuara
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("  LECTURE 2: THE BASETABLE TIMELINE\n")
cat("  Timeline-Compliant Data Generation\n")
cat("============================================================================\n\n")

# Vendosim seed për riprodhueshmëri
set.seed(789)

# ============================================================================
# PJESA 1: LIBRARITE DHE PARAMETRAT
# ============================================================================

cat("STEP 1: Loading required libraries...\n")

required_packages <- c("tidyverse", "lubridate")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste0("  Installing ", pkg, "...\n"))
    install.packages(pkg, repos = "http://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

library(tidyverse)
library(lubridate)

cat("  Libraries loaded successfully!\n\n")

# Parametrat
n_donors <- 5000              # Numri i donatorëve
start_date <- as.Date("2012-01-01")  # Data e fillimit të historisë
end_date <- as.Date("2018-12-31")    # Data e fundit
observation_dates <- c(
  as.Date("2017-05-01"),
  as.Date("2018-05-01")
)

cat("Parameters:\n")
cat("  Number of donors:", n_donors, "\n")
cat("  Historical period:", start_date, "to", end_date, "\n")
cat("  Observation dates:", paste(observation_dates, collapse = ", "), "\n\n")

# ============================================================================
# PJESA 2: GJENERIMI I DONOR DEMOGRAPHICS
# ============================================================================

cat("============================================================================\n")
cat("STEP 2: Generating DONOR DEMOGRAPHICS\n")
cat("============================================================================\n")

cat("  Creating donor profiles...\n")

donors <- tibble(
  donor_id = 1:n_donors,
  
  # Demographics
  age = round(pmax(18, pmin(95, rnorm(n_donors, mean = 58, sd = 16)))),
  gender = sample(c("M", "F"), n_donors, replace = TRUE, prob = c(0.45, 0.55)),
  
  # Geographic
  country = sample(c("USA", "UK", "Canada", "Germany", "France", "Other"),
                   n_donors, replace = TRUE,
                   prob = c(0.40, 0.20, 0.15, 0.10, 0.08, 0.07)),
  
  region = sample(c("North", "South", "East", "West", "Central"),
                  n_donors, replace = TRUE,
                  prob = c(0.25, 0.20, 0.22, 0.18, 0.15)),
  
  # Donor segment (assigned based on latent quality)
  donor_quality = rnorm(n_donors, mean = 50, sd = 20)
)

# Assign segment based on quality
donors <- donors %>%
  mutate(
    segment = case_when(
      donor_quality >= 65 ~ "Gold",
      donor_quality >= 45 ~ "Silver",
      TRUE ~ "Bronze"
    )
  ) %>%
  select(-donor_quality)  # Remove temporary variable

# Calculate birth year for timeline compliance
donors <- donors %>%
  mutate(birth_year = 2018 - age)

cat("  Donor demographics created!\n")
cat("    Total donors:", nrow(donors), "\n")
cat("    Gender distribution:\n")
print(table(donors$gender))
cat("    Segment distribution:\n")
print(table(donors$segment))
cat("\n")

# ============================================================================
# PJESA 3: GJENERIMI I DONATION HISTORY (GIFTS)
# ============================================================================

cat("============================================================================\n")
cat("STEP 3: Generating DONATION HISTORY (gifts.csv)\n")
cat("============================================================================\n")

cat("  Simulating donation transactions over time...\n")

# Funksion për të gjeneruar donations për një donator
generate_donor_gifts <- function(donor_id, start_date, end_date, donor_segment) {
  # Parametrat bazuar në segment
  params <- list(
    Gold = list(
      lambda_frequency = 3.5,      # ~3-4 donations per year
      mean_amount = 120,
      sd_amount = 50,
      churn_prob = 0.05            # 5% chance to stop donating
    ),
    Silver = list(
      lambda_frequency = 2.0,      # ~2 donations per year
      mean_amount = 75,
      sd_amount = 35,
      churn_prob = 0.15
    ),
    Bronze = list(
      lambda_frequency = 1.0,      # ~1 donation per year
      mean_amount = 45,
      sd_amount = 25,
      churn_prob = 0.30
    )
  )
  
  segment_params <- params[[donor_segment]]
  
  # Gjenerojmë numrin e donations
  n_years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
  expected_donations <- rpois(1, lambda = segment_params$lambda_frequency * n_years)
  
  if (expected_donations == 0) {
    return(NULL)
  }
  
  # Gjenerojmë datat e donations (uniform random across period)
  donation_dates <- start_date + sample(0:as.numeric(difftime(end_date, start_date, units = "days")),
                                        expected_donations, replace = FALSE)
  donation_dates <- sort(donation_dates)
  
  # Simulojmë churn - nëse donatori ndalon së dhëni
  active <- TRUE
  final_dates <- c()
  
  for (date in donation_dates) {
    if (active) {
      final_dates <- c(final_dates, date)
      # Check if donor churns after this donation
      if (runif(1) < segment_params$churn_prob) {
        active <- FALSE
      }
    }
  }
  
  if (length(final_dates) == 0) {
    return(NULL)
  }
  
  # Gjenerojmë amounts
  amounts <- round(pmax(10, rnorm(length(final_dates),
                                  mean = segment_params$mean_amount,
                                  sd = segment_params$sd_amount)), 2)
  
  # Shtojmë seasonal effect (më shumë në fund të vitit)
  # Konvertojmë në Date nëse nuk janë
  date_objects <- as.Date(final_dates, origin = "1970-01-01")
  seasonal_multiplier <- 1 + 0.3 * (month(date_objects) %in% c(11, 12))
  amounts <- round(amounts * seasonal_multiplier, 2)
  
  return(tibble(
    id = donor_id,
    date = as.Date(final_dates, origin = "1970-01-01"),
    amount = amounts
  ))
}

# Gjenerojmë gifts për të gjithë donatorët
cat("  Generating gifts for", n_donors, "donors (this may take a minute)...\n")

gifts_list <- map2(
  donors$donor_id,
  donors$segment,
  ~generate_donor_gifts(.x, start_date, end_date, .y)
)

# Kombinojmë në një dataset
gifts <- bind_rows(gifts_list) %>%
  arrange(id, date)

# Heqim donors që nuk kanë asnjë donation
donors <- donors %>%
  filter(donor_id %in% unique(gifts$id))

cat("  Donation history generated!\n")
cat("    Total donations:", nrow(gifts), "\n")
cat("    Active donors:", length(unique(gifts$id)), "\n")
cat("    Date range:", min(gifts$date), "to", max(gifts$date), "\n")
cat("    Average donations per donor:", round(nrow(gifts) / length(unique(gifts$id)), 1), "\n")
cat("    Total amount donated: €", format(sum(gifts$amount), big.mark = ","), "\n\n")

# ============================================================================
# PJESA 4: FEATURE ENGINEERING FUNCTIONS
# ============================================================================

cat("============================================================================\n")
cat("STEP 4: Creating FEATURE ENGINEERING functions\n")
cat("============================================================================\n")

# Funksion për të llogaritur RFM features
calculate_rfm_features <- function(gifts_data, donor_ids, observation_date, window_months = 12) {
  
  window_start <- observation_date %m-% months(window_months)
  
  # Filter gifts në window
  gifts_window <- gifts_data %>%
    filter(date >= window_start & date < observation_date)
  
  # Llogarit RFM per secilin donor
  rfm <- gifts_window %>%
    group_by(id) %>%
    summarise(
      recency_days = as.numeric(observation_date - max(date)),
      frequency = n(),
      monetary_total = sum(amount),
      monetary_mean = mean(amount),
      monetary_max = max(amount),
      monetary_min = min(amount),
      .groups = "drop"
    )
  
  # Merge me donor_ids dhe fill missing me 0/NA
  result <- tibble(id = donor_ids) %>%
    left_join(rfm, by = "id") %>%
    mutate(
      recency_days = ifelse(is.na(recency_days), as.numeric(observation_date - window_start), recency_days),
      frequency = replace_na(frequency, 0),
      monetary_total = replace_na(monetary_total, 0),
      monetary_mean = replace_na(monetary_mean, 0),
      monetary_max = replace_na(monetary_max, 0),
      monetary_min = replace_na(monetary_min, 0)
    )
  
  # Rename columns to include window info
  suffix <- paste0("_", window_months, "m")
  result <- result %>%
    rename_with(~paste0(., suffix), -id)
  
  return(result)
}

# Funksion për trend features
calculate_trend_features <- function(gifts_data, donor_ids, observation_date, 
                                     short_window = 3, long_window = 12) {
  
  short_start <- observation_date %m-% months(short_window)
  long_start <- observation_date %m-% months(long_window)
  
  # Short window
  short_gifts <- gifts_data %>%
    filter(date >= short_start & date < observation_date) %>%
    group_by(id) %>%
    summarise(
      amount_short = sum(amount),
      count_short = n(),
      .groups = "drop"
    )
  
  # Long window
  long_gifts <- gifts_data %>%
    filter(date >= long_start & date < observation_date) %>%
    group_by(id) %>%
    summarise(
      amount_long = sum(amount),
      count_long = n(),
      .groups = "drop"
    )
  
  # Merge dhe calculate trends
  trends <- tibble(id = donor_ids) %>%
    left_join(short_gifts, by = "id") %>%
    left_join(long_gifts, by = "id") %>%
    mutate(
      amount_short = replace_na(amount_short, 0),
      count_short = replace_na(count_short, 0),
      amount_long = replace_na(amount_long, 0),
      count_long = replace_na(count_long, 0)
    ) %>%
    mutate(
      # Trend: short vs long (normalized per month)
      amount_trend = (amount_short / short_window) / (amount_long / long_window + 1),
      count_trend = (count_short / short_window) / (count_long / long_window + 1),
      
      # Growth rate
      amount_growth = (amount_short - amount_long/4) / (amount_long/4 + 1)
    ) %>%
    select(id, amount_trend, count_trend, amount_growth)
  
  return(trends)
}

# Funksion për lifetime features
calculate_lifetime_features <- function(gifts_data, donor_ids, observation_date) {
  
  lifetime <- gifts_data %>%
    filter(date < observation_date) %>%
    group_by(id) %>%
    summarise(
      first_gift_date = min(date),
      last_gift_date = max(date),
      lifetime_donations = n(),
      lifetime_amount = sum(amount),
      .groups = "drop"
    )
  
  result <- tibble(id = donor_ids) %>%
    left_join(lifetime, by = "id") %>%
    mutate(
      days_since_first = as.numeric(observation_date - first_gift_date),
      days_since_last = as.numeric(observation_date - last_gift_date),
      donor_age_days = as.numeric(observation_date - first_gift_date),
      lifetime_donations = replace_na(lifetime_donations, 0),
      lifetime_amount = replace_na(lifetime_amount, 0),
      avg_days_between = ifelse(lifetime_donations > 1,
                                days_since_first / (lifetime_donations - 1),
                                NA),
      is_new_donor = ifelse(days_since_first <= 365, 1, 0)
    ) %>%
    select(-first_gift_date, -last_gift_date)
  
  return(result)
}

cat("  Feature engineering functions created!\n\n")

# ============================================================================
# PJESA 5: KONSTRUKTIMI I BASETABLE PËR 2017
# ============================================================================

cat("============================================================================\n")
cat("STEP 5: Constructing BASETABLE for May 2017\n")
cat("============================================================================\n")

observation_date_2017 <- as.Date("2017-05-01")
target_end_2017 <- as.Date("2017-08-01")

cat("  Observation date:", as.character(observation_date_2017), "\n")
cat("  Target period:", as.character(observation_date_2017), "to", 
    as.character(target_end_2017), "\n")

# Përcaktoni population - donors që kanë dhënë në 2016
donations_2016 <- gifts %>%
  filter(year(date) == 2016)

population_2017 <- unique(donations_2016$id)

cat("  Population size:", length(population_2017), "donors\n")

# Exclude donors që dhanë Jan-Apr 2017
donations_early_2017 <- gifts %>%
  filter(year(date) == 2017, month(date) < 5)

donors_exclude_2017 <- unique(donations_early_2017$id)

population_2017 <- setdiff(population_2017, donors_exclude_2017)

cat("  Final population (after exclusions):", length(population_2017), "donors\n")

# Krijoni basetable
basetable_2017 <- tibble(donor_id = population_2017)

# Merge demographics
basetable_2017 <- basetable_2017 %>%
  left_join(donors %>% select(donor_id, age, gender, country, region, segment, birth_year),
            by = "donor_id") %>%
  mutate(
    age_at_observation = year(observation_date_2017) - birth_year
  )

# Calculate features
cat("  Calculating features...\n")

# RFM features (multiple windows)
rfm_12m <- calculate_rfm_features(gifts, population_2017, observation_date_2017, 12)
rfm_24m <- calculate_rfm_features(gifts, population_2017, observation_date_2017, 24)

# Trend features
trends <- calculate_trend_features(gifts, population_2017, observation_date_2017, 3, 12)

# Lifetime features
lifetime <- calculate_lifetime_features(gifts, population_2017, observation_date_2017)

# Merge features
basetable_2017 <- basetable_2017 %>%
  left_join(rfm_12m, by = c("donor_id" = "id")) %>%
  left_join(rfm_24m, by = c("donor_id" = "id")) %>%
  left_join(trends, by = c("donor_id" = "id")) %>%
  left_join(lifetime, by = c("donor_id" = "id"))

# Define target
target_gifts_2017 <- gifts %>%
  filter(date >= observation_date_2017 & date < target_end_2017)

donors_donated_2017 <- unique(target_gifts_2017$id)

basetable_2017 <- basetable_2017 %>%
  mutate(
    target_donated = ifelse(donor_id %in% donors_donated_2017, 1, 0)
  )

# Calculate continuous target (total amount)
target_amounts_2017 <- target_gifts_2017 %>%
  group_by(id) %>%
  summarise(target_amount = sum(amount), .groups = "drop")

basetable_2017 <- basetable_2017 %>%
  left_join(target_amounts_2017, by = c("donor_id" = "id")) %>%
  mutate(target_amount = replace_na(target_amount, 0))

cat("  Basetable 2017 created!\n")
cat("    Rows:", nrow(basetable_2017), "\n")
cat("    Columns:", ncol(basetable_2017), "\n")
cat("    Target response rate:", round(mean(basetable_2017$target_donated) * 100, 2), "%\n\n")

# ============================================================================
# PJESA 6: KONSTRUKTIMI I BASETABLE PËR 2018
# ============================================================================

cat("============================================================================\n")
cat("STEP 6: Constructing BASETABLE for May 2018\n")
cat("============================================================================\n")

observation_date_2018 <- as.Date("2018-05-01")
target_end_2018 <- as.Date("2018-08-01")

cat("  Observation date:", as.character(observation_date_2018), "\n")
cat("  Target period:", as.character(observation_date_2018), "to", 
    as.character(target_end_2018), "\n")

# Përcaktoni population - donors që kanë dhënë në 2017
donations_2017 <- gifts %>%
  filter(year(date) == 2017)

population_2018 <- unique(donations_2017$id)

cat("  Population size:", length(population_2018), "donors\n")

# Exclude donors që dhanë Jan-Apr 2018
donations_early_2018 <- gifts %>%
  filter(year(date) == 2018, month(date) < 5)

donors_exclude_2018 <- unique(donations_early_2018$id)

population_2018 <- setdiff(population_2018, donors_exclude_2018)

cat("  Final population (after exclusions):", length(population_2018), "donors\n")

# Krijoni basetable
basetable_2018 <- tibble(donor_id = population_2018)

# Merge demographics
basetable_2018 <- basetable_2018 %>%
  left_join(donors %>% select(donor_id, age, gender, country, region, segment, birth_year),
            by = "donor_id") %>%
  mutate(
    age_at_observation = year(observation_date_2018) - birth_year
  )

# Calculate features
cat("  Calculating features...\n")

# RFM features
rfm_12m_2018 <- calculate_rfm_features(gifts, population_2018, observation_date_2018, 12)
rfm_24m_2018 <- calculate_rfm_features(gifts, population_2018, observation_date_2018, 24)

# Trend features
trends_2018 <- calculate_trend_features(gifts, population_2018, observation_date_2018, 3, 12)

# Lifetime features
lifetime_2018 <- calculate_lifetime_features(gifts, population_2018, observation_date_2018)

# Merge features
basetable_2018 <- basetable_2018 %>%
  left_join(rfm_12m_2018, by = c("donor_id" = "id")) %>%
  left_join(rfm_24m_2018, by = c("donor_id" = "id")) %>%
  left_join(trends_2018, by = c("donor_id" = "id")) %>%
  left_join(lifetime_2018, by = c("donor_id" = "id"))

# Define target
target_gifts_2018 <- gifts %>%
  filter(date >= observation_date_2018 & date < target_end_2018)

donors_donated_2018 <- unique(target_gifts_2018$id)

basetable_2018 <- basetable_2018 %>%
  mutate(
    target_donated = ifelse(donor_id %in% donors_donated_2018, 1, 0)
  )

# Calculate continuous target
target_amounts_2018 <- target_gifts_2018 %>%
  group_by(id) %>%
  summarise(target_amount = sum(amount), .groups = "drop")

basetable_2018 <- basetable_2018 %>%
  left_join(target_amounts_2018, by = c("donor_id" = "id")) %>%
  mutate(target_amount = replace_na(target_amount, 0))

cat("  Basetable 2018 created!\n")
cat("    Rows:", nrow(basetable_2018), "\n")
cat("    Columns:", ncol(basetable_2018), "\n")
cat("    Target response rate:", round(mean(basetable_2018$target_donated) * 100, 2), "%\n\n")

# ============================================================================
# PJESA 7: FEATURE CATALOG
# ============================================================================

cat("============================================================================\n")
cat("STEP 7: Creating FEATURE CATALOG\n")
cat("============================================================================\n")

feature_catalog <- tibble(
  feature_name = c(
    "recency_days_12m", "frequency_12m", "monetary_total_12m",
    "monetary_mean_12m", "monetary_max_12m", "monetary_min_12m",
    "recency_days_24m", "frequency_24m", "monetary_total_24m",
    "amount_trend", "count_trend", "amount_growth",
    "days_since_first", "days_since_last", "donor_age_days",
    "lifetime_donations", "lifetime_amount", "avg_days_between",
    "is_new_donor", "age_at_observation", "gender", "country",
    "region", "segment"
  ),
  feature_type = c(
    rep("RFM_12m", 6),
    rep("RFM_24m", 3),
    rep("Trend", 3),
    rep("Lifetime", 6),
    rep("Flag", 1),
    rep("Demographic", 5)
  ),
  description = c(
    "Days since last donation (12-month window)",
    "Number of donations (12-month window)",
    "Total donation amount (12-month window)",
    "Average donation amount (12-month window)",
    "Maximum donation amount (12-month window)",
    "Minimum donation amount (12-month window)",
    "Days since last donation (24-month window)",
    "Number of donations (24-month window)",
    "Total donation amount (24-month window)",
    "Donation amount trend (3m vs 12m)",
    "Donation count trend (3m vs 12m)",
    "Amount growth rate",
    "Days since first donation",
    "Days since most recent donation",
    "Donor tenure in days",
    "Total lifetime donations",
    "Total lifetime amount donated",
    "Average days between donations",
    "New donor flag (first donation < 1 year)",
    "Age at observation date",
    "Gender (M/F)",
    "Country",
    "Region",
    "Donor segment (Gold/Silver/Bronze)"
  ),
  data_type = c(
    rep("Numeric", 18),
    "Binary",
    "Numeric",
    rep("Categorical", 4)
  )
)

cat("  Feature catalog created with", nrow(feature_catalog), "features\n\n")

# ============================================================================
# PJESA 8: RUAJTJA E DATASET-EVE
# ============================================================================

cat("============================================================================\n")
cat("STEP 8: Saving all datasets\n")
cat("============================================================================\n")

output_dir <- getwd()

cat("  Saving to directory:", output_dir, "\n\n")

# Ruajmë dataset-et
cat("  1. Saving gifts.csv...\n")
write.csv(gifts, file.path(output_dir, "gifts.csv"), row.names = FALSE)

cat("  2. Saving donors.csv...\n")
write.csv(donors, file.path(output_dir, "donors.csv"), row.names = FALSE)

cat("  3. Saving basetable_2017.csv...\n")
write.csv(basetable_2017, file.path(output_dir, "basetable_2017.csv"), row.names = FALSE)

cat("  4. Saving basetable_2018.csv...\n")
write.csv(basetable_2018, file.path(output_dir, "basetable_2018.csv"), row.names = FALSE)

cat("  5. Saving feature_catalog.csv...\n")
write.csv(feature_catalog, file.path(output_dir, "feature_catalog.csv"), row.names = FALSE)

# Ruajmë edhe në RDS format
cat("  6. Saving gifts.rds...\n")
saveRDS(gifts, file.path(output_dir, "gifts.rds"))

cat("  7. Saving basetable_2017.rds...\n")
saveRDS(basetable_2017, file.path(output_dir, "basetable_2017.rds"))

cat("  8. Saving basetable_2018.rds...\n")
saveRDS(basetable_2018, file.path(output_dir, "basetable_2018.rds"))

cat("\n  All datasets saved successfully!\n\n")

# ============================================================================
# PJESA 9: SUMMARY STATISTICS
# ============================================================================

cat("============================================================================\n")
cat("STEP 9: SUMMARY STATISTICS\n")
cat("============================================================================\n\n")

cat("GIFTS DATASET:\n")
cat("  Total transactions:", nrow(gifts), "\n")
cat("  Unique donors:", length(unique(gifts$id)), "\n")
cat("  Date range:", as.character(min(gifts$date)), "to", as.character(max(gifts$date)), "\n")
cat("  Total amount: €", format(sum(gifts$amount), big.mark = ","), "\n")
cat("  Average amount: €", round(mean(gifts$amount), 2), "\n\n")

cat("BASETABLE 2017:\n")
cat("  Population:", nrow(basetable_2017), "donors\n")
cat("  Features:", ncol(basetable_2017) - 2, "(excluding ID and target)\n")
cat("  Response rate:", round(mean(basetable_2017$target_donated) * 100, 2), "%\n")
cat("  Average target amount: €", round(mean(basetable_2017$target_amount), 2), "\n\n")

cat("BASETABLE 2018:\n")
cat("  Population:", nrow(basetable_2018), "donors\n")
cat("  Features:", ncol(basetable_2018) - 2, "(excluding ID and target)\n")
cat("  Response rate:", round(mean(basetable_2018$target_donated) * 100, 2), "%\n")
cat("  Average target amount: €", round(mean(basetable_2018$target_amount), 2), "\n\n")

cat("FEATURE CATALOG:\n")
cat("  Total features:", nrow(feature_catalog), "\n")
cat("  Feature types:\n")
print(table(feature_catalog$feature_type))
cat("\n")

# ============================================================================
# PËRFUNDIMI
# ============================================================================

cat("============================================================================\n")
cat("  DATA GENERATION COMPLETED SUCCESSFULLY!\n")
cat("============================================================================\n\n")

cat("Generated files:\n")
cat("  1. gifts.csv - Transaction-level donation data\n")
cat("  2. donors.csv - Donor demographics\n")
cat("  3. basetable_2017.csv - Features + target for May 2017\n")
cat("  4. basetable_2018.csv - Features + target for May 2018\n")
cat("  5. feature_catalog.csv - Documentation of all features\n")
cat("  6-8. RDS versions of main files\n\n")

cat("Key Concepts Demonstrated:\n")
cat("  ✓ Timeline compliance (no data leakage)\n")
cat("  ✓ Multiple observation dates (2017, 2018)\n")
cat("  ✓ Population filtering (inclusion/exclusion logic)\n")
cat("  ✓ RFM features (multiple time windows)\n")
cat("  ✓ Trend features (short vs long term)\n")
cat("  ✓ Lifetime features (donor tenure)\n")
cat("  ✓ Binary and continuous targets\n\n")

cat("Ready for Lecture 2 exercises!\n\n")
cat("============================================================================\n\n")