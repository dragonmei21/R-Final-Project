# 02_data_prep.R
# Customer Personality Analysis — Data Preparation
#
# Run after 01_raw_eda.R, before 03_postclean_eda.R and app.R.
# Produces DATA/customers_clean.rds.
#
# Every cleaning decision below is justified by evidence in 01_raw_eda.R.
# Do NOT read the cleaned file here — always read the raw CSV.

library(tidyverse)
library(lubridate)

# ── 0. Load raw data ──────────────────────────────────────────────────────────
raw_path <- "./DATA/marketing_campaign.csv"
if (!file.exists(raw_path)) stop("Raw file not found: ", raw_path)

raw <- read_delim(
  raw_path,
  delim          = "\t",
  col_types      = cols(.default = col_guess()),
  show_col_types = FALSE
)

n_raw <- nrow(raw)
cat("Raw data loaded:", n_raw, "rows,", ncol(raw), "cols\n")

# Track removals for the final audit
removed <- list()

# ── 1. Drop non-informative columns ──────────────────────────────────────────
# Z_CostContact (always 3) and Z_Revenue (always 11) are constants.
# Constants carry no predictive or analytical information.
# ID is a row identifier — it must not be used as a feature or predictor.
raw <- raw %>%
  select(-ID, -Z_CostContact, -Z_Revenue)

cat("Non-informative columns dropped. Remaining:", ncol(raw), "\n")

# ── 2. Income: flag, handle outlier, impute ───────────────────────────────────
# Evidence (01_raw_eda.R A5):
#   - One row has Income = 666,666 (~4x the 99th percentile of ~$162k).
#     This is almost certainly a data-entry error.
#   - 24 rows have genuinely missing Income.
#   - Distribution is right-skewed → median preferred over mean for imputation.
#
# An IncomeImputed flag is added BEFORE imputation so downstream analysis
# can test whether imputed rows behave differently.

INCOME_OUTLIER_THRESHOLD <- 200000

n_income_outliers  <- sum(!is.na(raw$Income) & raw$Income > INCOME_OUTLIER_THRESHOLD)
n_income_na_before <- sum(is.na(raw$Income))

raw <- raw %>%
  mutate(
    # Flag rows that will be imputed (outlier converted to NA, or genuine NA)
    IncomeImputed = is.na(Income) | (!is.na(Income) & Income > INCOME_OUTLIER_THRESHOLD),
    # Convert outlier to NA so it enters the same imputation path
    Income        = if_else(!is.na(Income) & Income > INCOME_OUTLIER_THRESHOLD,
                            NA_real_, Income),
    # Impute all NAs with the training-set median (computed on non-NA values only)
    Income        = if_else(is.na(Income), median(Income, na.rm = TRUE), Income)
  )

cat("Income outliers flagged and imputed:", n_income_outliers, "\n")
cat("Income NAs imputed:", n_income_na_before, "\n")
cat("IncomeImputed = TRUE for:", sum(raw$IncomeImputed), "rows\n")
cat("Income NAs remaining:", sum(is.na(raw$Income)), "\n")

# ── 3. Derive age; remove implausible rows ────────────────────────────────────
# Evidence (01_raw_eda.R A6, A7):
#   - Dt_Customer dates range from ~2012 to late 2014.
#   - Three customers have Year_Birth in the 1890s, implying age > 120.
#   - Reference year 2015: data collection / campaign assumed to have occurred
#     in early-to-mid 2015. This is an assumption — if the exact campaign date
#     is known, substitute it here.
#   - Filter: remove Age < 18 (not a plausible customer) or Age >= 100
#     (physiologically implausible — only the three 1890s rows are affected).

REFERENCE_YEAR <- 2015  # assumption: adjust if actual campaign year is known

raw <- raw %>%
  mutate(Age = REFERENCE_YEAR - Year_Birth)

n_age_removed <- sum(raw$Age < 18 | raw$Age >= 100)
removed$age_outliers <- n_age_removed
cat("Rows removed for implausible age (< 18 or >= 100):", n_age_removed, "\n")

raw <- raw %>% filter(Age >= 18, Age < 100)

if (max(raw$Age) > 85) {
  warning("Oldest customer is ", max(raw$Age), " — verify this is not a data error.")
}

# ── 4. Parse join date → Tenure ───────────────────────────────────────────────
# Evidence (01_raw_eda.R A7): Dt_Customer is formatted DD-MM-YYYY.
# Tenure = integer days from customer join date to a fixed reference date.
# REFERENCE_DATE is set to 2015-06-01 (mid-2015) as a conservative midpoint
# consistent with REFERENCE_YEAR above. If the exact campaign cut-off date is
# known, use that date instead.

REFERENCE_DATE <- as.Date("2015-06-01")  # assumption — see note above

raw <- raw %>%
  mutate(Dt_Customer = dmy(Dt_Customer))

n_dt_fail <- sum(is.na(raw$Dt_Customer))
if (n_dt_fail > 0) {
  stop("Date parsing produced ", n_dt_fail,
       " NAs in Dt_Customer. Expected format is DD-MM-YYYY.")
}

raw <- raw %>%
  mutate(Tenure = as.integer(REFERENCE_DATE - Dt_Customer))

if (any(raw$Tenure < 0)) {
  warning("Negative tenure detected for ",
          sum(raw$Tenure < 0), " rows. Check REFERENCE_DATE vs Dt_Customer.")
}
cat("Tenure range (days):", min(raw$Tenure), "-", max(raw$Tenure), "\n")

# ── 5. Clean marital status ───────────────────────────────────────────────────
# Evidence (01_raw_eda.R A8) — raw value counts drive the mapping:
#
#   "Married"  + "Together" → "Partnered"  (both indicate a shared household)
#   "Single"   + "Alone"    → "Single"
#     Note: "Alone" (n=3) means living without a partner — semantically it
#     belongs with Single, NOT with the junk-data group.
#     Previous code mapped "Alone" to "Other", which was a mistake.
#   "Divorced"              → "Divorced"
#   "Widow"                 → "Widow"
#   "Absurd"   + "YOLO"     → NA
#     These are clearly erroneous entries (n=2 each). Mapping them to a
#     labelled "Other" category implies they form a meaningful group, which
#     they do not. Setting to NA is more honest.

raw <- raw %>%
  mutate(
    MaritalSimple = case_when(
      Marital_Status %in% c("Married", "Together") ~ "Partnered",
      Marital_Status %in% c("Single",  "Alone")    ~ "Single",
      Marital_Status == "Divorced"                  ~ "Divorced",
      Marital_Status == "Widow"                     ~ "Widow",
      TRUE                                          ~ NA_character_
    ),
    MaritalSimple = factor(MaritalSimple,
                           levels = c("Partnered", "Single", "Divorced", "Widow"))
  )

cat("\nMarital status after cleaning:\n")
print(table(raw$MaritalSimple, useNA = "ifany"))

# ── 6. Order education as a factor ───────────────────────────────────────────
# "2n Cycle" is the European secondary-school leaving qualification.
# Ordered factor gives correct plot axis ordering and allows >= comparisons
# in grouped summaries.
# CAUTION: ordered does not imply equal numeric distance between levels.
# Treat as ordinal in modeling, not interval-scaled.

raw <- raw %>%
  mutate(
    Education = factor(Education,
                       levels  = c("Basic", "2n Cycle", "Graduation",
                                   "Master", "PhD"),
                       ordered = TRUE)
  )

# ── 7. Derive spend and purchase features ─────────────────────────────────────
spend_cols <- c("MntWines", "MntFruits", "MntMeatProducts",
                "MntFishProducts", "MntSweetProducts", "MntGoldProds")

# NumDealsPurchases is NOT included in TotalPurchases.
# Rationale (01_raw_eda.R A10): deal purchases almost certainly occur through
# one of the three channels below — including them would double-count those
# transactions. DealsPct is defined separately as a price-sensitivity proxy.
# ASSUMPTION: deals are a subset of channel purchases. If the original data
# dictionary shows they are an independent channel, this logic must be revised.
purchase_channel_cols <- c("NumWebPurchases", "NumCatalogPurchases",
                            "NumStorePurchases")

camp_cols <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
               "AcceptedCmp4", "AcceptedCmp5")

raw <- raw %>%
  mutate(
    TotalSpend = rowSums(across(all_of(spend_cols))),

    # Total purchases across the three distinct channels only
    TotalPurchases = rowSums(across(all_of(purchase_channel_cols))),

    # Average spend per purchase (0 where no purchases recorded)
    SpendPerPurchase = if_else(TotalPurchases > 0,
                               TotalSpend / TotalPurchases,
                               0),

    # Proportion of purchases made via deals — proxy for price sensitivity
    DealsPct = if_else(TotalPurchases > 0,
                       NumDealsPurchases / TotalPurchases,
                       0),

    HasChildren     = (Kidhome + Teenhome) > 0,
    CampaignHistory = rowSums(across(all_of(camp_cols)))
  )

# Validate deal-subset assumption: DealsPct > 1 would contradict it
n_deals_exceed <- sum(raw$DealsPct > 1, na.rm = TRUE)
if (n_deals_exceed > 0) {
  warning(n_deals_exceed, " rows have NumDealsPurchases > TotalPurchases.",
          " The deal-subset assumption may be wrong — check the data dictionary.")
} else {
  cat("Deal-subset check passed: DealsPct <= 1 for all rows. ✓\n")
}

# ── 8. Grouping bins for visualisation ───────────────────────────────────────
# AgeGroup, IncomeTier, SpendTier are display-only bins for dashboard filters
# and chart facets. They are NOT inherently meaningful modeling features.
# Boundaries are data-driven (terciles) but chosen for display, not analysis.
# safe_cut_tercile() handles the edge case where quantile breaks are not unique
# (possible in small or heavily-tied distributions).

safe_cut_tercile <- function(x, labels = c("Low", "Mid", "High")) {
  breaks <- unique(quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
  if (length(breaks) < 4) {
    warning("Non-unique tercile breaks — falling back to median split.")
    breaks <- unique(quantile(x, probs = c(0, 0.5, 1), na.rm = TRUE))
    labels <- c("Low", "High")
  }
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

raw <- raw %>%
  mutate(
    AgeGroup   = cut(Age,
                     breaks = c(17, 35, 50, 65, 99),
                     labels = c("Under 35", "35-50", "50-65", "Over 65"),
                     right  = TRUE),
    IncomeTier = safe_cut_tercile(Income),
    SpendTier  = safe_cut_tercile(TotalSpend),
    Response   = factor(if_else(Response == 1, "Yes", "No"),
                        levels = c("Yes", "No"))
  )

# ── 9. Select final columns ───────────────────────────────────────────────────
customers_clean <- raw %>%
  select(
    Age, AgeGroup, Education, MaritalSimple, Income, IncomeImputed, IncomeTier,
    HasChildren, Kidhome, Teenhome, Tenure,
    all_of(spend_cols), TotalSpend, SpendTier, SpendPerPurchase,
    all_of(purchase_channel_cols), NumDealsPurchases, TotalPurchases, DealsPct,
    all_of(camp_cols), CampaignHistory,
    Recency, NumWebVisitsMonth, Complain,
    Response
  )

# ── 10. Validation summary ────────────────────────────────────────────────────
cat("\n=== CLEANING SUMMARY ===\n")
cat("Raw rows:                   ", n_raw, "\n")
cat("Age outliers removed:       ", removed$age_outliers, "\n")
cat("Final clean rows:           ", nrow(customers_clean), "\n")
cat("Final columns:              ", ncol(customers_clean), "\n")
cat("Income rows imputed:        ", sum(customers_clean$IncomeImputed), "\n")

cat("\nNA counts in cleaned dataset:\n")
na_counts <- colSums(is.na(customers_clean))
has_na <- na_counts[na_counts > 0]
if (length(has_na) == 0) {
  cat("  None (except MaritalSimple for Absurd/YOLO rows — expected).\n")
} else {
  print(has_na)
}

cat("\nResponse distribution:\n")
print(table(customers_clean$Response))

cat("\nKey variable ranges:\n")
cat("  Age:             ", min(customers_clean$Age),       "-", max(customers_clean$Age), "\n")
cat("  Income:         $", round(min(customers_clean$Income)), "-",
    "$", round(max(customers_clean$Income)), "\n")
cat("  Tenure (days):   ", min(customers_clean$Tenure),    "-", max(customers_clean$Tenure), "\n")
cat("  TotalSpend:     $", min(customers_clean$TotalSpend), "-",
    "$", max(customers_clean$TotalSpend), "\n")
cat("  TotalPurchases:  ", min(customers_clean$TotalPurchases), "-",
    max(customers_clean$TotalPurchases), "\n")
cat("  DealsPct:        ", round(min(customers_clean$DealsPct), 3), "-",
    round(max(customers_clean$DealsPct), 3), "\n")
cat("  SpendPerPurchase:$", round(min(customers_clean$SpendPerPurchase)), "-",
    "$", round(max(customers_clean$SpendPerPurchase)), "\n")

cat("\nCleaned categorical distributions:\n")
cat("Education:\n");    print(table(customers_clean$Education))
cat("MaritalSimple:\n"); print(table(customers_clean$MaritalSimple, useNA = "ifany"))
cat("IncomeTier:\n");   print(table(customers_clean$IncomeTier))
cat("AgeGroup:\n");     print(table(customers_clean$AgeGroup))

# ── 11. Save ──────────────────────────────────────────────────────────────────
saveRDS(customers_clean, "./DATA/customers_clean.rds")
cat("\n✓ customers_clean.rds saved —",
    nrow(customers_clean), "rows,", ncol(customers_clean), "cols\n")
