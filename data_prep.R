# data_prep.R
# Customer Personality Analysis — Data Preparation
# Run once before launching app.R to produce DATA/customers_clean.rds
#
# Cleaning decisions documented inline. See eda.R for the exploratory
# analysis that motivated each choice.

library(tidyverse)
library(lubridate)

# ── 1. Load raw data ──────────────────────────────────────────────────────────
# File is TAB-separated despite the .csv extension
raw <- read_delim(
  "./DATA/marketing_campaign.csv",
  delim     = "\t",
  col_types = cols(.default = col_guess()),
  show_col_types = FALSE
)

cat("Raw data loaded:", nrow(raw), "rows,", ncol(raw), "cols\n")

# ── 2. Drop useless columns ───────────────────────────────────────────────────
# Z_CostContact and Z_Revenue are constants (always 3 and 11 respectively).
# They carry zero information and would silently break any model that uses them.
# ID is a row identifier with no analytical meaning.
raw <- raw %>%
  select(-ID, -Z_CostContact, -Z_Revenue)

cat("After dropping constants: ", ncol(raw), "cols remaining\n")

# ── 3. Handle income outliers and NAs ────────────────────────────────────────
# One customer has Income = 666,666 — clearly a data entry error (next highest
# is ~162k). Replace with NA so it is handled alongside the 24 genuine NAs.
# Impute all NAs with the median (robust to skew; preferred over mean here
# because income distribution is right-skewed).
income_outlier_threshold <- 200000

raw <- raw %>%
  mutate(
    Income = if_else(Income > income_outlier_threshold, NA_real_, Income),
    Income = if_else(is.na(Income), median(Income, na.rm = TRUE), Income)
  )

cat("Income NAs after imputation:", sum(is.na(raw$Income)), "\n")
# Expected: 0

# ── 4. Fix age outliers ───────────────────────────────────────────────────────
# Three customers have Year_Birth in the 1890s–1900s, implying age > 100.
# Almost certainly data entry errors. Remove these rows entirely (only 3 of 2240).
REFERENCE_YEAR <- 2015   # approximate year of data collection

raw <- raw %>%
  mutate(Age = REFERENCE_YEAR - Year_Birth) %>%
  filter(Age >= 18, Age <= 90)

cat("Rows after removing age outliers:", nrow(raw), "\n")
# Expected: ~2237

# ── 5. Clean marital status ───────────────────────────────────────────────────
# "Alone" (3 cases), "Absurd" (2), and "YOLO" (2) are junk values.
# "Together" is functionally equivalent to "Married" (partnered household).
# Collapse into 5 clean levels: Partnered, Single, Divorced, Widow, Other.
raw <- raw %>%
  mutate(
    MaritalSimple = case_when(
      Marital_Status %in% c("Married", "Together") ~ "Partnered",
      Marital_Status == "Single"                    ~ "Single",
      Marital_Status == "Divorced"                  ~ "Divorced",
      Marital_Status == "Widow"                     ~ "Widow",
      TRUE                                           ~ "Other"    # Alone/Absurd/YOLO
    ),
    MaritalSimple = factor(MaritalSimple,
                           levels = c("Partnered", "Single",
                                      "Divorced", "Widow", "Other"))
  )

# ── 6. Order education as a factor ───────────────────────────────────────────
# "2n Cycle" is the European secondary-school leaving qualification —
# roughly equivalent to completing high school. Ordering allows meaningful
# comparisons and correct axis ordering in plots.
raw <- raw %>%
  mutate(
    Education = factor(Education,
                       levels  = c("Basic", "2n Cycle", "Graduation",
                                   "Master", "PhD"),
                       ordered = TRUE)
  )

# ── 7. Parse customer join date → Tenure ─────────────────────────────────────
# Dt_Customer is formatted as DD-MM-YYYY. Tenure = days as a customer,
# measured from a fixed reference date (2015-01-01).
REFERENCE_DATE <- as.Date("2015-01-01")

raw <- raw %>%
  mutate(
    Dt_Customer = dmy(Dt_Customer),    # lubridate: day-month-year format
    Tenure      = as.integer(REFERENCE_DATE - Dt_Customer)
  )

cat("Tenure range (days):", min(raw$Tenure), "–", max(raw$Tenure), "\n")
# Expected: roughly 365 – 885 days (1 to 2.4 years)

# ── 8. Derive spend and purchase features ─────────────────────────────────────
spend_cols   <- c("MntWines", "MntFruits", "MntMeatProducts",
                  "MntFishProducts", "MntSweetProducts", "MntGoldProds")
channel_cols <- c("NumWebPurchases", "NumCatalogPurchases",
                  "NumStorePurchases", "NumDealsPurchases")
camp_cols    <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
                  "AcceptedCmp4", "AcceptedCmp5")

raw <- raw %>%
  mutate(
    # Total spend across all product categories
    TotalSpend      = rowSums(across(all_of(spend_cols))),

    # Total purchases across all channels
    TotalPurchases  = rowSums(across(all_of(channel_cols))),

    # Average basket size — spending per purchase occasion
    # Avoid division by zero for customers with 0 purchases
    SpendPerPurchase = if_else(
      TotalPurchases > 0,
      TotalSpend / TotalPurchases,
      0
    ),

    # Proportion of purchases made via deals — proxy for price sensitivity
    DealsPct = if_else(
      TotalPurchases > 0,
      NumDealsPurchases / TotalPurchases,
      0
    ),

    # Whether the customer has any children at home
    HasChildren     = (Kidhome + Teenhome) > 0,

    # Number of previous campaigns accepted (0–5)
    CampaignHistory = rowSums(across(all_of(camp_cols)))
  )

# ── 9. Create grouping factors for visualisation ──────────────────────────────
# These bins are used as sidebar filters and chart facets.
# Terciles for income and spend (data-driven, not arbitrary round numbers).
income_breaks <- quantile(raw$Income, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
spend_breaks  <- quantile(raw$TotalSpend, probs = c(0, 1/3, 2/3, 1))

raw <- raw %>%
  mutate(
    AgeGroup  = cut(Age,
                    breaks = c(17, 35, 50, 65, 90),
                    labels = c("Under 35", "35–50", "50–65", "Over 65"),
                    right  = TRUE),
    IncomeTier = cut(Income,
                     breaks         = income_breaks,
                     labels         = c("Low", "Mid", "High"),
                     include.lowest = TRUE),
    SpendTier  = cut(TotalSpend,
                     breaks         = spend_breaks,
                     labels         = c("Low", "Mid", "High"),
                     include.lowest = TRUE),
    # Response as a labelled factor — cleaner in plots than 0/1
    Response   = factor(if_else(Response == 1, "Yes", "No"),
                        levels = c("Yes", "No"))
  )

# ── 10. Drop columns no longer needed ────────────────────────────────────────
customers_clean <- raw %>%
  select(
    # Demographics
    Age, AgeGroup, Education, MaritalSimple, Income, IncomeTier,
    HasChildren, Kidhome, Teenhome, Tenure,
    # Spend
    all_of(spend_cols), TotalSpend, SpendTier, SpendPerPurchase,
    # Channels
    all_of(channel_cols), TotalPurchases, DealsPct,
    # Campaigns
    all_of(camp_cols), CampaignHistory,
    # Engagement
    Recency, NumWebVisitsMonth, Complain,
    # Target
    Response
  )

# ── 11. Final validation ──────────────────────────────────────────────────────
cat("\n=== FINAL DATASET SUMMARY ===\n")
cat("Rows:", nrow(customers_clean), "\n")         # expected: ~2216
cat("Cols:", ncol(customers_clean), "\n")          # expected: ~35
cat("NA count:\n"); print(colSums(is.na(customers_clean)))
cat("\nResponse distribution:\n")
print(table(customers_clean$Response))             # expected: ~334 Yes, ~1882 No
cat("\nIncome range: $",
    round(min(customers_clean$Income)), "– $",
    round(max(customers_clean$Income)), "\n")
cat("Age range:", min(customers_clean$Age), "–", max(customers_clean$Age), "\n")
cat("TotalSpend range: $", min(customers_clean$TotalSpend),
    "– $", max(customers_clean$TotalSpend), "\n")

# ── 12. Save ──────────────────────────────────────────────────────────────────
saveRDS(customers_clean, "./DATA/customers_clean.rds")
cat("\n✓ customers_clean.rds saved —",
    nrow(customers_clean), "rows,", ncol(customers_clean), "columns\n")
