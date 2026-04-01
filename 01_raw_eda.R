# 01_raw_eda.R
# Customer Personality Analysis — Raw Data Audit
#
# Run FIRST, before any cleaning.
# Purpose: inspect the original CSV and surface evidence that justifies the
#          cleaning decisions made in 02_data_prep.R.
# Output:  structured console report only. No data is written or modified.

library(tidyverse)
library(lubridate)

# ── 0. Locate raw file ────────────────────────────────────────────────────────
raw_path <- "./DATA/marketing_campaign.csv"
if (!file.exists(raw_path)) {
  stop("Raw file not found at: ", raw_path)
}

# ── A1. Confirm delimiter before parsing ──────────────────────────────────────
cat("============================================================\n")
cat("A1. FILE DELIMITER CHECK\n")
cat("============================================================\n")
first_line <- readLines(raw_path, n = 1)
cat("First 120 chars of row 1:\n  ", substr(first_line, 1, 120), "...\n")
cat("Tab count in row 1:  ", stringr::str_count(first_line, "\t"), "\n")
cat("Comma count in row 1:", stringr::str_count(first_line, ","), "\n\n")
# Expected: many tabs, zero commas — file is TSV despite .csv extension.

raw <- read_delim(
  raw_path,
  delim          = "\t",
  col_types      = cols(.default = col_guess()),
  show_col_types = FALSE
)

# ── A2. Dimensions and column types ───────────────────────────────────────────
cat("============================================================\n")
cat("A2. RAW DIMENSIONS AND TYPES\n")
cat("============================================================\n")
cat("Rows:", nrow(raw), " | Cols:", ncol(raw), "\n\n")
glimpse(raw)

# ── A3. Missingness audit ──────────────────────────────────────────────────────
cat("\n============================================================\n")
cat("A3. MISSINGNESS BY COLUMN\n")
cat("============================================================\n")
missing_tbl <- raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(pct = round(n_missing / nrow(raw) * 100, 2)) %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))

if (nrow(missing_tbl) == 0) {
  cat("No missing values detected in raw file.\n")
} else {
  print(missing_tbl)
}

# ── A4. Unique-value counts — catch constants and near-constants ───────────────
cat("\n============================================================\n")
cat("A4. UNIQUE VALUES PER COLUMN (bottom 15 — candidates for review)\n")
cat("============================================================\n")
unique_counts <- raw %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_unique") %>%
  arrange(n_unique)

print(head(unique_counts, 15))

cat("\nColumns with only 1 unique value (true constants — no information):\n")
constants <- unique_counts %>% filter(n_unique == 1)
if (nrow(constants) == 0) {
  cat("  None.\n")
} else {
  for (col in constants$column) {
    cat(" ", col, ":", paste(unique(raw[[col]]), collapse = ", "), "\n")
  }
}

cat("\nColumns with exactly 2 unique values:\n")
near_const <- unique_counts %>% filter(n_unique == 2)
for (col in near_const$column) {
  cat(" ", col, ":", paste(sort(unique(raw[[col]])), collapse = ", "), "\n")
}

# ── A5. Income — outlier evidence ─────────────────────────────────────────────
cat("\n============================================================\n")
cat("A5. INCOME — DISTRIBUTION AND OUTLIERS\n")
cat("============================================================\n")
cat("Quantile summary:\n")
print(quantile(raw$Income, probs = c(0, .01, .10, .25, .50, .75, .90, .99, 1),
               na.rm = TRUE))

cat("\nRows with Income > $200,000 (> 4x the 99th percentile):\n")
income_extreme <- raw %>%
  filter(!is.na(Income), Income > 200000) %>%
  select(ID, Income, Education, Marital_Status, Year_Birth)
print(income_extreme)
cat("→ Income = 666,666 is implausible (~4x the p99 of ~$162k).",
    "Decision: convert to NA and impute with median.\n")

cat("\nRows with missing Income (genuine NAs):\n")
raw %>%
  filter(is.na(Income)) %>%
  select(ID, Education, Marital_Status, Year_Birth, Kidhome, Teenhome) %>%
  print()
cat("→ N missing:", sum(is.na(raw$Income)),
    "— right-skewed distribution favours median imputation over mean.\n")

# ── A6. Year_Birth — age outlier evidence ─────────────────────────────────────
cat("\n============================================================\n")
cat("A6. YEAR_BIRTH — IMPLAUSIBLE AGES\n")
cat("============================================================\n")
cat("Year_Birth quantiles:\n")
print(quantile(raw$Year_Birth, probs = c(0, .01, .05, .50, .95, .99, 1),
               na.rm = TRUE))

cat("\nRows with Year_Birth < 1925 (age > 90 assuming data collection ~2015):\n")
raw %>%
  filter(Year_Birth < 1925) %>%
  select(ID, Year_Birth, Income, Education, Marital_Status) %>%
  print()
cat("→ Three rows with birth years 1893–1900 imply ages 115–122.",
    "Almost certainly data-entry errors. Remove these rows.\n")

# ── A7. Dt_Customer — format and date range ────────────────────────────────────
cat("\n============================================================\n")
cat("A7. DT_CUSTOMER — FORMAT AND PARSING VALIDATION\n")
cat("============================================================\n")
cat("Sample raw values:\n")
print(head(raw$Dt_Customer, 12))

dt_parsed <- dmy(raw$Dt_Customer)
n_parse_fail <- sum(is.na(dt_parsed)) - sum(is.na(raw$Dt_Customer))
cat("\ndmy() parse failures (new NAs introduced):", n_parse_fail, "\n")
cat("Parsed date range:",
    as.character(min(dt_parsed, na.rm = TRUE)), "to",
    as.character(max(dt_parsed, na.rm = TRUE)), "\n")
cat("→ Customers joined between ~2012 and ~2014.",
    "Reference year 2015 is just outside this range, giving positive tenure.\n")
cat("→ Assumption: campaign data frozen in early-to-mid 2015.",
    "If exact date is known, use it in 02_data_prep.R.\n")

# ── A8. Marital status — all raw levels ───────────────────────────────────────
cat("\n============================================================\n")
cat("A8. MARITAL_STATUS — RAW VALUE COUNTS\n")
cat("============================================================\n")
raw %>%
  count(Marital_Status, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()
cat("\n→ 'Together' (~580): functionally partnered — collapse with 'Married'.\n")
cat("→ 'Alone'   (n=3):  living without a partner — maps to 'Single',",
    "NOT 'Other'.\n")
cat("→ 'Absurd'  (n=2) and 'YOLO' (n=2): junk entries — map to NA.\n")

# ── A9. Education — raw value counts ──────────────────────────────────────────
cat("\n============================================================\n")
cat("A9. EDUCATION — RAW VALUE COUNTS\n")
cat("============================================================\n")
raw %>%
  count(Education, sort = TRUE) %>%
  print()
cat("→ '2n Cycle': European secondary-school leaving certificate.\n")
cat("→ Ordering as an ordinal factor is useful for plotting.",
    "Does not imply equal numeric spacing between levels.\n")

# ── A10. NumDealsPurchases — channel-overlap check ────────────────────────────
cat("\n============================================================\n")
cat("A10. NUMDEALSPURCHASES — CHANNEL OVERLAP CHECK\n")
cat("============================================================\n")
cat("This check tests whether deal purchases appear to be a SUBSET of",
    "web+catalog+store purchases (suggesting they should NOT be summed in",
    "as a separate channel) or an additional independent count.\n\n")

channel_check <- raw %>%
  mutate(
    channel_sum  = NumWebPurchases + NumCatalogPurchases + NumStorePurchases,
    deals_exceed = NumDealsPurchases > channel_sum
  )

cat("Rows where NumDealsPurchases > (Web + Catalog + Store):",
    sum(channel_check$deals_exceed, na.rm = TRUE), "\n")
cat("Max NumDealsPurchases:", max(raw$NumDealsPurchases, na.rm = TRUE), "\n")
cat("Correlation(NumDealsPurchases, channel_sum):",
    round(cor(channel_check$NumDealsPurchases, channel_check$channel_sum,
              use = "complete.obs"), 3), "\n")

cat("\nChannel column means:\n")
raw %>%
  summarise(
    NumWebPurchases     = mean(NumWebPurchases, na.rm = TRUE),
    NumCatalogPurchases = mean(NumCatalogPurchases, na.rm = TRUE),
    NumStorePurchases   = mean(NumStorePurchases, na.rm = TRUE),
    NumDealsPurchases   = mean(NumDealsPurchases, na.rm = TRUE)
  ) %>%
  pivot_longer(everything()) %>%
  print()

cat("\n→ ASSUMPTION: NumDealsPurchases counts purchases made via discount deals,",
    "\n  which take place through one of the three channels above.",
    "\n  Including it in TotalPurchases double-counts those transactions.",
    "\n  TotalPurchases = Web + Catalog + Store only.",
    "\n  DealsPct = NumDealsPurchases / TotalPurchases.",
    "\n  ⚠ This interpretation is assumed. Verify with the data dictionary.\n")

# ── A11. Response variable ─────────────────────────────────────────────────────
cat("\n============================================================\n")
cat("A11. RESPONSE VARIABLE (TARGET)\n")
cat("============================================================\n")
raw %>%
  count(Response) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()
cat("→ ~14.9% positive class. Class imbalance — accuracy alone is misleading.",
    "Use AUC.\n")

cat("\n============================================================\n")
cat("RAW AUDIT COMPLETE\n")
cat("Raw file:", nrow(raw), "rows,", ncol(raw), "cols\n")
cat("Proceed to 02_data_prep.R\n")
cat("============================================================\n")
