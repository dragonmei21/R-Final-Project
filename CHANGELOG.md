# CHANGELOG — Customer Personality Analysis Pipeline

## What changed and why

---

### 1. Methodological fixes

**Raw EDA separated from post-cleaning EDA**

The original `data_prep.R` claimed its cleaning decisions were motivated by `eda.R`. However, `eda.R` reads the already-cleaned `customers_clean.rds` — it cannot justify the cleaning decisions that produced that file. This is a circular justification.

Fixed by splitting the workflow into three scripts:
- `01_raw_eda.R` — inspects the raw CSV before any transformation. This is the evidence base for cleaning decisions.
- `02_data_prep.R` — applies cleaning and feature engineering, citing `01_raw_eda.R` as the justification source.
- `03_postclean_eda.R` — describes the cleaned customer base for stakeholder-facing analysis.

**Descriptive vs inferential language separated**

Several claims in the original `eda.R` used language that implied statistical inference without running any formal tests:
- "statistically reliable" → rewritten as "not driven by small sample sizes, but no formal test has been run"
- "independent predictor" (from a scatterplot only) → rewritten as "associated with spend beyond income"
- "will improve ML performance" (log transforms) → rewritten as "may help — should be tested empirically"

The "strongest predictor" conflict is resolved: recency shows the steepest bucketed gradient; CampaignHistory has the highest Pearson correlation. Both claims are now qualified by the method used, so they are no longer contradictory.

---

### 2. Feature engineering fixes

**TotalPurchases corrected**

Old definition included `NumDealsPurchases` as a fourth channel alongside web, catalog, and store:
```r
# OLD (wrong)
channel_cols <- c("NumWebPurchases", "NumCatalogPurchases",
                  "NumStorePurchases", "NumDealsPurchases")
TotalPurchases = rowSums(across(all_of(channel_cols)))
```

This almost certainly double-counts purchases. Deal purchases are assumed to occur *through* one of the three channels, not in addition to them. The corrected definition:
```r
# NEW (correct)
purchase_channel_cols <- c("NumWebPurchases", "NumCatalogPurchases",
                            "NumStorePurchases")
TotalPurchases = rowSums(across(all_of(purchase_channel_cols)))
```

**SpendPerPurchase corrected**

Now uses the corrected `TotalPurchases` (Web + Catalog + Store only). The old version divided by a double-counted total, producing systematically lower values.

**DealsPct corrected**

Now defined as `NumDealsPurchases / TotalPurchases` where `TotalPurchases` is the channel-only total. A validation check confirms no rows have `DealsPct > 1` (which would indicate a contradiction with the subset assumption).

**IncomeImputed flag added**

The original script imputed income silently. A new `IncomeImputed` column is now set to `TRUE` before imputation for any row where income was missing or was the outlier value. This lets downstream analysis test whether imputed rows behave differently.

---

### 3. Documentation fixes

**Circular reference removed**

`data_prep.R` previously said: *"See eda.R for the exploratory analysis that motivated each choice."* This has been removed. `02_data_prep.R` now references `01_raw_eda.R` instead.

**"Silently break any model" removed**

The original comment said constants "would silently break any model that uses them." This is an overstatement. Replaced with accurate language: constants carry no predictive or analytical information.

**Stale expected row counts removed**

Comments like `# Expected: ~2237` were removed. The final validation section now prints actual counts computed from the data.

**Marital status mapping corrected**

The original code mapped `"Alone"` to `"Other"` alongside `"Absurd"` and `"YOLO"`. This conflates a plausible value ("Alone" = living without a partner) with clearly erroneous entries. Fixed:
- `"Alone"` → `"Single"` (semantically appropriate)
- `"Absurd"`, `"YOLO"` → `NA` (too few to form a group; no analytical interpretation)

**purchase_channel_cols naming**

The variable previously named `channel_cols` included deal purchases, making the name misleading. Renamed to `purchase_channel_cols` to make the scope clear. `NumDealsPurchases` is now explicitly separated and documented as a price-sensitivity proxy.

**Validation checks added**

New explicit checks in `02_data_prep.R`:
- Date parsing: script stops with an informative error if `dmy()` produces unexpected NAs
- Negative tenure: warning if any tenure values are negative
- Deal-subset assumption: warning if any row has `NumDealsPurchases > TotalPurchases`
- Age range after cleaning: warning if oldest customer exceeds 85
- Non-unique quantile breaks: `safe_cut_tercile()` falls back to median split with a warning

---

### 4. Unresolved assumptions requiring confirmation

| Assumption | Where used | Status |
|---|---|---|
| `NumDealsPurchases` is a subset of Web/Catalog/Store purchases | `TotalPurchases`, `DealsPct` | **Assumed** — no data dictionary available. If deals are an independent channel, revert to including them in `TotalPurchases` and remove the subset validation. |
| Reference year 2015 for age and tenure | `Age = 2015 - Year_Birth`, `Tenure = REFERENCE_DATE - Dt_Customer` | **Assumed** — inferred from the max Dt_Customer date (~late 2014). If the actual campaign date is known, replace `REFERENCE_YEAR` and `REFERENCE_DATE` in `02_data_prep.R`. |
| `"Alone"` means single (no partner), not some other category | Marital status mapping | **Assumed** — based on the plain English meaning. Confirm with the original data collection context. |
| Ordered education factor implies a hierarchy, not equal spacing | `Education` factor | Documented. Do not treat as interval-scaled in regression without justification. |
