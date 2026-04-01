# Customer Personality Analysis — Shiny Dashboard

**ESADE MIBA · Data Analytics with R · 2025**

## Research question
What makes a customer valuable — and are we targeting the right ones
with our marketing campaigns?

## How to run
1. Place `marketing_campaign.csv` in `DATA/`.
2. Open R in the `marketing_shiny/` directory.
3. `source("data_prep.R")` — generates `DATA/customers_clean.rds`.
4. `shiny::runApp()`

## Tabs
| Tab | Description |
|---|---|
| About | Dataset overview, cleaning decisions, methodology notes |
| Customer profiles | Demographic explorer — age, income, education, children |
| Spending patterns | Product mix and channel preferences by segment |
| Campaign performance | Acceptance rates, responder profiling, key predictors |
| ML predictor | Predict campaign response probability (coming soon) |

## Key EDA findings
1. Wine (50%) + Meat (28%) = 78% of all spend
2. Child-free customers spend 2.7× more (verified on both mean and median)
3. Recency: ~26% response (0–25 days) vs ~7% (75+ days)
4. Campaign history 2+: ~63% response rate vs 8% for 0 previous accepts
5. Catalogue channel scales sharply with income (0.5 → 5.3 purchases/yr)
6. Campaign 2 acceptance: 1.3% — >10× worse than best campaigns
7. Response imbalance: 14.9% Yes — ROC-AUC used for ML evaluation

## Cleaning decisions
- Removed 3 age outliers (Year_Birth in 1890s → implied age > 90)
- Replaced 1 income outlier (666,666) with NA, then imputed all 25 income NAs with median
- Consolidated marital status junk values ('YOLO', 'Absurd', 'Alone') into 'Other'
- Dropped constant columns Z_CostContact and Z_Revenue
- Final dataset: 2,237 rows, 35 columns, 0 NAs
