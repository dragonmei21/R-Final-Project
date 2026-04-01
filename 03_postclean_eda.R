# 03_postclean_eda.R
# Customer Personality Analysis — Post-cleaning EDA
#
# Run after 02_data_prep.R. Reads customers_clean.rds.
#
# Purpose: describe the cleaned customer base, surface patterns useful for
#          visualisation and modeling intuition, and generate defensible insights.
#
# This script does NOT justify raw cleaning decisions — that is 01_raw_eda.R's job.
#
# Language policy:
#   - Use "associated with", "appears linked to", "pattern suggests"
#   - Do NOT use "independently predicts", "statistically reliable", "proves",
#     or "will improve" unless formal inferential machinery is run.
#   - Where sample sizes are small, note that estimates are imprecise.
#   - Recency and campaign history are both descriptively strong predictors;
#     that is not a contradiction — each section is explicit about the method used.

library(tidyverse)
library(scales)
library(patchwork)

customers <- readRDS("./DATA/customers_clean.rds")

# Column groups — consistent with 02_data_prep.R
spend_cols            <- c("MntWines", "MntFruits", "MntMeatProducts",
                            "MntFishProducts", "MntSweetProducts", "MntGoldProds")
purchase_channel_cols <- c("NumWebPurchases", "NumCatalogPurchases",
                            "NumStorePurchases")
# NumDealsPurchases is kept separate — it is a price-sensitivity proxy,
# not a fourth purchase channel.
camp_cols             <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
                            "AcceptedCmp4", "AcceptedCmp5")

# ── SECTION 1: Dataset overview ───────────────────────────────────────────────
cat("\n============================================================\n")
cat("SECTION 1: Dataset Overview\n")
cat("============================================================\n\n")

glimpse(customers)

customers %>% select(where(is.numeric)) %>% summary()

cat("\nResponse distribution:\n")
customers %>%
  count(Response) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# NOTE: ~14.9% Yes, ~85.1% No. Class imbalance.
# A model that always predicts "No" achieves 85% accuracy without learning anything.
# Use ROC-AUC or precision-recall as primary metrics — not raw accuracy.

# ── SECTION 2: Demographic distributions (univariate) ─────────────────────────
cat("\n============================================================\n")
cat("SECTION 2: Demographic Distributions (Univariate)\n")
cat("============================================================\n\n")

p_age <- ggplot(customers, aes(x = Age)) +
  geom_histogram(binwidth = 3, fill = "#3d6b1e", colour = "white") +
  labs(title = "Age distribution", x = "Age", y = "Count") +
  theme_minimal()

p_income <- ggplot(customers, aes(x = Income)) +
  geom_histogram(bins = 40, fill = "#3d6b1e", colour = "white") +
  scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
  labs(title = "Income distribution", x = "Income", y = "Count") +
  theme_minimal()

p_edu <- customers %>%
  count(Education) %>%
  ggplot(aes(x = Education, y = n)) +
  geom_col(fill = "#3d6b1e") +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  labs(title = "Education level", x = NULL, y = "Count") +
  theme_minimal()

p_kids <- customers %>%
  count(HasChildren) %>%
  mutate(label = if_else(HasChildren, "Has children", "No children"),
         pct   = n / sum(n) * 100) %>%
  ggplot(aes(x = label, y = pct)) +
  geom_col(fill = "#3d6b1e") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.4) +
  labs(title = "Children at home", x = NULL, y = "%") +
  theme_minimal()

print((p_age + p_income) / (p_edu + p_kids))

cat("\nAge summary:\n")
customers %>%
  summarise(mean = round(mean(Age), 1), median = median(Age), sd = round(sd(Age), 1)) %>%
  print()

cat("\nChildren breakdown:\n")
customers %>%
  count(HasChildren) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\nEducation counts:\n")
print(table(customers$Education))

# PATTERN: Customers skew middle-aged (peak ~45-55). Income is right-skewed.
# ~63% have children. Graduation is the most common education level (~50%).
# The "Under 35" age group is small — segment-level estimates for this group
# have wide uncertainty and should not be over-interpreted.

# ── SECTION 3: Spend distributions (univariate) ───────────────────────────────
cat("\n============================================================\n")
cat("SECTION 3: Spend Distributions (Univariate)\n")
cat("============================================================\n\n")

cat("TotalSpend summary statistics:\n")
customers %>%
  summarise(
    mean          = round(mean(TotalSpend)),
    median        = round(median(TotalSpend)),
    sd            = round(sd(TotalSpend)),
    min           = min(TotalSpend),
    max           = max(TotalSpend),
    pct_under_100 = round(mean(TotalSpend < 100) * 100, 1)
  ) %>%
  print()

# NOTE: Mean >> median — right-skewed distribution.
# Reporting only the mean would overstate typical customer spend.
# The app should display both where possible.

cat("\nSpend share by product category:\n")
spend_share <- customers %>%
  summarise(across(all_of(spend_cols), sum)) %>%
  pivot_longer(everything(), names_to = "category", values_to = "total") %>%
  mutate(
    pct      = total / sum(total) * 100,
    category = str_remove(category, "Mnt") %>%
               str_replace("Products", "") %>%
               str_trim()
  ) %>%
  arrange(desc(pct))

print(spend_share)

# PATTERN: Wine (~50%) and Meat (~28%) together make up ~78% of all spend.
# Any visualisation treating all six categories equally is misleading.

ggplot(spend_share, aes(x = reorder(category, pct), y = pct)) +
  geom_col(fill = "#3d6b1e") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 60)) +
  labs(title = "Spend share by product category", x = NULL, y = "% of total spend") +
  theme_minimal()

customers %>%
  select(all_of(spend_cols)) %>%
  pivot_longer(everything(), names_to = "category", values_to = "amount") %>%
  mutate(category = str_remove(category, "Mnt")) %>%
  ggplot(aes(x = amount)) +
  geom_histogram(bins = 30, fill = "#3d6b1e", colour = "white") +
  facet_wrap(~category, scales = "free") +
  labs(title = "Spend per category — all right-skewed with near-zero mass",
       x = "Amount ($)", y = "Count") +
  theme_minimal()

# NOTE: Every category has a mass of near-zero values and a long right tail.
# Log-transforming spend columns may help linear models handle this shape —
# though whether it does should be tested empirically.

cat("\nMean and median spend per category:\n")
customers %>%
  summarise(across(all_of(spend_cols), list(mean = mean, median = median))) %>%
  pivot_longer(everything(),
               names_to  = c("category", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  mutate(category = str_remove(category, "Mnt")) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(desc(mean)) %>%
  print()

# ── SECTION 4: Spend vs demographics (bivariate) ──────────────────────────────
cat("\n============================================================\n")
cat("SECTION 4: Spend vs Demographics (Bivariate)\n")
cat("============================================================\n\n")

cat("Spend by children status:\n")
customers %>%
  group_by(HasChildren) %>%
  summarise(
    n            = n(),
    mean_spend   = round(mean(TotalSpend)),
    median_spend = round(median(TotalSpend))
  ) %>%
  print()

# PATTERN: No-children customers spend ~2.5x more on average.
# The ratio holds on median as well as mean, which suggests it is not driven
# by a small number of high-spending outliers. This is a descriptive finding —
# income differences between the groups may partly explain it.

customers %>%
  group_by(HasChildren) %>%
  summarise(across(all_of(spend_cols), mean)) %>%
  pivot_longer(-HasChildren, names_to = "category", values_to = "avg") %>%
  mutate(
    HasChildren = if_else(HasChildren, "Has children", "No children"),
    category    = str_remove(category, "Mnt")
  ) %>%
  ggplot(aes(x = category, y = avg, fill = HasChildren)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Has children" = "#a8c87a", "No children" = "#2d5016")) +
  labs(title = "Average spend per category: children vs no children",
       x = NULL, y = "Avg spend ($)", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

cat("\nAbsolute spend gaps (no children minus has children):\n")
customers %>%
  group_by(HasChildren) %>%
  summarise(across(all_of(spend_cols), mean)) %>%
  pivot_longer(-HasChildren, names_to = "category", values_to = "avg") %>%
  pivot_wider(names_from = HasChildren, values_from = avg) %>%
  mutate(gap = round(`FALSE` - `TRUE`),
         category = str_remove(category, "Mnt")) %>%
  arrange(desc(gap)) %>%
  print()

# PATTERN: Wine and Meat show the largest absolute gaps.
# Children appear associated with lower spend in premium discretionary categories.
# This is a correlation — income confounding is not ruled out here.

cat("\nSpend by education level:\n")
customers %>%
  group_by(Education) %>%
  summarise(n = n(), mean_spend = round(mean(TotalSpend)),
            median_spend = round(median(TotalSpend))) %>%
  print()

# NOTE: Higher education levels are associated with higher spend, but education
# correlates with income — this likely explains much of the spending difference.

ggplot(customers, aes(x = Income, y = TotalSpend, colour = HasChildren)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
  scale_y_continuous(labels = label_dollar()) +
  scale_colour_manual(values = c("TRUE" = "#a8c87a", "FALSE" = "#2d5016"),
                      labels = c("TRUE" = "Has children", "FALSE" = "No children")) +
  labs(title = "Income vs total spend, by children status",
       subtitle = "Parallel trend lines suggest family structure may be associated with spend\nbeyond income — but this plot does not control for other variables",
       x = "Annual income ($k)", y = "Total 2-year spend ($)", colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

# NOTE: Both groups show a positive income-spend slope.
# The no-children group shows a higher fitted line, suggesting family structure
# is associated with spend over and above income — though this plot alone cannot
# establish that relationship as independent.

# ── SECTION 5: Purchase channels and deal behavior ────────────────────────────
cat("\n============================================================\n")
cat("SECTION 5: Purchase Channels and Deal Behavior\n")
cat("============================================================\n\n")

# NumDealsPurchases is treated as a price-sensitivity indicator, not a channel.
# See 01_raw_eda.R A10 and 02_data_prep.R for the reasoning.

cat("Average purchases per channel (all customers):\n")
customers %>%
  summarise(across(all_of(purchase_channel_cols), mean)) %>%
  pivot_longer(everything(), names_to = "channel", values_to = "avg") %>%
  mutate(channel = str_remove(channel, "Num|Purchases")) %>%
  arrange(desc(avg)) %>%
  print()

cat("\nDeal purchase behavior:\n")
customers %>%
  summarise(
    avg_deal_purchases = round(mean(NumDealsPurchases), 2),
    avg_deals_pct      = round(mean(DealsPct) * 100, 1),
    pct_no_deals       = round(mean(NumDealsPurchases == 0) * 100, 1)
  ) %>%
  print()

# PATTERN: Store is the dominant channel (~5.8 avg/year). Web (~4.1) and
# Catalogue (~2.7) are secondary. Deal purchases are a separate behavior —
# ~X% of customers made zero deal purchases in the period.

cat("\nChannel usage by income tier:\n")
customers %>%
  group_by(IncomeTier) %>%
  summarise(
    across(all_of(purchase_channel_cols), mean),
    avg_DealsPct = mean(DealsPct)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print()

# PATTERN: Catalogue usage is strongly associated with income tier.
# Deal-purchase proportion tends to be lower for high-income customers —
# consistent with lower price sensitivity at higher income levels.
# These are descriptive averages; confounding by age or family structure is possible.

customers %>%
  group_by(IncomeTier) %>%
  summarise(across(all_of(purchase_channel_cols), mean)) %>%
  pivot_longer(-IncomeTier, names_to = "channel", values_to = "avg") %>%
  mutate(channel = str_remove(channel, "Num|Purchases")) %>%
  ggplot(aes(x = IncomeTier, y = avg, fill = IncomeTier)) +
  geom_col() +
  facet_wrap(~channel, scales = "free_y") +
  scale_fill_manual(values = c(Low = "#e8f0d0", Mid = "#3d6b1e", High = "#1a3009")) +
  labs(title = "Avg purchases per channel by income tier",
       x = NULL, y = "Avg purchases", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

ggplot(customers, aes(x = DealsPct, fill = IncomeTier)) +
  geom_histogram(bins = 20, position = "identity", alpha = 0.6) +
  scale_fill_manual(values = c(Low = "#e8f0d0", Mid = "#3d6b1e", High = "#1a3009")) +
  labs(title = "Deal purchase proportion by income tier",
       x = "Deal purchases / channel purchases", y = "Count") +
  theme_minimal() +
  theme(legend.position = "top")

# ── SECTION 6: Campaign response analysis ─────────────────────────────────────
cat("\n============================================================\n")
cat("SECTION 6: Campaign Response Analysis\n")
cat("============================================================\n\n")

cat("Campaign acceptance rates (%):\n")
campaign_rates <- customers %>%
  summarise(
    Cmp1     = mean(AcceptedCmp1),
    Cmp2     = mean(AcceptedCmp2),
    Cmp3     = mean(AcceptedCmp3),
    Cmp4     = mean(AcceptedCmp4),
    Cmp5     = mean(AcceptedCmp5),
    Response = mean(Response == "Yes")
  ) %>%
  pivot_longer(everything(), names_to = "campaign", values_to = "rate") %>%
  mutate(rate_pct = round(rate * 100, 1))

print(campaign_rates)

# PATTERN: Campaign 2 was accepted by only 1.3% vs 7-15% for all others.
# This is anomalously low. We do not have data on the offer, channel, or
# targeting used — this script cannot explain why. Present as an anomaly.

ggplot(campaign_rates, aes(x = campaign, y = rate_pct, fill = rate_pct < 4)) +
  geom_col() +
  geom_hline(yintercept = mean(campaign_rates$rate_pct),
             linetype = "dashed", colour = "#7a6a52") +
  scale_fill_manual(values = c("FALSE" = "#3d6b1e", "TRUE" = "#8b2020"),
                    guide   = "none") +
  labs(title = "Acceptance rate per campaign",
       subtitle = "Red = far below average — reason unknown from this dataset",
       x = NULL, y = "Acceptance rate (%)") +
  theme_minimal()

# ── Recency and response ──────────────────────────────────────────────────────
cat("\nResponse rate by recency bucket:\n")
customers %>%
  mutate(RecencyBucket = cut(Recency,
                             breaks = c(-1, 25, 50, 75, 100),
                             labels = c("0-25 days", "26-50", "51-75", "76-100"))) %>%
  group_by(RecencyBucket) %>%
  summarise(
    response_rate = round(mean(Response == "Yes") * 100, 1),
    n             = n()
  ) %>%
  print()

# PATTERN: The most recently active customers (0-25 days) respond at ~26.5%;
# those inactive for 75+ days respond at ~7.4% — a ~3.6x difference.
# All four recency buckets have n > 400, so this pattern is not an artefact
# of small sample sizes. No formal hypothesis test has been run.

# ── Campaign history and response ─────────────────────────────────────────────
cat("\nResponse rate by number of prior campaign accepts:\n")
customers %>%
  group_by(CampaignHistory) %>%
  summarise(response_rate = round(mean(Response == "Yes") * 100, 1), n = n()) %>%
  print()

customers %>%
  group_by(CampaignHistory) %>%
  summarise(response_rate = mean(Response == "Yes") * 100, n = n()) %>%
  ggplot(aes(x = factor(CampaignHistory), y = response_rate)) +
  geom_col(fill = "#3d6b1e") +
  geom_text(aes(label = paste0(round(response_rate, 1), "%")), vjust = -0.4) +
  labs(title = "Response rate by number of previous campaigns accepted",
       x = "Campaigns accepted (out of 5)", y = "Response rate (%)") +
  theme_minimal()

# PATTERN: CampaignHistory shows the highest Pearson correlation with Response
# among numeric features examined in Section 8.
# Customers with 2+ prior accepts respond at ~63% vs ~10.5% for those with 0.
# NOTE: Groups with 4-5 prior accepts have small n (< 50 each) — individual
# rates for those groups are directionally credible but imprecise. The combined
# 2+ group (adequate n) is used for headline comparisons.
#
# Recency and CampaignHistory are both descriptively important for response.
# Recency shows a steep gradient across bucketed groups (above).
# CampaignHistory shows the highest linear correlation (Section 8).
# Both findings are complementary — neither cancels the other out.

# ── Education and response ────────────────────────────────────────────────────
cat("\nResponse rate by education level:\n")
customers %>%
  group_by(Education) %>%
  summarise(response_rate = round(mean(Response == "Yes") * 100, 1), n = n()) %>%
  print()

# NOTE: "Basic" has ~54 customers — the ~3.7% rate is based on ~2 responses.
# Direction is credible; magnitude is not precise. PhD (n ~486) is more reliable.

cat("\nResponse rate by children status:\n")
customers %>%
  group_by(HasChildren) %>%
  summarise(
    response_rate = round(mean(Response == "Yes") * 100, 1),
    n             = n()
  ) %>%
  mutate(label = if_else(HasChildren, "Has children", "No children")) %>%
  print()

# ── SECTION 7: Tenure effect ──────────────────────────────────────────────────
cat("\n============================================================\n")
cat("SECTION 7: Tenure Effect\n")
cat("============================================================\n\n")

customers %>%
  mutate(TenureGroup = cut(Tenure,
                           breaks  = quantile(Tenure, probs = seq(0, 1, .25)),
                           labels  = c("Q1 (newest)", "Q2", "Q3", "Q4 (oldest)"),
                           include.lowest = TRUE)) %>%
  group_by(TenureGroup) %>%
  summarise(
    response_rate = round(mean(Response == "Yes") * 100, 1),
    avg_spend     = round(mean(TotalSpend)),
    n             = n()
  ) %>%
  print()

# PATTERN: Longest-tenure customers respond at ~26% vs ~8-9% for newest.
# Note: all customers in this dataset joined within ~1-2.5 years of the reference
# date — the tenure effect may be more pronounced with a longer observation window.

# ── SECTION 8: Correlations with Response ─────────────────────────────────────
cat("\n============================================================\n")
cat("SECTION 8: Feature Correlations with Response (Screening)\n")
cat("============================================================\n\n")

response_numeric <- customers %>%
  mutate(Response_01 = if_else(Response == "Yes", 1L, 0L))

cat("Pearson correlations with Response (sorted by absolute value):\n")
cors <- response_numeric %>%
  select(Response_01, Income, Age, Tenure, Recency, TotalSpend,
         TotalPurchases, SpendPerPurchase, DealsPct, CampaignHistory,
         NumWebVisitsMonth, all_of(spend_cols)) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  select(feature, Response_01) %>%
  filter(feature != "Response_01") %>%
  arrange(desc(abs(Response_01)))

print(cors, n = 20)

# PATTERNS (linear Pearson — non-linear associations are understated):
# 1. CampaignHistory  (~0.30) — highest linear correlation
# 2. TotalSpend       (~0.27)
# 3. MntWines         (~0.25)
# 4. Recency          (~-0.20) — negative: more days since last purchase = lower rate
# 5. Tenure           (~0.19)
#
# This is a screening table, not a feature importance ranking. A model using
# all features together will behave differently than these pairwise correlations
# suggest, because features are correlated with each other.

cat("\nDealsPct correlation with Response:\n")
cors %>% filter(feature == "DealsPct") %>% print()
# PATTERN: Negative correlation (~-0.15) — deal-focused customers are less likely
# to respond to direct campaigns. Consistent with price-sensitivity framing.

# ── SECTION 9: Responder profiling ────────────────────────────────────────────
cat("\n============================================================\n")
cat("SECTION 9: Responder vs Non-responder Profile\n")
cat("============================================================\n\n")

profile <- customers %>%
  group_by(Response) %>%
  summarise(
    n                  = n(),
    avg_income         = round(mean(Income)),
    avg_age            = round(mean(Age), 1),
    avg_tenure_days    = round(mean(Tenure)),
    avg_recency        = round(mean(Recency), 1),
    avg_total_spend    = round(mean(TotalSpend)),
    pct_has_children   = round(mean(HasChildren) * 100, 1),
    avg_camp_history   = round(mean(CampaignHistory), 2),
    pct_high_education = round(mean(Education >= "Master") * 100, 1)
  )

print(t(profile))

# PATTERN: Responders tend to have higher income, lower recency (more recently
# active), higher spend, more campaign history, fewer children, and longer tenure.
# Differences are consistent across all dimensions. These are group averages —
# the groups overlap substantially, and no single variable cleanly separates them.

cat("\n============================================================\n")
cat("EDA COMPLETE\n")
cat("============================================================\n")
cat("Key patterns (descriptive — not causal):\n")
cat("1.  Wine (~50%) + Meat (~28%) = ~78% of total spend\n")
cat("2.  No-children customers spend ~2.5x more (holds on mean AND median)\n")
cat("3.  Recency: 26.5% response (0-25 days) vs 7.4% (75+ days) — 3.6x gap\n")
cat("4.  CampaignHistory 2+: ~63% response vs ~10.5% for 0 prior accepts\n")
cat("5.  Catalogue channel is strongly associated with income tier\n")
cat("6.  DealsPct is negatively associated with response (~-0.15 correlation)\n")
cat("7.  Campaign 2: ~1.3% acceptance — anomalously low, cause unknown\n")
cat("8.  PhD: ~20.8% response; Basic: ~3.7% (Basic n=54 — imprecise estimate)\n")
cat("9.  Tenure Q4 vs Q1: ~26% vs ~9% response rate\n")
cat("10. CampaignHistory has the highest linear correlation with Response among",
    "features screened\n")
cat("11. Class imbalance: 14.9% Yes — use AUC, not accuracy, for ML\n")
