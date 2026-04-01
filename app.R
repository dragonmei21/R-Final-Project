# ============================================================================
# app.R — FreshMart Customer Intelligence
# ESADE MIBA · Data Analytics with R · 2025
#
# Research question:
#   What makes a customer valuable — and are we targeting the right ones?
#
# Tabs:
#   The Store       — dataset context and cleaning notes
#   Our Shoppers    — demographic explorer with reactive filters
#   Basket Analysis — product mix and channel preferences
#   Promotions      — acceptance rates, predictors, responder profiles
#   Next Purchase   — ML predictor
#
# Pre-requisite: run 02_data_prep.R once to generate DATA/customers_clean.rds
# ============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(shinycssloaders)
library(DT)
library(randomForest)
library(pROC)

# ── Column name vectors (used throughout) ─────────────────────────────────────
spend_cols   <- c("MntWines", "MntFruits", "MntMeatProducts",
                  "MntFishProducts", "MntSweetProducts", "MntGoldProds")
# NumDealsPurchases is excluded — it is a price-sensitivity proxy, not a channel.
# See 02_data_prep.R for the full reasoning.
purchase_channel_cols <- c("NumWebPurchases", "NumCatalogPurchases",
                            "NumStorePurchases")
camp_cols    <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
                  "AcceptedCmp4", "AcceptedCmp5")

# ── Shared helpers ────────────────────────────────────────────────────────────

# KPI box HTML generator
kpi_box <- function(value, label, colour = "#2d5016") {
  div(class = "kpi-box",
      div(class = "kpi-value", style = paste0("color:", colour, ";"), value),
      div(class = "kpi-label", label))
}

# Consistent ggplot theme for all app charts — store palette
theme_app <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "#ffffff", colour = NA),
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#ede8df", linewidth = 0.5),
      axis.text        = element_text(colour = "#7a6a52", size = 10,
                                      family = "Lato"),
      axis.title       = element_text(colour = "#3d2b1a", size = 11,
                                      family = "Lato"),
      plot.margin      = margin(12, 16, 12, 16),
      legend.text      = element_text(colour = "#7a6a52", family = "Lato",
                                      size = 10),
      legend.title     = element_text(colour = "#3d2b1a", family = "Lato",
                                      size = 10)
    )
}

# Colour ramp for charts (store green palette)
GREEN_RAMP <- c("#e8f0d0", "#a8c87a", "#3d6b1e", "#2d5016", "#1a3009")

# ── Tab 1: plot functions ──────────────────────────────────────────────────────

plot_spend_by_edu <- function(df) {
  df %>%
    group_by(Education) %>%
    summarise(
      avg_spend = mean(TotalSpend),
      n         = n(),
      .groups   = "drop"
    ) %>%
    ggplot(aes(x = Education, y = avg_spend,
               fill   = as.numeric(Education),
               text   = paste0(Education, "\nAvg spend: $", round(avg_spend),
                               "\nn = ", n))) +
    geom_col() +
    geom_text(aes(label = paste0("$", round(avg_spend))),
              vjust = -0.4, size = 3.2, colour = "#3d2b1a") +
    scale_fill_gradient(low = "#e8f0d0", high = "#2d5016", guide = "none") +
    scale_y_continuous(labels = label_dollar(),
                       expand = expansion(mult = c(0, .15))) +
    labs(x = NULL, y = "Avg 2-year spend ($)") +
    theme_app()
}

plot_income_spend <- function(df) {
  df %>%
    mutate(children_label = if_else(as.logical(HasChildren),
                                    "Has children", "No children")) %>%
    ggplot(aes(x = Income, y = TotalSpend,
               colour = children_label,
               text   = paste0("Income: $", scales::comma(Income),
                               "\nSpend: $", TotalSpend,
                               "\nAge: ", Age,
                               "\nEducation: ", Education))) +
    geom_point(alpha = 0.35, size = 1.8) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
    scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
    scale_y_continuous(labels = label_dollar()) +
    scale_colour_manual(
      values = c("Has children" = "#a8c87a", "No children" = "#2d5016")
    ) +
    labs(x = "Annual income", y = "Total 2-year spend ($)", colour = NULL) +
    theme_app() +
    theme(legend.position = "top")
}

plot_age_hist <- function(df) {
  ggplot(df, aes(x = Age)) +
    geom_histogram(binwidth = 4, fill = "#3d6b1e", colour = "white") +
    geom_vline(xintercept = median(df$Age),
               linetype = "dashed", colour = "#7a6a52") +
    annotate("text", x = median(df$Age) + 1, y = Inf,
             label = paste0("median: ", round(median(df$Age))),
             hjust = 0, vjust = 1.5, size = 3, colour = "#7a6a52") +
    labs(x = "Age", y = "Count") +
    theme_app()
}

plot_marital <- function(df) {
  df %>%
    count(MaritalSimple) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ggplot(aes(x = reorder(MaritalSimple, n), y = pct,
               text = paste0(MaritalSimple, ": ", round(pct, 1), "%\nn = ", n))) +
    geom_col(fill = "#3d6b1e") +
    geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, .2))) +
    labs(x = NULL, y = "% of customers") +
    theme_app()
}

# ── Tab 2: plot functions ──────────────────────────────────────────────────────

plot_spend_share <- function(df) {
  df %>%
    summarise(across(all_of(spend_cols), sum)) %>%
    pivot_longer(everything(), names_to = "cat", values_to = "total") %>%
    mutate(
      pct       = total / sum(total) * 100,
      cat       = str_remove(cat, "Mnt") %>%
                  str_replace("Products", "") %>%
                  str_trim(),
      cat       = factor(cat, levels = cat[order(pct)]),
      # Use character string, not logical — ggplotly can't handle logical fills
      highlight = if_else(pct > 20, "major", "minor")
    ) %>%
    ggplot(aes(x = cat, y = pct,
               fill = highlight,
               text = paste0(cat, ": ", round(pct, 1), "%"))) +
    geom_col() +
    geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_fill_manual(values = c("minor" = "#a8c87a", "major" = "#2d5016"),
                      guide = "none") +
    scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
    labs(x = NULL, y = "% of total spend") +
    theme_app()
}

plot_spend_kids <- function(df) {
  df %>%
    filter(!is.na(HasChildren)) %>%
    mutate(HasChildren = as.logical(HasChildren)) %>%
    group_by(HasChildren) %>%
    summarise(across(all_of(spend_cols), mean), .groups = "drop") %>%
    pivot_longer(-HasChildren, names_to = "cat", values_to = "avg") %>%
    mutate(
      cat         = str_remove(cat, "Mnt") %>% str_replace("Products", "") %>% str_trim(),
      HasChildren = if_else(HasChildren, "Has children", "No children")
    ) %>%
    ggplot(aes(x = cat, y = avg, fill = HasChildren,
               text = paste0(HasChildren, "\n", cat, ": $", round(avg)))) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Has children" = "#a8c87a",
                                 "No children"  = "#2d5016")) +
    scale_y_continuous(labels = label_dollar()) +
    labs(x = NULL, y = "Avg spend ($)", fill = NULL) +
    theme_app() +
    theme(legend.position = "top")
}

plot_channels <- function(df) {
  df %>%
    filter(!is.na(IncomeTier)) %>%
    mutate(IncomeTier = factor(as.character(IncomeTier),
                               levels = c("Low", "Mid", "High"))) %>%
    group_by(IncomeTier) %>%
    summarise(across(all_of(purchase_channel_cols), mean), .groups = "drop") %>%
    filter(!is.na(IncomeTier)) %>%
    pivot_longer(-IncomeTier, names_to = "channel", values_to = "avg") %>%
    mutate(
      channel = str_remove(channel, "Num|Purchases") %>% str_trim()
    ) %>%
    ggplot(aes(x = channel, y = avg, fill = IncomeTier,
               text = paste0(IncomeTier, " income\n",
                             channel, ": ", round(avg, 1), " purchases/yr"))) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(Low = "#e8f0d0", Mid = "#3d6b1e", High = "#1a3009")) +
    labs(x = NULL, y = "Avg purchases per year", fill = "Income tier") +
    theme_app() +
    theme(legend.position = "top")
}

# ── Tab 3: plot functions ──────────────────────────────────────────────────────

plot_campaign_rates <- function(df) {
  rates <- df %>%
    summarise(across(all_of(camp_cols), ~ mean(.x) * 100),
              Response = mean(Response == "Yes") * 100) %>%
    pivot_longer(everything(), names_to = "campaign", values_to = "rate") %>%
    mutate(
      label    = c("Cmp 1", "Cmp 2", "Cmp 3", "Cmp 4", "Cmp 5", "Final"),
      is_low   = rate < 4,
      is_final = campaign == "Response"
    )

  overall_avg <- mean(rates$rate)

  ggplot(rates, aes(x = label, y = rate,
                    fill  = case_when(is_final ~ "final",
                                      is_low   ~ "low",
                                      TRUE     ~ "normal"),
                    text  = paste0(label, ": ", round(rate, 1), "% accepted"))) +
    geom_col() +
    geom_hline(yintercept = overall_avg,
               linetype = "dashed", colour = "#7a6a52", linewidth = 0.7) +
    annotate("text", x = 0.6, y = overall_avg + 0.4,
             label = "avg", size = 3, colour = "#7a6a52", hjust = 0) +
    scale_fill_manual(
      values = c(final = "#2d5016", low = "#8b2020", normal = "#3d6b1e"),
      guide  = "none"
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, .15))) +
    labs(x = NULL, y = "Acceptance rate") +
    theme_app()
}

plot_responder_profile <- function(df) {
  df %>%
    group_by(Response) %>%
    summarise(
      `Avg income ($k)`    = mean(Income) / 1000,
      `Avg spend ($)`      = mean(TotalSpend),
      `Avg recency (days)` = mean(Recency),
      `Avg tenure (days)`  = mean(Tenure),
      `Avg camp. history`  = mean(CampaignHistory),
      .groups = "drop"
    ) %>%
    pivot_longer(-Response, names_to = "metric", values_to = "value") %>%
    ggplot(aes(x = metric, y = value, fill = Response,
               text = paste0(Response, "\n", metric, ": ", round(value, 1)))) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_fill_manual(values = c(Yes = "#2d5016", No = "#e8f0d0")) +
    labs(x = NULL, y = NULL, fill = "Responded?") +
    theme_app() +
    theme(legend.position = "top")
}

plot_recency_response <- function(df) {
  df %>%
    mutate(RecencyBucket = cut(Recency,
                               breaks = c(-1, 25, 50, 75, 100),
                               labels = c("0\u201325", "26\u201350", "51\u201375", "76\u2013100"))) %>%
    group_by(RecencyBucket) %>%
    summarise(
      rate = mean(Response == "Yes") * 100,
      n    = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = RecencyBucket, y = rate,
               text = paste0("Recency: ", RecencyBucket, " days\n",
                             "Response rate: ", round(rate, 1), "%\n",
                             "n = ", n))) +
    geom_col(fill = "#3d6b1e") +
    geom_text(aes(label = paste0(round(rate, 1), "%")), vjust = -0.4, size = 3.2) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, .18))) +
    labs(x = "Days since last purchase", y = "Response rate (%)") +
    theme_app()
}

plot_camp_history_response <- function(df) {
  df %>%
    group_by(CampaignHistory) %>%
    summarise(
      rate = mean(Response == "Yes") * 100,
      n    = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = factor(CampaignHistory), y = rate,
               text = paste0("Previous accepts: ", CampaignHistory,
                             "\nResponse rate: ", round(rate, 1), "%\n",
                             "n = ", n))) +
    geom_col(fill = "#2d5016") +
    geom_text(aes(label = paste0(round(rate, 1), "%")), vjust = -0.4, size = 3.2) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, .18))) +
    labs(x = "Campaigns accepted previously (out of 5)",
         y = "Response rate (%)") +
    theme_app()
}

# ── Load clean data ───────────────────────────────────────────────────────────
customers     <- readRDS("./DATA/customers_clean.rds")
lr_model      <- readRDS("./DATA/lr_model.rds")
rf_model      <- readRDS("./DATA/rf_model.rds")
model_scaler  <- readRDS("./DATA/model_scaler.rds")
model_metrics <- readRDS("./DATA/model_metrics.rds")
feature_meta  <- readRDS("./DATA/feature_meta.rds")

# ── ML: prediction helper ─────────────────────────────────────────────────────
predict_response <- function(feature_values) {
  # feature_values: named list of raw user inputs (Income, Age, Tenure, etc.)
  # Applies the same feature engineering as ml_model.R before predicting.

  # Feature engineering — must match ml_model.R exactly
  fv <- feature_values
  fv$log_Income                <- log(fv$Income + 1)
  fv$CampaignHistory_x_Recency <- fv$CampaignHistory * fv$Recency

  LR_FEATURES <- model_scaler$lr_features   # order stored in the scaler
  input_df    <- as.data.frame(fv)[LR_FEATURES]

  # Scale for logistic regression using training-set parameters
  input_sc          <- as.data.frame(
    sweep(sweep(input_df, 2, model_scaler$means, "-"),
          2, model_scaler$sds, "/")
  )
  input_sc$Response <- 0L   # dummy column required by formula-based glm predict

  lr_prob <- as.numeric(predict(lr_model, newdata = input_sc, type = "response"))
  rf_prob <- as.numeric(predict(rf_model, newdata = input_df, type = "prob")[, "Yes"])

  list(
    lr_prob  = round(lr_prob * 100, 1),
    rf_prob  = round(rf_prob * 100, 1),
    lr_label = if (lr_prob >= 0.3) "Likely to respond" else "Unlikely to respond",
    rf_label = if (rf_prob >= 0.3) "Likely to respond" else "Unlikely to respond"
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    style = "font-family:'Playfair Display',Georgia,serif;
             font-style:italic; color:#f5c842; font-size:1.2rem;",
    "FreshMart Analytics"
  ),
  theme = bs_theme(
    bootswatch      = "flatly",
    primary         = "#2d5016",
    secondary       = "#7a6a52",
    success         = "#3d6b1e",
    danger          = "#8b2020",
    "navbar-bg"     = "#2d5016",
    "navbar-fg"     = "#f5c842",
    base_font       = font_google("Lato"),
    heading_font    = font_google("Playfair Display"),
    code_font       = font_google("IBM Plex Mono")
  ),
  header = tagList(
    includeCSS("www/custom.css"),
    tags$div(class = "awning-bar"),
    tags$div(
      class = "store-subbanner",
      "Customer Intelligence Dept. \u00b7 Loyalty Programme Analytics \u00b7 Est. 2012"
    )
  ),

  # ── The Store (About) ──────────────────────────────────────────────────────
  nav_panel(
    title = "The Store",

    div(
      style = "max-width: 800px; margin: 40px auto; padding: 0 24px;",

      tags$h1(class = "store-hero-title",
              "Welcome to our customer intelligence department"),
      tags$p(class = "store-hero-sub",
              "FreshMart loyalty programme \u00b7 2,216 members \u00b7 Est. 2012"),

      fluidRow(
        column(4, div(class = "kpi-box",
          tags$h5(style = "color:var(--green-dark);font-family:'Playfair Display',serif;
                           font-style:italic;font-size:1rem;margin-bottom:6px;",
                  "Our shoppers"),
          tags$p(style = "font-size:0.82rem;color:var(--brown-dark);",
                 "Who are our loyalty members? Age, income, education, and
                 family structure of the people who shop with us."))),
        column(4, div(class = "kpi-box",
          tags$h5(style = "color:var(--green-dark);font-family:'Playfair Display',serif;
                           font-style:italic;font-size:1rem;margin-bottom:6px;",
                  "Basket analysis"),
          tags$p(style = "font-size:0.82rem;color:var(--brown-dark);",
                 "What goes in the basket? Wine and meat dominate.
                 Families shop very differently from singles."))),
        column(4, div(class = "kpi-box",
          tags$h5(style = "color:var(--green-dark);font-family:'Playfair Display',serif;
                           font-style:italic;font-size:1rem;margin-bottom:6px;",
                  "Promotions"),
          tags$p(style = "font-size:0.82rem;color:var(--brown-dark);",
                 "Which offers worked? Promotion #2 failed.
                 Recency and loyalty history predict who redeems.")))
      ),

      div(class = "insight-box", style = "margin: 24px 0;",
          tags$b("About this data: "),
          "2,216 loyalty members after data cleaning. Covers 2 years of purchase
          history across wine, meat, fish, fruit, sweets, and gold products.
          One income entry (666,666) and three implausible birth years removed.
          24 missing income values filled with the store median."),

      div(class = "insight-box",
          tags$b("Note on class imbalance: "),
          "Only 14.9% of members redeemed our last promotion. A model that
          predicts \u2018won\u2019t redeem\u2019 for everyone achieves 85% accuracy without
          learning anything useful. We report AUC instead \u2014 see the Next Purchase
          tab for details."),

      tags$p(style = "font-size:0.72rem;color:var(--brown-mid);
                      margin-top:28px;font-family:'Lato',sans-serif;
                      letter-spacing:0.06em;text-transform:uppercase;",
             "ESADE MIBA \u00b7 Data Analytics with R \u00b7 2025")
    )
  ),

  # ── Our Shoppers (Customer Profiles) ───────────────────────────────────────
  nav_panel(
    "Our Shoppers",

    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        class = "sidebar-panel",

        tags$div(class = "sidebar-aisle-sign", "Filter by aisle"),

        tags$p(class = "filter-section-label", "Education level"),
        checkboxGroupInput(
          "filter_edu", label = NULL,
          choices  = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"),
          selected = c("Basic", "2n Cycle", "Graduation", "Master", "PhD")
        ),

        hr(),

        tags$p(class = "filter-section-label", "Children at home"),
        radioButtons(
          "filter_kids", label = NULL,
          choices  = c("All", "Yes", "No"),
          selected = "All"
        ),

        hr(),

        tags$p(class = "filter-section-label", "Household composition"),
        checkboxGroupInput(
          "filter_marital", label = NULL,
          choices  = c("Partnered", "Single", "Divorced", "Widow"),
          selected = c("Partnered", "Single", "Divorced", "Widow")
        ),

        hr(),

        actionButton("reset_tab1", "Clear filters \u2014 show all shoppers",
                     class = "btn-reset-receipt")
      ),

      div(
        class = "tab-content-area",

        # KPI row
        fluidRow(
          column(3, uiOutput("kpi_n_customers")),
          column(3, uiOutput("kpi_avg_income")),
          column(3, uiOutput("kpi_avg_age")),
          column(3, uiOutput("kpi_avg_spend"))
        ),

        # Charts 2x2 grid
        fluidRow(
          style = "margin-top:14px;",
          column(7,
            div(class = "shelf-card",
              tags$div(class = "aisle-sign", "Aisle 1 \u2014 spend by education"),
              withSpinner(plotlyOutput("plot_spend_edu", height = "240px"),
                          color = "#2d5016")
            )
          ),
          column(5,
            div(class = "shelf-card",
              tags$div(class = "aisle-sign", "Aisle 2 \u2014 income vs basket size"),
              withSpinner(plotlyOutput("plot_income_spend", height = "240px"),
                          color = "#2d5016")
            )
          )
        ),

        fluidRow(
          style = "margin-top:14px;",
          column(5,
            div(class = "shelf-card",
              tags$div(class = "aisle-sign", "Aisle 3 \u2014 shopper age profile"),
              withSpinner(plotlyOutput("plot_age", height = "220px"),
                          color = "#2d5016")
            )
          ),
          column(7,
            div(class = "shelf-card",
              tags$div(class = "aisle-sign", "Aisle 4 \u2014 household composition"),
              withSpinner(plotlyOutput("plot_marital", height = "220px"),
                          color = "#2d5016")
            )
          )
        ),

        uiOutput("insight_tab1")
      )
    )
  ),

  # ── Basket Analysis (Spending Patterns) ────────────────────────────────────
  nav_panel(
    "Basket Analysis",

    layout_sidebar(
      sidebar = sidebar(
        width = 200,
        class = "sidebar-panel",

        tags$div(class = "sidebar-aisle-sign", "Filter by aisle"),

        tags$p(class = "filter-section-label", "Income tier"),
        radioButtons("t2_income", NULL,
                     choices = c("All", "Low", "Mid", "High"), selected = "All"),

        hr(),

        tags$p(class = "filter-section-label", "Children at home"),
        radioButtons("t2_kids", NULL,
                     choices = c("All", "Yes", "No"), selected = "All"),

        hr(),

        div(class = "insight-box",
            tags$b("Key fact: "),
            "Wine alone accounts for 50% of all customer spend. ",
            "Meat adds another 28%. Toggle the filters to see how ",
            "this mix shifts across segments.")
      ),

      div(
        class = "tab-content-area",

        fluidRow(
          column(3, uiOutput("kpi_t2_spend")),
          column(3, uiOutput("kpi_t2_wine_pct")),
          column(3, uiOutput("kpi_t2_top_channel")),
          column(3, uiOutput("kpi_t2_deals_pct"))
        ),

        fluidRow(
          style = "margin-top:14px;",
          column(5,
            div(class = "shelf-card",
              tags$div(class = "aisle-sign", "Aisle 5 \u2014 what's in the basket"),
              withSpinner(plotlyOutput("plot_spend_share", height = "260px"),
                          color = "#2d5016")
            )
          ),
          column(7,
            div(class = "shelf-card",
              tags$div(class = "aisle-sign", "Aisle 6 \u2014 families vs singles"),
              withSpinner(plotlyOutput("plot_spend_kids", height = "260px"),
                          color = "#2d5016")
            ),
            div(class = "insight-box",
                "Child-free shoppers fill their basket with 2\u20133\u00d7 more wine and meat. ",
                "Families redirect that budget elsewhere. Our biggest spenders don\u2019t have kids.")
          )
        ),

        fluidRow(
          style = "margin-top:14px;",
          column(12,
            div(class = "shelf-card",
              tags$div(class = "aisle-sign", "Aisle 7 \u2014 how shoppers buy"),
              withSpinner(plotlyOutput("plot_channels", height = "220px"),
                          color = "#2d5016")
            ),
            div(class = "insight-box",
                tags$b("Manager\u2019s note: "),
                "High-income shoppers don\u2019t use our deals \u2014 they buy at full ",
                "price through catalogue and in-store. Targeting them with discounts wastes ",
                "the promotions budget.")
          )
        )
      )
    )
  ),

  # ── Promotions (Campaign Performance) ─────────────────────────────────────
  nav_panel(
    "Promotions",

    div(
      class = "tab-content-area",

      fluidRow(
        column(3, uiOutput("kpi_overall_response")),
        column(3, uiOutput("kpi_best_campaign")),
        column(3, uiOutput("kpi_worst_campaign")),
        column(3, uiOutput("kpi_top_predictor"))
      ),

      fluidRow(
        style = "margin-top:16px;",
        column(6,
          div(class = "shelf-card",
            tags$div(class = "aisle-sign", "Promotions board"),
            withSpinner(plotlyOutput("plot_camp_rates", height = "240px"),
                        color = "#2d5016")
          ),
          div(class = "warning-box",
              tags$b("Promotion #2: "),
              "Only 1.3% of shoppers redeemed this offer \u2014 far below our ",
              "7\u201315% average. This promotion\u2019s targeting or message needs ",
              "a full review before it is run again.")
        ),
        column(6,
          div(class = "shelf-card",
            tags$div(class = "aisle-sign", "Who redeems offers"),
            withSpinner(plotlyOutput("plot_responder_profile", height = "240px"),
                        color = "#2d5016")
          )
        )
      ),

      fluidRow(
        style = "margin-top:16px;",
        column(6,
          div(class = "shelf-card",
            tags$div(class = "aisle-sign", "Last visit timing"),
            withSpinner(plotlyOutput("plot_recency", height = "220px"),
                        color = "#2d5016")
          ),
          div(class = "insight-box",
              "Shoppers who visited in the last 25 days redeem offers at 26.5%. ",
              "Wait 75 days and that drops to 7.4%. Strike while the basket is warm.")
        ),
        column(6,
          div(class = "shelf-card",
            tags$div(class = "aisle-sign", "Loyalty to promotions"),
            withSpinner(plotlyOutput("plot_camp_history", height = "220px"),
                        color = "#2d5016")
          ),
          div(class = "insight-box",
              "A shopper who accepted 2 or more of our previous promotions redeems the next ",
              "one at 63% \u2014 four times the average. These are our best promotion targets.")
        )
      )
    )
  ),

  # ── Next Purchase (ML Predictor) ───────────────────────────────────────────
  nav_panel(
    "Next Purchase",

    div(
      class = "tab-content-area",

      # ── Model evaluation KPIs ────────────────────────────────────────────────
      fluidRow(
        column(3, div(class = "kpi-box",
          div(class = "kpi-value", textOutput("kpi_ml_auc", inline = TRUE)),
          div(class = "kpi-label", "AUC — both models (test set)"))),
        column(3, div(class = "kpi-box",
          div(class = "kpi-value", "0.875"),
          div(class = "kpi-label", "LR cross-validated AUC (train)"))),
        column(3, div(class = "kpi-box",
          div(class = "kpi-value", "14.9%"),
          div(class = "kpi-label", "baseline — always predict No"))),
        column(3, div(class = "kpi-box",
          div(class = "kpi-value", textOutput("kpi_ml_n", inline = TRUE)),
          div(class = "kpi-label", "customers used to train")))
      ),

      div(class = "insight-box", style = "margin: 12px 0;",
          tags$b("Why AUC, not accuracy: "),
          "85% of customers said No to the last campaign. A model that always ",
          "predicts No achieves 85% accuracy \u2014 without learning anything. ",
          "AUC of 0.857 means: if you randomly pick one responder and one ",
          "non-responder, the model ranks the responder higher 85.7% of the time."),

      tags$hr(),

      fluidRow(
        # ── Left: customer profile inputs ─────────────────────────────────────
        column(4,
          tags$h5(style = "font-weight:500; margin-bottom:14px;",
                  "Enter customer profile"),

          sliderInput("ml_campaign", "Previous campaigns accepted",
                      min = 0, max = 4, value = 0, step = 1),
          sliderInput("ml_recency", "Days since last purchase (Recency)",
                      min = 0, max = 99, value = 49, step = 1),
          sliderInput("ml_tenure", "Days as a customer (Tenure)",
                      min = 186, max = 885, value = 542, step = 1),
          radioButtons("ml_children", "Children at home",
                       choices = c("Yes" = 1, "No" = 0), selected = 1,
                       inline = TRUE),
          sliderInput("ml_income", "Annual income ($)",
                      min = 5000, max = 160000, value = 52000, step = 1000),
          sliderInput("ml_spend", "Total 2-year spend ($)",
                      min = 5, max = 2525, value = 605, step = 5),
          sliderInput("ml_education", "Education level",
                      min = 0, max = 4, value = 2, step = 1),
          tags$small(style = "color:#6b7280; font-size:0.75rem;",
                     "0=Basic  1=2n Cycle  2=Graduation  3=Master  4=PhD"),
          sliderInput("ml_web_visits", "Web visits per month",
                      min = 0, max = 20, value = 5, step = 1),
          sliderInput("ml_deals_pct", "% of purchases via deals",
                      min = 0, max = 1, value = 0.2, step = 0.05),

          actionButton("ml_reset", "Reset to average customer",
                       class = "btn btn-outline-secondary btn-sm w-100 mt-2")
        ),

        # ── Right: prediction output ───────────────────────────────────────────
        column(8,

          fluidRow(
            column(6,
              tags$h6("Logistic Regression", style = "font-weight:500;"),
              withSpinner(plotlyOutput("ml_gauge_lr", height = "200px"),
                          color = "#2d5016")
            ),
            column(6,
              tags$h6("Random Forest", style = "font-weight:500;"),
              withSpinner(plotlyOutput("ml_gauge_rf", height = "200px"),
                          color = "#2d5016")
            )
          ),

          div(style = "margin: 12px 0;",
              tags$h6("Decision threshold", style = "font-weight:500;"),
              sliderInput("ml_threshold", label = NULL,
                          min = 0.10, max = 0.80, value = 0.50, step = 0.05,
                          width = "100%"),
              uiOutput("threshold_explanation")
          ),

          tags$h6("What\u2019s driving this prediction?",
                  style = "font-weight:500; margin-top:10px;"),
          tags$p(class = "chart-subtitle",
            "Logistic regression coefficients \u2014 bar length = importance, ",
            "colour = positive (blue) or negative (red) effect on response probability."),
          withSpinner(plotlyOutput("ml_importance", height = "280px"),
                      color = "#2d5016"),

          div(class = "insight-box", style = "margin-top:10px;",
              tags$b("Model comparison insight: "),
              "Logistic regression and random forest achieve identical AUC on the test set. ",
              "When a simpler model performs as well as a complex one, prefer the simpler model ",
              "\u2014 it is more interpretable and less likely to overfit. ",
              "The linear relationships in this dataset mean additional tree complexity adds no value.")
        )
      ),

      tags$hr(style = "margin:20px 0;"),

      # ── ROC curve + confusion matrix ──────────────────────────────────────────
      fluidRow(
        column(6,
          tags$h5("ROC curve \u2014 both models", style = "font-weight:500;"),
          tags$p(class = "chart-subtitle",
            "The closer the curve is to the top-left corner, the better. ",
            "The dashed diagonal is a random classifier (AUC = 0.5)."),
          withSpinner(plotlyOutput("ml_roc", height = "300px"), color = "#2d5016")
        ),
        column(6,
          tags$h5("Confusion matrix", style = "font-weight:500;"),
          tags$p(class = "chart-subtitle",
            "Logistic regression at the selected threshold. ",
            "True Positives (top-left) are correctly identified responders."),
          withSpinner(plotlyOutput("ml_confusion", height = "300px"), color = "#2d5016")
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Tab 1: filtered data ───────────────────────────────────────────────────
  tab1_data <- reactive({
    df <- customers

    if (length(input$filter_edu) > 0) {
      df <- df %>% filter(as.character(Education) %in% input$filter_edu)
    }

    if (input$filter_kids != "All") {
      has_kids <- input$filter_kids == "Yes"
      df <- df %>% filter(HasChildren == has_kids)
    }

    if (length(input$filter_marital) > 0) {
      df <- df %>% filter(as.character(MaritalSimple) %in% input$filter_marital)
    }

    df
  })

  # Tab 1: KPI boxes
  output$kpi_n_customers <- renderUI({
    kpi_box(scales::comma(nrow(tab1_data())), "loyalty members")
  })
  output$kpi_avg_income <- renderUI({
    kpi_box(paste0("$", round(mean(tab1_data()$Income) / 1000, 1), "k"),
            "avg household income")
  })
  output$kpi_avg_age <- renderUI({
    kpi_box(round(mean(tab1_data()$Age)), "avg shopper age")
  })
  output$kpi_avg_spend <- renderUI({
    kpi_box(paste0("$", round(mean(tab1_data()$TotalSpend))),
            "avg spend in 2 years")
  })

  # Tab 1: charts
  output$plot_spend_edu <- renderPlotly({
    tryCatch({
      df <- tab1_data()
      validate(need(nrow(df) >= 5,
                    "Not enough data for current selection. Broaden your filters."))
      ggplotly(plot_spend_by_edu(df), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })

  output$plot_income_spend <- renderPlotly({
    tryCatch({
      df <- tab1_data()
      validate(need(nrow(df) >= 5,
                    "Not enough data for current selection. Broaden your filters."))
      ggplotly(plot_income_spend(df), tooltip = "text") %>%
        layout(legend = list(orientation = "h", y = 1.1)) %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })

  output$plot_age <- renderPlotly({
    tryCatch({
      df <- tab1_data()
      validate(need(nrow(df) >= 5,
                    "Not enough data for current selection. Broaden your filters."))
      ggplotly(plot_age_hist(df)) %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })

  output$plot_marital <- renderPlotly({
    tryCatch({
      df <- tab1_data()
      validate(need(nrow(df) >= 5,
                    "Not enough data for current selection. Broaden your filters."))
      ggplotly(plot_marital(df), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })

  # Tab 1: dynamic insight box
  output$insight_tab1 <- renderUI({
    df  <- tab1_data()
    n   <- nrow(df)

    if (n == 0) return(div(class = "insight-box", "No customers match the current filters."))

    pct_no_kids      <- round(mean(!df$HasChildren) * 100)
    avg_spend_nokids <- if (any(!df$HasChildren)) round(mean(df$TotalSpend[!df$HasChildren])) else NA
    avg_spend_kids   <- if (any(df$HasChildren))  round(mean(df$TotalSpend[df$HasChildren]))  else NA

    spend_text <- if (!is.na(avg_spend_nokids) && !is.na(avg_spend_kids)) {
      paste0(
        "Today\u2019s special: ", scales::comma(n), " shoppers in view. ",
        pct_no_kids, "% have no children at home. ",
        "Child-free members spend $", avg_spend_nokids,
        " on average vs $", avg_spend_kids,
        " for families \u2014 a difference of $",
        avg_spend_nokids - avg_spend_kids, "."
      )
    } else {
      "Only one children-status group present in current selection."
    }

    div(class = "insight-box", spend_text)
  })

  # Tab 1: reset button
  observeEvent(input$reset_tab1, {
    updateCheckboxGroupInput(session, "filter_edu",
      selected = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"))
    updateRadioButtons(session, "filter_kids", selected = "All")
    updateCheckboxGroupInput(session, "filter_marital",
      selected = c("Partnered", "Single", "Divorced", "Widow"))
  })

  # ── Tab 2: filtered data ───────────────────────────────────────────────────
  tab2_data <- reactive({
    df <- customers
    if (input$t2_income != "All")
      df <- df %>% filter(as.character(IncomeTier) == input$t2_income)
    if (input$t2_kids != "All")
      df <- df %>% filter(HasChildren == (input$t2_kids == "Yes"))
    df
  })

  # Tab 2: KPI boxes
  output$kpi_t2_spend <- renderUI({
    kpi_box(paste0("$", round(mean(tab2_data()$TotalSpend))), "avg spend in 2 years")
  })
  output$kpi_t2_wine_pct <- renderUI({
    pct <- round(sum(tab2_data()$MntWines) /
                 sum(tab2_data()$TotalSpend) * 100)
    kpi_box(paste0(pct, "%"), "of basket is wine")
  })
  output$kpi_t2_top_channel <- renderUI({
    means <- tab2_data() %>%
      summarise(
        Web     = mean(NumWebPurchases),
        Catalog = mean(NumCatalogPurchases),
        Store   = mean(NumStorePurchases),
        Deals   = mean(NumDealsPurchases)
      )
    top <- names(which.max(unlist(means)))
    kpi_box(top, "favourite way to shop")
  })
  output$kpi_t2_deals_pct <- renderUI({
    pct <- round(mean(tab2_data()$DealsPct) * 100)
    kpi_box(paste0(pct, "%"), "of purchases use a deal")
  })

  # Tab 2: charts
  output$plot_spend_share <- renderPlotly({
    tryCatch({
      req(nrow(tab2_data()) > 0)
      ggplotly(plot_spend_share(tab2_data()), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_spend_kids <- renderPlotly({
    tryCatch({
      req(nrow(tab2_data()) > 0)
      ggplotly(plot_spend_kids(tab2_data()), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_channels <- renderPlotly({
    tryCatch({
      req(nrow(tab2_data()) > 0)
      ggplotly(plot_channels(tab2_data()), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })

  # ── Tab 3: outputs (uses full customers dataset — no filter on this tab) ───
  output$kpi_overall_response <- renderUI({
    rate <- round(mean(customers$Response == "Yes") * 100, 1)
    kpi_box(paste0(rate, "%"), "redeemed our last offer")
  })
  output$kpi_best_campaign <- renderUI({
    kpi_box("Campaign 4/5", "best offer \u2014 7.5% redemption")
  })
  output$kpi_worst_campaign <- renderUI({
    kpi_box("Campaign 2", "worst offer \u2014 needs review", colour = "#8b2020")
  })
  output$kpi_top_predictor <- renderUI({
    kpi_box("Camp. history", "top predictor of redemption")
  })

  output$plot_camp_rates <- renderPlotly({
    tryCatch({
      ggplotly(plot_campaign_rates(customers), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_responder_profile <- renderPlotly({
    tryCatch({
      ggplotly(plot_responder_profile(customers), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_recency <- renderPlotly({
    tryCatch({
      ggplotly(plot_recency_response(customers), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_camp_history <- renderPlotly({
    tryCatch({
      ggplotly(plot_camp_history_response(customers), tooltip = "text") %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })

  # ── Tab 4: ML predictor ───────────────────────────────────────────────────────

  # Static KPIs from saved model artifacts
  output$kpi_ml_auc <- renderText({
    round(model_metrics$lr_auc, 3)
  })
  output$kpi_ml_n <- renderText({
    scales::comma(model_metrics$n_train)
  })

  # Collect slider/radio inputs into a feature list
  ml_inputs <- reactive({
    list(
      Income            = input$ml_income,
      Age               = 46,    # fixed at training-set mean — not exposed as slider
      Tenure            = input$ml_tenure,
      Recency           = input$ml_recency,
      TotalSpend        = input$ml_spend,
      CampaignHistory   = input$ml_campaign,
      HasChildren       = as.integer(input$ml_children),
      SpendPerPurchase  = if (input$ml_spend > 0) input$ml_spend / 15 else 0,
      DealsPct          = input$ml_deals_pct,
      NumWebVisitsMonth = input$ml_web_visits,
      Education_n       = input$ml_education
    )
  })

  ml_pred <- reactive({
    predict_response(ml_inputs())
  })

  # Gauge chart helper
  make_gauge <- function(prob, label, colour, threshold_pct) {
    plotly::plot_ly(
      type  = "indicator",
      mode  = "gauge+number+delta",
      value = prob,
      number = list(suffix = "%", font = list(size = 32)),
      delta  = list(
        reference  = 14.9,
        increasing = list(color = "#16a34a"),
        decreasing = list(color = "#dc2626")
      ),
      gauge = list(
        axis  = list(range = list(0, 100), ticksuffix = "%"),
        bar   = list(color = colour),
        steps = list(
          list(range = c(0,  30), color = "#f3f4f6"),
          list(range = c(30, 60), color = "#dbeafe"),
          list(range = c(60, 100), color = "#bfdbfe")
        ),
        threshold = list(
          line      = list(color = "#2d5016", width = 3),
          thickness = 0.8,
          value     = threshold_pct
        )
      ),
      title = list(text = label, font = list(size = 13))
    ) %>%
      layout(margin = list(t = 40, b = 10, l = 20, r = 20)) %>%
      config(displayModeBar = FALSE)
  }

  output$ml_gauge_lr <- renderPlotly({
    make_gauge(ml_pred()$lr_prob, ml_pred()$lr_label,
               "#2d5016", input$ml_threshold * 100)
  })
  output$ml_gauge_rf <- renderPlotly({
    make_gauge(ml_pred()$rf_prob, ml_pred()$rf_label,
               "#3d6b1e", input$ml_threshold * 100)
  })

  # Threshold explanation text
  output$threshold_explanation <- renderUI({
    t_val <- input$ml_threshold
    # Find the ROC point closest to this threshold
    lr_roc_pts <- model_metrics$roc_df %>%
      filter(model == "Logistic Regression", is.finite(threshold)) %>%
      mutate(dist = abs(threshold - t_val)) %>%
      slice_min(dist, n = 1)

    pct_flagged <- if (nrow(lr_roc_pts) > 0)
      round(lr_roc_pts$fpr[1] * 100 + (1 - lr_roc_pts$fpr[1]) *
              lr_roc_pts$tpr[1] * 100 * 0.149 / (100 * 0.149))
    else NA

    div(class = "chart-subtitle",
        sprintf(
          "At threshold %.0f%%: model labels a customer as a likely responder
           when its predicted probability exceeds this value.",
          t_val * 100
        ),
        tags$br(),
        "Lower threshold \u2192 catch more responders (higher recall) but
         contact more non-responders too (lower precision).
         Raise it to be more selective.")
  })

  # Feature importance bar chart
  output$ml_importance <- renderPlotly({
    imp <- model_metrics$lr_imp_df %>%
      arrange(desc(importance)) %>%
      mutate(
        feature = factor(feature, levels = rev(feature)),
        colour  = if_else(direction == "negative", "#ef4444", "#2d5016"),
        label   = case_when(
          feature == "CampaignHistory"   ~ "Campaign history",
          feature == "Tenure"            ~ "Tenure",
          feature == "Recency"           ~ "Recency (negative effect)",
          feature == "HasChildren"       ~ "Has children (negative effect)",
          feature == "DealsPct"          ~ "Deals %",
          feature == "Education_n"       ~ "Education level",
          feature == "NumWebVisitsMonth" ~ "Web visits / month",
          feature == "TotalSpend"        ~ "Total spend",
          feature == "Age"               ~ "Age",
          feature == "Income"            ~ "Income",
          feature == "SpendPerPurchase"  ~ "Spend per purchase",
          TRUE                           ~ as.character(feature)
        )
      )

    p <- ggplot(imp, aes(x = feature, y = importance, fill = colour,
                          text = paste0(label, "\nCoefficient magnitude: ",
                                        round(importance, 3)))) +
      geom_col() +
      coord_flip() +
      scale_x_discrete(labels = setNames(imp$label, as.character(imp$feature))) +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, .15))) +
      labs(x = NULL, y = "Coefficient magnitude (logistic regression)") +
      theme_app()

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  # ROC curve
  output$ml_roc <- renderPlotly({
    roc_df <- model_metrics$roc_df

    p <- ggplot(roc_df, aes(x = fpr, y = tpr, colour = model,
                             text = paste0(model,
                                           "\nFPR: ", round(fpr, 3),
                                           " | TPR: ", round(tpr, 3)))) +
      geom_line(linewidth = 1.0) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  colour = "#d1d5db") +
      annotate("text", x = 0.68, y = 0.10,
               label = paste0("LR AUC = ", round(model_metrics$lr_auc, 3),
                              "\nRF AUC = ", round(model_metrics$rf_auc, 3)),
               size = 3.5, colour = "#374151", hjust = 0) +
      scale_colour_manual(values = c("Logistic Regression" = "#2d5016",
                                     "Random Forest"       = "#a8c87a")) +
      labs(x = "False positive rate", y = "True positive rate", colour = NULL) +
      theme_app() +
      theme(legend.position = "top")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = 1.1)) %>%
      config(displayModeBar = FALSE)
  })

  # Confusion matrix — recomputed from saved test-set probabilities at selected threshold
  output$ml_confusion <- renderPlotly({
    t_val  <- input$ml_threshold
    probs  <- model_metrics$lr_test_probs
    y_true <- model_metrics$y_test
    y_pred <- as.integer(probs >= t_val)

    tp <- sum(y_pred == 1 & y_true == 1)
    fp <- sum(y_pred == 1 & y_true == 0)
    fn <- sum(y_pred == 0 & y_true == 1)
    tn <- sum(y_pred == 0 & y_true == 0)

    cm <- data.frame(
      Predicted = c("Yes", "Yes", "No",  "No"),
      Actual    = c("Yes", "No",  "Yes", "No"),
      n         = c(tp, fp, fn, tn),
      label     = c("True Positive\n(correctly identified responders)",
                    "False Positive\n(contacted, won\u2019t respond)",
                    "False Negative\n(missed responders)",
                    "True Negative\n(correctly excluded)"),
      fill      = c("#dbeafe", "#fee2e2", "#fef9c3", "#f0fdf4")
    )

    p <- ggplot(cm, aes(x = Predicted, y = Actual, fill = fill,
                         text = paste0(label, "\nn = ", n))) +
      geom_tile(colour = "white", linewidth = 1.5) +
      geom_text(aes(label = n), size = 7, fontface = "bold",
                colour = "#1f2937") +
      scale_fill_identity() +
      labs(x = "Predicted", y = "Actual") +
      theme_app() +
      theme(panel.grid = element_blank())

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  # Reset button
  observeEvent(input$ml_reset, {
    updateSliderInput(session, "ml_campaign",   value = 0)
    updateSliderInput(session, "ml_recency",    value = 49)
    updateSliderInput(session, "ml_tenure",     value = 542)
    updateSliderInput(session, "ml_income",     value = 52000)
    updateSliderInput(session, "ml_spend",      value = 605)
    updateSliderInput(session, "ml_education",  value = 2)
    updateSliderInput(session, "ml_web_visits", value = 5)
    updateSliderInput(session, "ml_deals_pct",  value = 0.2)
    updateSliderInput(session, "ml_threshold",  value = 0.50)
    updateRadioButtons(session, "ml_children",  selected = 1)
  })
}

shinyApp(ui, server)
