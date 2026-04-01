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
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(scales)
library(shinycssloaders)
library(DT)
library(randomForest)

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

# ── Shared plotly layout defaults ─────────────────────────────────────────────
plt_layout <- list(
  plot_bgcolor  = "#ffffff",
  paper_bgcolor = "#ffffff",
  font          = list(color = "#3d2b1a"),
  margin        = list(t = 30, b = 50, l = 60, r = 20)
)

apply_layout <- function(p, ...) {
  p %>% layout(plot_bgcolor  = plt_layout$plot_bgcolor,
               paper_bgcolor = plt_layout$paper_bgcolor,
               font          = plt_layout$font,
               ...) %>%
    config(displayModeBar = FALSE)
}

# ── Tab 1: plot functions (native plotly) ─────────────────────────────────────

plot_spend_by_edu <- function(df) {
  d <- df %>%
    group_by(Education) %>%
    summarise(avg_spend = mean(TotalSpend), n = n(), .groups = "drop")
  cols <- colorRampPalette(c("#e8f0d0", "#2d5016"))(nrow(d))
  plot_ly(d,
          x         = ~as.character(Education),
          y         = ~avg_spend,
          type      = "bar",
          marker    = list(color = cols),
          text      = ~paste0(as.character(Education),
                              "\nAvg spend: $", round(avg_spend),
                              "\nn = ", n),
          hoverinfo = "text") %>%
    apply_layout(
      xaxis = list(title = "", categoryorder = "array",
                   categoryarray = as.character(d$Education)),
      yaxis = list(title = "Avg 2-year spend ($)", tickprefix = "$"),
      showlegend = FALSE
    )
}

plot_income_spend <- function(df) {
  d <- df %>%
    mutate(grp = if_else(as.logical(HasChildren), "Has children", "No children"))
  no_k  <- d[d$grp == "No children",  ]
  has_k <- d[d$grp == "Has children", ]
  x_rng <- seq(min(d$Income), max(d$Income), length.out = 100)
  lm_no  <- tryCatch(lm(TotalSpend ~ Income, data = no_k),  error = function(e) NULL)
  lm_yes <- tryCatch(lm(TotalSpend ~ Income, data = has_k), error = function(e) NULL)
  p <- plot_ly() %>%
    add_markers(data = no_k,  x = ~Income, y = ~TotalSpend, name = "No children",
                marker    = list(color = "#2d5016", opacity = 0.35, size = 5),
                text      = ~paste0("Income: $", scales::comma(Income),
                                    "\nSpend: $", TotalSpend,
                                    "\nAge: ", Age, "\nEducation: ", as.character(Education)),
                hoverinfo = "text") %>%
    add_markers(data = has_k, x = ~Income, y = ~TotalSpend, name = "Has children",
                marker    = list(color = "#a8c87a", opacity = 0.35, size = 5),
                text      = ~paste0("Income: $", scales::comma(Income),
                                    "\nSpend: $", TotalSpend,
                                    "\nAge: ", Age, "\nEducation: ", as.character(Education)),
                hoverinfo = "text")
  if (!is.null(lm_no))
    p <- p %>% add_lines(x = x_rng, y = predict(lm_no,  data.frame(Income = x_rng)),
                         name = "No children trend", showlegend = FALSE, hoverinfo = "skip",
                         line = list(color = "#2d5016", width = 2))
  if (!is.null(lm_yes))
    p <- p %>% add_lines(x = x_rng, y = predict(lm_yes, data.frame(Income = x_rng)),
                         name = "Has children trend", showlegend = FALSE, hoverinfo = "skip",
                         line = list(color = "#a8c87a", width = 2))
  p %>% apply_layout(
    xaxis  = list(title = "Annual income", tickprefix = "$"),
    yaxis  = list(title = "Total 2-year spend ($)", tickprefix = "$"),
    legend = list(orientation = "h", y = 1.12)
  )
}

plot_age_hist <- function(df) {
  med_age <- median(df$Age)
  plot_ly(df, x = ~Age, type = "histogram",
          xbins     = list(size = 4),
          marker    = list(color = "#3d6b1e", line = list(color = "white", width = 1)),
          hovertemplate = "Age: %{x}<br>Count: %{y}<extra></extra>") %>%
    apply_layout(
      xaxis  = list(title = "Age"),
      yaxis  = list(title = "Count"),
      showlegend = FALSE,
      shapes = list(list(type = "line", x0 = med_age, x1 = med_age,
                         y0 = 0, y1 = 1, yref = "paper",
                         line = list(color = "#7a6a52", dash = "dash", width = 1.5))),
      annotations = list(list(x = med_age + 1, y = 0.95, yref = "paper",
                               text = paste0("median: ", round(med_age)),
                               showarrow = FALSE, xanchor = "left",
                               font = list(color = "#7a6a52", size = 11)))
    )
}

plot_marital <- function(df) {
  d <- df %>% count(MaritalSimple) %>%
    mutate(pct = n / sum(n) * 100, ms = as.character(MaritalSimple)) %>%
    arrange(n)
  plot_ly(d, y = ~ms, x = ~pct, type = "bar", orientation = "h",
          marker    = list(color = "#3d6b1e"),
          text      = ~paste0(ms, ": ", round(pct, 1), "%\nn = ", n),
          hoverinfo = "text") %>%
    apply_layout(
      xaxis = list(title = "% of customers"),
      yaxis = list(title = "", categoryorder = "array", categoryarray = d$ms),
      showlegend = FALSE,
      margin = list(t = 20, b = 40, l = 90, r = 40)
    )
}

# ── Tab 2: plot functions (native plotly) ─────────────────────────────────────

plot_spend_share <- function(df) {
  d <- df %>%
    summarise(across(all_of(spend_cols), sum)) %>%
    pivot_longer(everything(), names_to = "cat", values_to = "total") %>%
    mutate(pct = total / sum(total) * 100,
           cat = str_remove(cat, "Mnt") %>% str_replace("Products", "") %>% str_trim()) %>%
    arrange(pct)
  cols <- if_else(d$pct > 20, "#2d5016", "#a8c87a")
  plot_ly(d, y = ~cat, x = ~pct, type = "bar", orientation = "h",
          marker    = list(color = cols),
          text      = ~paste0(cat, ": ", round(pct, 1), "%"),
          hoverinfo = "text") %>%
    apply_layout(
      xaxis = list(title = "% of total spend", range = c(0, 62)),
      yaxis = list(title = "", categoryorder = "array", categoryarray = d$cat),
      showlegend = FALSE,
      margin = list(t = 20, b = 40, l = 80, r = 20)
    )
}

plot_spend_kids <- function(df) {
  d <- df %>%
    filter(!is.na(HasChildren)) %>%
    mutate(grp = if_else(as.logical(HasChildren), "Has children", "No children")) %>%
    group_by(grp) %>%
    summarise(across(all_of(spend_cols), mean), .groups = "drop") %>%
    pivot_longer(-grp, names_to = "cat", values_to = "avg") %>%
    mutate(cat = str_remove(cat, "Mnt") %>% str_replace("Products", "") %>% str_trim())
  d_no  <- d %>% filter(grp == "No children")
  d_yes <- d %>% filter(grp == "Has children")
  plot_ly() %>%
    add_bars(data = d_no,  x = ~cat, y = ~avg, name = "No children",
             marker    = list(color = "#2d5016"),
             text      = ~paste0("No children\n", cat, ": $", round(avg)),
             hoverinfo = "text") %>%
    add_bars(data = d_yes, x = ~cat, y = ~avg, name = "Has children",
             marker    = list(color = "#a8c87a"),
             text      = ~paste0("Has children\n", cat, ": $", round(avg)),
             hoverinfo = "text") %>%
    apply_layout(
      barmode = "group",
      xaxis   = list(title = ""),
      yaxis   = list(title = "Avg spend ($)", tickprefix = "$"),
      legend  = list(orientation = "h", y = 1.12)
    )
}

plot_channels <- function(df) {
  d <- df %>%
    filter(!is.na(IncomeTier)) %>%
    mutate(IncomeTier = factor(as.character(IncomeTier), levels = c("Low", "Mid", "High"))) %>%
    group_by(IncomeTier) %>%
    summarise(across(all_of(purchase_channel_cols), mean), .groups = "drop") %>%
    pivot_longer(-IncomeTier, names_to = "channel", values_to = "avg") %>%
    mutate(channel = str_remove(channel, "Num|Purchases") %>% str_trim())
  plot_ly() %>%
    add_bars(data = d %>% filter(IncomeTier == "Low"),
             x = ~channel, y = ~avg, name = "Low",
             marker    = list(color = "#e8f0d0"),
             text      = ~paste0("Low income\n", channel, ": ", round(avg, 1), " purchases/yr"),
             hoverinfo = "text") %>%
    add_bars(data = d %>% filter(IncomeTier == "Mid"),
             x = ~channel, y = ~avg, name = "Mid",
             marker    = list(color = "#3d6b1e"),
             text      = ~paste0("Mid income\n", channel, ": ", round(avg, 1), " purchases/yr"),
             hoverinfo = "text") %>%
    add_bars(data = d %>% filter(IncomeTier == "High"),
             x = ~channel, y = ~avg, name = "High",
             marker    = list(color = "#1a3009"),
             text      = ~paste0("High income\n", channel, ": ", round(avg, 1), " purchases/yr"),
             hoverinfo = "text") %>%
    apply_layout(
      barmode = "group",
      xaxis   = list(title = ""),
      yaxis   = list(title = "Avg purchases per year"),
      legend  = list(orientation = "h", y = 1.12, title = list(text = "Income tier"))
    )
}

# ── Tab 3: plot functions (native plotly) ─────────────────────────────────────

plot_campaign_rates <- function(df) {
  rates <- df %>%
    summarise(across(all_of(camp_cols), ~ mean(.x) * 100),
              Response = mean(Response == "Yes") * 100) %>%
    pivot_longer(everything(), names_to = "campaign", values_to = "rate") %>%
    mutate(
      label = c("Cmp 1", "Cmp 2", "Cmp 3", "Cmp 4", "Cmp 5", "Final"),
      color = case_when(campaign == "Response" ~ "#2d5016",
                        rate < 4               ~ "#8b2020",
                        TRUE                   ~ "#3d6b1e")
    )
  overall_avg <- mean(rates$rate)
  plot_ly(rates, x = ~label, y = ~rate, type = "bar",
          marker    = list(color = ~color),
          text      = ~paste0(label, ": ", round(rate, 1), "% accepted"),
          hoverinfo = "text") %>%
    apply_layout(
      xaxis  = list(title = ""),
      yaxis  = list(title = "Acceptance rate", ticksuffix = "%"),
      showlegend = FALSE,
      shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                         y0 = overall_avg, y1 = overall_avg,
                         line = list(color = "#7a6a52", dash = "dash", width = 1.5))),
      annotations = list(list(x = 0.02, y = overall_avg + 0.4, xref = "paper",
                               text = "avg", showarrow = FALSE, xanchor = "left",
                               font = list(color = "#7a6a52", size = 11)))
    )
}

plot_responder_profile <- function(df) {
  d <- df %>%
    group_by(Response) %>%
    summarise(`Avg income ($k)`    = mean(Income) / 1000,
              `Avg spend ($)`      = mean(TotalSpend),
              `Avg recency (days)` = mean(Recency),
              `Avg tenure (days)`  = mean(Tenure),
              `Avg camp. history`  = mean(CampaignHistory),
              .groups = "drop") %>%
    pivot_longer(-Response, names_to = "metric", values_to = "value")
  plot_ly() %>%
    add_bars(data = d %>% filter(Response == "Yes"),
             y = ~metric, x = ~value, name = "Yes", orientation = "h",
             marker    = list(color = "#2d5016"),
             text      = ~paste0("Yes\n", metric, ": ", round(value, 1)),
             hoverinfo = "text") %>%
    add_bars(data = d %>% filter(Response == "No"),
             y = ~metric, x = ~value, name = "No", orientation = "h",
             marker    = list(color = "#e8f0d0"),
             text      = ~paste0("No\n", metric, ": ", round(value, 1)),
             hoverinfo = "text") %>%
    apply_layout(
      barmode = "group",
      xaxis   = list(title = ""),
      yaxis   = list(title = ""),
      legend  = list(orientation = "h", y = 1.12, title = list(text = "Responded?")),
      margin  = list(t = 40, b = 20, l = 130, r = 20)
    )
}

plot_recency_response <- function(df) {
  d <- df %>%
    mutate(RecencyBucket = cut(Recency, breaks = c(-1, 25, 50, 75, 100),
                               labels = c("0\u201325", "26\u201350",
                                          "51\u201375", "76\u2013100"))) %>%
    group_by(RecencyBucket) %>%
    summarise(rate = mean(Response == "Yes") * 100, n = n(), .groups = "drop")
  lvls <- c("0\u201325", "26\u201350", "51\u201375", "76\u2013100")
  plot_ly(d, x = ~as.character(RecencyBucket), y = ~rate, type = "bar",
          marker    = list(color = "#3d6b1e"),
          text      = ~paste0("Recency: ", RecencyBucket, " days\n",
                              "Response rate: ", round(rate, 1), "%\nn = ", n),
          hoverinfo = "text") %>%
    apply_layout(
      xaxis = list(title = "Days since last purchase",
                   categoryorder = "array", categoryarray = lvls),
      yaxis = list(title = "Response rate (%)", ticksuffix = "%"),
      showlegend = FALSE
    )
}

plot_camp_history_response <- function(df) {
  d <- df %>%
    group_by(CampaignHistory) %>%
    summarise(rate = mean(Response == "Yes") * 100, n = n(), .groups = "drop")
  plot_ly(d, x = ~as.character(CampaignHistory), y = ~rate, type = "bar",
          marker    = list(color = "#2d5016"),
          text      = ~paste0("Previous accepts: ", CampaignHistory,
                              "\nResponse rate: ", round(rate, 1), "%\nn = ", n),
          hoverinfo = "text") %>%
    apply_layout(
      xaxis = list(title = "Campaigns accepted previously (out of 5)"),
      yaxis = list(title = "Response rate (%)", ticksuffix = "%"),
      showlegend = FALSE
    )
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
      plot_spend_by_edu(df)
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
      plot_income_spend(df)
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
      plot_age_hist(df)
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
      plot_marital(df)
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
      plot_spend_share(tab2_data())
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_spend_kids <- renderPlotly({
    tryCatch({
      req(nrow(tab2_data()) > 0)
      plot_spend_kids(tab2_data())
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_channels <- renderPlotly({
    tryCatch({
      req(nrow(tab2_data()) > 0)
      plot_channels(tab2_data())
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
      plot_campaign_rates(customers)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_responder_profile <- renderPlotly({
    tryCatch({
      plot_responder_profile(customers)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_recency <- renderPlotly({
    tryCatch({
      plot_recency_response(customers)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste("Error:", e$message),
                            font = list(color = "#8b2020", size = 13)))
    })
  })
  output$plot_camp_history <- renderPlotly({
    tryCatch({
      plot_camp_history_response(customers)
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
      arrange(importance) %>%
      mutate(
        colour = if_else(direction == "negative", "#ef4444", "#2d5016"),
        label  = case_when(
          feature == "CampaignHistory"         ~ "Campaign history",
          feature == "CampaignHistory_x_Recency" ~ "Campaign hist × Recency",
          feature == "Tenure"                  ~ "Tenure",
          feature == "Recency"                 ~ "Recency (negative effect)",
          feature == "HasChildren"             ~ "Has children (negative effect)",
          feature == "DealsPct"                ~ "Deals %",
          feature == "Education_n"             ~ "Education level",
          feature == "NumWebVisitsMonth"       ~ "Web visits / month",
          feature == "TotalSpend"              ~ "Total spend",
          feature == "Age"                     ~ "Age",
          feature == "log_Income"              ~ "Log income",
          feature == "SpendPerPurchase"        ~ "Spend per purchase",
          TRUE                                 ~ as.character(feature)
        )
      )

    plot_ly(imp,
            x         = ~importance,
            y         = ~label,
            type      = "bar",
            orientation = "h",
            marker    = list(color = ~colour),
            text      = ~paste0(label, "\nCoefficient magnitude: ", round(importance, 3)),
            hoverinfo = "text") %>%
      apply_layout(
        xaxis = list(title = "Coefficient magnitude (logistic regression)"),
        yaxis = list(title = "", categoryorder = "array", categoryarray = imp$label),
        showlegend = FALSE
      )
  })

  # ROC curve
  output$ml_roc <- renderPlotly({
    roc_df <- model_metrics$roc_df
    lr_df  <- roc_df[roc_df$model == "Logistic Regression", ]
    rf_df  <- roc_df[roc_df$model == "Random Forest", ]

    plot_ly() %>%
      add_lines(data = lr_df, x = ~fpr, y = ~tpr, name = "Logistic Regression",
                line = list(color = "#2d5016", width = 2),
                text = ~paste0("Logistic Regression\nFPR: ", round(fpr, 3),
                               " | TPR: ", round(tpr, 3)),
                hoverinfo = "text") %>%
      add_lines(data = rf_df, x = ~fpr, y = ~tpr, name = "Random Forest",
                line = list(color = "#a8c87a", width = 2),
                text = ~paste0("Random Forest\nFPR: ", round(fpr, 3),
                               " | TPR: ", round(tpr, 3)),
                hoverinfo = "text") %>%
      add_lines(x = c(0, 1), y = c(0, 1), name = "Random classifier",
                line = list(color = "#d1d5db", dash = "dash", width = 1),
                hoverinfo = "none", showlegend = FALSE) %>%
      apply_layout(
        xaxis = list(title = "False positive rate", range = c(0, 1)),
        yaxis = list(title = "True positive rate",  range = c(0, 1)),
        legend = list(orientation = "h", y = 1.08, x = 0),
        annotations = list(list(
          x = 0.68, y = 0.08, xref = "x", yref = "y", showarrow = FALSE,
          text = paste0("LR AUC = ", round(model_metrics$lr_auc, 3),
                        "<br>RF AUC = ", round(model_metrics$rf_auc, 3)),
          font = list(color = "#374151", size = 12), align = "left"
        ))
      )
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

    plot_ly(cm,
            x         = ~Predicted,
            y         = ~Actual,
            z         = ~n,
            type      = "heatmap",
            colorscale = list(c(0, "#f0fdf4"), c(1, "#dbeafe")),
            showscale  = FALSE,
            text       = ~paste0(label, "\nn = ", n),
            hoverinfo  = "text",
            colors     = ~fill) %>%
      add_annotations(
        x    = ~Predicted,
        y    = ~Actual,
        text = ~as.character(n),
        font = list(size = 22, color = "#1f2937"),
        showarrow = FALSE
      ) %>%
      apply_layout(
        xaxis = list(title = "Predicted", categoryorder = "array",
                     categoryarray = c("Yes", "No")),
        yaxis = list(title = "Actual", categoryorder = "array",
                     categoryarray = c("No", "Yes"))
      )
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
