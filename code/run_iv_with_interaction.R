library(ggplot2)
library(purrr)
panel_iv <- read_csv("./data/panel_iv.csv")
df <- panel_iv %>% select(gop_two_party_share, gop_two_party_share_lag4, pop_total, )
panel_iv$trump <- ifelse(panel_iv$YEAR %in% c(2016, 2024), 1, 0)

m_low_B_fit_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    fit_low_B_10k * factor(trump) |     # <-- year-by-fit interactions here
    county_fips + factor(YEAR),                   # FE only for county
  data    = panel_iv,
  cluster = ~county_fips
)

m_total_fit_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    fit_overall * factor(YEAR) |
    county_fips,
  data    = panel_iv,
  cluster = ~county_fips
)

m_low_A_fit_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    fit_low_A * factor(YEAR) |
    county_fips,
  data    = panel_iv,
  cluster = ~county_fips
)

m_high_A_fit_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    fit_high_A * factor(YEAR) |
    county_fips,
  data    = panel_iv,
  cluster = ~county_fips
)

m_high_B_fit_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    fit_high_B * factor(YEAR) |
    county_fips,
  data    = panel_iv,
  cluster = ~county_fips
)

# Total inflow (fitted) × Trump
m_total_fit_trump <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    fit_overall_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)

# Low-skill A (fitted) × Trump
m_low_A_fit_trump <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    fit_low_A_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)

# High-skill A (fitted) × Trump
m_high_A_fit_trump <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    fit_high_A_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)

# Low-skill B (fitted) × Trump
m_low_B_fit_trump <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    fit_low_B_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)

# High-skill B (fitted) × Trump
m_high_B_fit_trump <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    fit_high_B_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)

coef_map_trump <- c(
  # Total inflow
  "fit_overall_10k"                = "Fit Inflow",
  "fit_overall_10k:factor(trump)1" = "Fit Inflow × Trump",
  
  # Low-skill inflow A
  "fit_low_A_10k"                  = "Fit Low-Skill Inflow",
  "fit_low_A_10k:factor(trump)1"   = "Fit Low-Skill Inflow × Trump",
  
  # High-skill inflow A
  "fit_high_A_10k"                 = "Fit High-Skill Inflow",
  "fit_high_A_10k:factor(trump)1"  = "Fit High-Skill Inflow × Trump",
  
  # Low-skill inflow B
  "fit_low_B_10k"                  = "Fit Low-Skill Inflow",
  "fit_low_B_10k:factor(trump)1"   = "Fit Low-Skill Inflow × Trump",
  
  # High-skill inflow B
  "fit_high_B_10k"                 = "Fit High-Skill Inflow",
  "fit_high_B_10k:factor(trump)1"  = "Fit High-Skill Inflow × Trump"
)

models_trump <- list(
  "Total inflow (fit × Trump)" = m_total_fit_trump,
  "Low-skill A (fit × Trump)"  = m_low_A_fit_trump,
  "High-skill A (fit × Trump)" = m_high_A_fit_trump,
  "Low-skill B (fit × Trump)"  = m_low_B_fit_trump,
  "High-skill B (fit × Trump)" = m_high_B_fit_trump
)

coef_omit_trump <- "gop_two_party_share_lag4|log\\(pop_total\\)|hispanic_native_share|white_share|native_lt_hs_share|Intercept"

modelsummary(
  models_trump,
  coef_map  = coef_map_trump,
  coef_omit = coef_omit_trump,
  stars     = TRUE,
  gof_omit  = "IC|Log|Adj|Pseudo|RMSE",
  title     = "Fitted 3-Year Migrant Inflows and Differential Effects in Trump vs. Non-Trump Elections",
  output    = "output/tables/fit_trump_interactions.html"
)

######### YEAR EFFECTS GRAPH








