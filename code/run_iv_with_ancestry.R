panel_iv <- read_csv("./data/panel_iv.csv")
# Total inflow (fitted) × Ancestry
iv_total_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    fit_overall_10k * hispanic_native_share |
    county_fips + YEAR,
  data = panel_iv,
  cluster = ~county_fips
)

# Low-skill A (fitted) × Ancestry
iv_low_A_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    fit_low_A_10k * hispanic_native_share |
    county_fips + YEAR,
  data = panel_iv,
  cluster = ~county_fips
)

# High-skill A (fitted) × Ancestry
iv_high_A_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    fit_high_A_10k * hispanic_native_share |
    county_fips + YEAR,
  data = panel_iv,
  cluster = ~county_fips
)

# Low-skill B (fitted) × Ancestry
iv_low_B_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    fit_low_B_10k * hispanic_native_share |
    county_fips + YEAR,
  data = panel_iv,
  cluster = ~county_fips
)

# High-skill B (fitted) × Ancestry
iv_high_B_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    fit_high_B_10k * hispanic_native_share |
    county_fips + YEAR,
  data = panel_iv,
  cluster = ~county_fips
)

coef_map_ancestry <- c(
  # Main fitted inflow terms
  "fit_overall_10k" = "Fit Inflow (10k)",
  "fit_low_A_10k"   = "Fit Low-Skill (10k)",
  "fit_high_A_10k"  = "Fit High-Skill (10k)",
  "fit_low_B_10k"   = "Fit Low-Skill (10k)",
  "fit_high_B_10k"  = "Fit High-Skill (10k)",
  
  # Interactions with ancestry
  "fit_overall_10k:hispanic_native_share" = "Fit Inflow × Hispanic Ancestry",
  "fit_low_A_10k:hispanic_native_share"   = "Low-Skill × Hispanic Ancestry",
  "fit_high_A_10k:hispanic_native_share"  = "High-Skill × Hispanic Ancestry",
  "fit_low_B_10k:hispanic_native_share"   = "Low-Skill × Hispanic Ancestry",
  "fit_high_B_10k:hispanic_native_share"  = "High-Skill × Hispanic Ancestry"
)

coef_omit_ancestry <- "gop_two_party_share_lag4|pop_total|white_share|native_lt_hs_share|Intercept"

models_ancestry <- list(
  "1" = iv_total_ancestry,
  "2a"  = iv_low_A_ancestry,
  "2b"  = iv_low_B_ancestry,
  "3a" = iv_high_A_ancestry,
  "3b" = iv_high_B_ancestry
)

modelsummary(
  models_ancestry,
  coef_map  = coef_map_ancestry,
  coef_omit = coef_omit_ancestry,
  stars     = TRUE,
  gof_omit  = "IC|Log|Adj|Pseudo|RMSE",
  title     = "Second Stage IV: Interaction Between Predicted Migrant Inflows and Immigrant Ancestry (Hispanic Share)",
  notes     = "Flows scaled so coefficients reflect the effect of a 10,000-person increase in predicted 3-year inflows. Interaction terms measure how ancestry moderates political responses.",
  output    = "output/tables/iv_inflow_ancestry_interaction.html"
)


