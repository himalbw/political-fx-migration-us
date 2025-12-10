# 0. Install any packages
library(readr) 
library(modelsummary)
library(tidyr)
library(fixest)

# 1. Load Data
cy <- read_csv("./data/acs_05_23.csv")
load("./data/base_shares_2000.RData")
load("./data/nat_origin_flows_3y.RData")
vote_df <- read_csv('./data/votes_by_county.csv')

# 2. Construct Shift-Share
bs   <- as.data.table(base_shares)        
no   <- as.data.table(nat_origin)
noA  <- as.data.table(nat_origin_skillA)
noB  <- as.data.table(nat_origin_skillB)

# 2a. Overall Instrument
Z_all <- merge(bs, no[, .(origin, YEAR, g_3y)],  by = "origin", allow.cartesian = TRUE)
Z_all[, Z_term := s_co * g_3y]
Z_all <- Z_all %>% rename(county_fips = full_county_fips)
Z_overall <- Z_all[, .(Z = sum(Z_term, na.rm = TRUE)), by = .(county_fips, YEAR)]

# b. HS Instrument (a) 
Z_A_dt <- merge(bs,noA[, .(origin, skill_group_a, YEAR, g_3y)],by = "origin", allow.cartesian = TRUE)
Z_A_dt[, Z_term := s_co * g_3y]
Z_A_dt <- Z_A_dt %>% rename(county_fips = full_county_fips)
Z_A <- Z_A_dt[
  ,
  .(
    Z_lowa  = sum(Z_term[skill_group_a == "low"],  na.rm = TRUE),
    Z_higha = sum(Z_term[skill_group_a == "high"], na.rm = TRUE)
  ),
  by = .(county_fips, YEAR)
]
Z_B_dt <- merge(bs, noB[, .(origin, skill_group_b, YEAR, g_3y)], by = "origin", allow.cartesian = TRUE)
Z_B_dt[, Z_term := s_co * g_3y]
Z_B_dt <- Z_B_dt %>% rename(county_fips = full_county_fips)
Z_B <- Z_B_dt[
  ,
  .(
    Z_lowb  = sum(Z_term[skill_group_b == "low"],  na.rm = TRUE),
    Z_highb = sum(Z_term[skill_group_b == "high"], na.rm = TRUE)
  ),
  by = .(county_fips, YEAR)
]

Z_panel <- Reduce(
  function(d1, d2) merge(d1, d2, by = c("county_fips", "YEAR"), all = TRUE),
  list(Z_overall, Z_A, Z_B)
)
save(Z_panel, file = "./data/shiftshare_Z_panel.RData")

# prep cy for merge 
make_cy_small <- function(cy) {
  cy_small <- cy %>%
    select(
      county_fips, YEAR,
      pop_total,
      foreign_flow_3y,              # ✅ put this back in
      fb_lt_hs_share, fb_hs_share,
      fb_sc_share, fb_ba_share, fb_grad_share
    ) %>%
    arrange(county_fips, YEAR) %>%
    group_by(county_fips) %>%
    mutate(
      # -------------------------------------------------------------
      # 1. Convert shares to LEVELS (counts) for each skill component
      # -------------------------------------------------------------
      fb_lt_hs_count  = fb_lt_hs_share  * pop_total,
      fb_hs_count     = fb_hs_share     * pop_total,
      fb_sc_count     = fb_sc_share     * pop_total,
      fb_ba_count     = fb_ba_share     * pop_total,
      fb_grad_count   = fb_grad_share   * pop_total,
      
      # Cutoff A: low = lt_hs | high = hs+sc+ba+grad
      fb_low_A_count  = fb_lt_hs_count,
      fb_high_A_count = fb_hs_count + fb_sc_count + fb_ba_count + fb_grad_count,
      
      # Cutoff B: low = lt_hs + hs | high = sc+ba+grad
      fb_low_B_count  = fb_lt_hs_count + fb_hs_count,
      fb_high_B_count = fb_sc_count + fb_ba_count + fb_grad_count,
      
      # -------------------------------------------------------------
      # 2. Compute 3-year lag in LEVELS
      # -------------------------------------------------------------
      fb_low_A_lag3   = lag(fb_low_A_count, 3),
      fb_high_A_lag3  = lag(fb_high_A_count, 3),
      
      fb_low_B_lag3   = lag(fb_low_B_count, 3),
      fb_high_B_lag3  = lag(fb_high_B_count, 3),
      
      # -------------------------------------------------------------
      # 3. Compute 3-year flows in LEVELS
      # -------------------------------------------------------------
      d_fb_low_skill_a_3y  = fb_low_A_count  - fb_low_A_lag3,
      d_fb_high_skill_a_3y = fb_high_A_count - fb_high_A_lag3,
      
      d_fb_low_skill_b_3y  = fb_low_B_count  - fb_low_B_lag3,
      d_fb_high_skill_b_3y = fb_high_B_count - fb_high_B_lag3
    ) %>%
    ungroup()
  
}

cy_small <- make_cy_small(cy)
cy_small  <- cy_small  %>% mutate(county_fips = as.integer(county_fips))
cy_small <- cy_small %>% mutate(election_year = ifelse(YEAR < 2023, YEAR, 2024))

Z_panel   <- Z_panel   %>% mutate(county_fips = as.integer(county_fips))
Z_panel <- Z_panel %>% mutate(election_year = ifelse(YEAR < 2023, YEAR, 2024))

panel_fs <- cy_small %>% left_join(Z_panel, by = c("county_fips", "election_year"))
panel_fs$YEAR <- panel_fs$election_year

# Overall inflow
fs_overall <- feols(
  foreign_flow_3y ~ Z | county_fips + YEAR,
  data    = panel_fs,
  cluster = ~county_fips
)
summary(fs_overall)

# Cutoff A: low = lt_hs, high = hs+sc+ba+grad
fs_low_A <- feols(
  d_fb_low_skill_a_3y ~ Z_lowa | county_fips + YEAR,
  data    = panel_fs,
  cluster = ~county_fips
)
summary(fs_low_A)

fs_high_A <- feols(
  d_fb_high_skill_a_3y ~ Z_higha | county_fips + YEAR,
  data    = panel_fs,
  cluster = ~county_fips
)
summary(fs_high_A)

# Cutoff B: low = lt_hs + hs, high = sc+ba+grad
fs_low_B <- feols(
  d_fb_low_skill_b_3y ~ Z_lowb  | county_fips + YEAR,
  data    = panel_fs,
  cluster = ~county_fips
)
summary(fs_low_B)

fs_high_B <- feols(
  d_fb_high_skill_b_3y ~ Z_highb | county_fips + YEAR,
  data    = panel_fs,
  cluster = ~county_fips
)
summary(fs_high_B)

dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)
first_stage_models <- list(
  "Overall inflow"            = fs_overall,
  "Low-skill inflow (A)"      = fs_low_A,
  "High-skill inflow (A)"     = fs_high_A,
  "Low-skill inflow (B)"      = fs_low_B,
  "High-skill inflow (B)"     = fs_high_B
)

coef_map <- c(
  "Z"       = "Overall instrument Z",
  "Z_lowa"  = "Low-skill instrument (A): Z_lowa",
  "Z_higha" = "High-skill instrument (A): Z_higha",
  "Z_lowb"  = "Low-skill instrument (B): Z_lowb",
  "Z_highb" = "High-skill instrument (B): Z_highb"
)

modelsummary(
  first_stage_models,
  coef_map  = coef_map,
  coef_omit = "Intercept",                 # hide intercept if present
  stars     = TRUE,
  #fmt       = sci_fmt,
  gof_omit  = "IC|Log|Adj|Pseudo|RMSE|AIC|BIC",
  output    = "./output/tables/first_stage_results_with24.html",
  title     = "First Stage Regressions: Shift-Share for 3-Year Inflows"
)

# Overall inflow
fs_overall_e <- feols(
  foreign_flow_3y ~ Z | county_fips + YEAR,
  data    = election_panel_iv,
  cluster = ~county_fips
)

# Cutoff A: low = lt_hs, high = hs+sc+ba+grad
fs_low_A_e <- feols(
  d_fb_low_skill_a_3y ~ Z_lowa | county_fips + YEAR,
  data    = election_panel_iv,
  cluster = ~county_fips
)

fs_high_A_e <- feols(
  d_fb_high_skill_a_3y ~ Z_higha | county_fips + YEAR,
  data    = election_panel_iv,
  cluster = ~county_fips
)

# Cutoff B: low = lt_hs + hs, high = sc+ba+grad
fs_low_B_e <- feols(
  d_fb_low_skill_b_3y ~ Z_lowb | county_fips + YEAR,
  data    = election_panel_iv,
  cluster = ~county_fips
)

fs_high_B_e <- feols(
  d_fb_high_skill_b_3y ~ Z_highb | county_fips + YEAR,
  data    = election_panel_iv,
  cluster = ~county_fips
)

## 2. Put in a modelsummary table -------------------------------------

dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)

first_stage_models_e <- list(
  "Overall inflow"            = fs_overall_e,
  "Low-skill inflow (A)"      = fs_low_A_e,
  "High-skill inflow (A)"     = fs_high_A_e,
  "Low-skill inflow (B)"      = fs_low_B_e,
  "High-skill inflow (B)"     = fs_high_B_e
)

coef_map <- c(
  "Z"       = "Overall instrument Z",
  "Z_lowa"  = "Low-skill instrument (A): Z_lowa",
  "Z_higha" = "High-skill instrument (A): Z_higha",
  "Z_lowb"  = "Low-skill instrument (B): Z_lowb",
  "Z_highb" = "High-skill instrument (B): Z_highb"
)

modelsummary(
  first_stage_models_e,
  coef_map  = coef_map,
  coef_omit = "Intercept",
  stars     = TRUE,
  # fmt     = sci_fmt,  # uncomment if you want scientific notation
  gof_omit  = "IC|Log|Adj|Pseudo",
  output    = "./output/tables/first_stage_results_election_years.html",
  title     = "First Stage Regressions (Election Years Only)"
)


## SECOND STAGE

vote_formatted <- vote_df %>%
  select(
    county_fips,
    starts_with("gop_two_party_share_")
  ) %>%
  pivot_longer(
    cols = starts_with("gop_two_party_share_"),
    names_to = "election_year",
    names_pattern = "gop_two_party_share_(\\d+)",
    values_to = "gop_two_party_share"
  ) %>%
  mutate(
    election_year = as.integer(election_year)
  ) %>%
  arrange(county_fips, election_year) %>%
  group_by(county_fips) %>%
  mutate(
    gop_two_party_share_lag4 = lag(gop_two_party_share, 1)  # previous election
  ) %>%
  ungroup()

panel_iv <- panel_fs %>%
  mutate(
    county_fips = as.integer(county_fips),
    YEAR        = as.integer(YEAR)
  ) %>%
  left_join(
    vote_formatted,
    by = c("county_fips", "YEAR" = "election_year")
  ) %>%
  # keep only presidential election years where we actually observe votes
  filter(!is.na(gop_two_party_share))

dem_controls <- cy %>% mutate(election_year = ifelse(YEAR < 2023, YEAR, 2024)) %>% select(county_fips, election_year, hispanic_native_share, white_share, black_share, native_lt_hs_share, native_hs_share)
panel_iv <- panel_iv %>% left_join(dem_controls, by = c("county_fips", "YEAR" = "election_year"))

iv_total <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_native_share + white_share + native_lt_hs_share |
    county_fips + YEAR |
    foreign_flow_3y ~ Z,
  data    = panel_iv,
  cluster = ~county_fips
)
iv_low_A <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_native_share + white_share + native_lt_hs_share |
    county_fips + YEAR |
    d_fb_low_skill_a_3y ~ Z_lowa,
  data    = panel_iv,
  cluster = ~county_fips
)


iv_high_A <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_native_share + white_share + native_lt_hs_share |
    county_fips + YEAR |
    d_fb_high_skill_a_3y ~ Z_higha,
  data    = panel_iv,
  cluster = ~county_fips
)

iv_low_B <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_native_share + white_share + native_lt_hs_share |
    county_fips + YEAR |
    d_fb_low_skill_b_3y ~ Z_lowb,
  data    = panel_iv,
  cluster = ~county_fips
)

iv_high_B <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_native_share + white_share + native_lt_hs_share  |
    county_fips + YEAR |
    d_fb_high_skill_b_3y ~ Z_highb,
  data    = panel_iv,
  cluster = ~county_fips
)


two_sls_models <- list(
  "Total inflow (IV)"         = iv_total,
  "High-skill inflow A (IV)"  = iv_high_A,
  "Low-skill inflow A (IV)"   = iv_low_A,
  "Low-skill inflow B (IV)"   = iv_low_B,
  "High-skill inflow B (IV)"  = iv_high_B
)

coef_map <- c(
  # fitted IV coefficients
  "fit_foreign_flow_3y"       = "Total foreign inflow (3y)",
  "fit_d_fb_high_skill_a_3y"  = "High-skill inflow, A (3y)",
  "fit_d_fb_low_skill_a_3y"   = "Low-skill inflow, A (3y)",
  "fit_d_fb_low_skill_b_3y"   = "Low-skill inflow, B (3y)",
  "fit_d_fb_high_skill_b_3y"  = "High-skill inflow, B (3y)",
  
  # controls / lag
  "gop_two_party_share_lag4"  = "GOP share, last election",
  "pop_total"                 = "Population (total)",
  "hispanic_native_share"     = "Hispanic native share",
  "white_share"               = "White share",
  "native_lt_hs_share"        = "Native <HS share"
)

sci_fmt <- function(x) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "e", digits = 3)
  )
}

modelsummary(
  two_sls_models,
  coef_map  = coef_map,
  # hide controls if you don’t want them shown
  coef_omit = "pop_total|hispanic_native_share|white_share|native_lt_hs_share",
  stars     = TRUE,
  fmt       = sci_fmt,
  gof_omit  = "IC|Log|Adj|Pseudo",
  output    = "./output/tables/2sls_results_with24.html",
  title     = "2SLS Estimates: Immigration Inflows and GOP Two-Party Vote Share (Election Years)"
)

### YEAR VARYING EFFECTS

panel_fs_pred <- panel_fs %>%
  mutate(
    fit_overall = predict(fs_overall, newdata = panel_fs),
    fit_low_A   = predict(fs_low_A,   newdata = panel_fs),
    fit_high_A  = predict(fs_high_A,  newdata = panel_fs),
    fit_low_B   = predict(fs_low_B,   newdata = panel_fs),
    fit_high_B  = predict(fs_high_B,  newdata = panel_fs)
  )
fits_df <- panel_fs_pred %>%
  select(
    county_fips,
    YEAR,
    fit_overall,
    fit_low_A,
    fit_high_A,
    fit_low_B,
    fit_high_B
  ) %>%
  mutate(
    county_fips = as.integer(county_fips),
    YEAR        = as.integer(YEAR)
  )

panel_iv <- panel_iv %>%
  mutate(
    county_fips = as.integer(county_fips),
    YEAR        = as.integer(YEAR)
  ) %>%
  left_join(fits_df, by = c("county_fips", "YEAR"))

panel_iv <- panel_iv %>% select(-YEAR.x, -YEAR.y)
write.csv(panel_iv, "./data/panel_iv.csv")


