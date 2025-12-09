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
Z_panel   <- Z_panel   %>% mutate(county_fips = as.integer(county_fips))
panel_fs <- cy_small %>% left_join(Z_panel, by = c("county_fips", "YEAR"))

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

dem_controls <- cy %>% select(county_fips, YEAR, hispanic_share, white_share, black_share, lt_hs_share, hs_share)
panel_iv <- panel_iv %>% left_join(dem_controls, by = c("county_fips", "YEAR"))

iv_total <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_share + white_share + lt_hs_share |
    county_fips + YEAR |
    foreign_flow_3y ~ Z,
  data    = panel_iv,
  cluster = ~county_fips
)

iv_high_A <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_share + white_share + lt_hs_share |
    county_fips + YEAR |
    d_fb_high_skill_a_3y ~ Z_higha,
  data    = panel_iv,
  cluster = ~county_fips
)

iv_low_B <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_share + white_share + lt_hs_share |
    county_fips + YEAR |
    d_fb_low_skill_b_3y ~ Z_lowb,
  data    = panel_iv,
  cluster = ~county_fips
)

iv_high_B <- feols(
  gop_two_party_share ~ gop_two_party_share_lag4 + pop_total + hispanic_share + white_share + lt_hs_share |
    county_fips + YEAR |
    d_fb_high_skill_b_3y ~ Z_highb,
  data    = panel_iv,
  cluster = ~county_fips
)

## MODEL REGRESSION TABLE



