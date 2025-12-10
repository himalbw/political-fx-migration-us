df <- read_csv("./data/panel_iv.csv")

# scale main vars so they can be per 10k
df$all    <- df$foreign_flow_3y      / 10000
df$low_A  <- df$d_fb_low_skill_a_3y  / 10000
df$high_A <- df$d_fb_high_skill_a_3y / 10000
df$low_B  <- df$d_fb_low_skill_b_3y  / 10000
df$high_B <- df$d_fb_high_skill_b_3y / 10000


m1_overall <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    all |
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

m2a_low_A <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    low_A |
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

m2b_low_B <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    low_B |
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

m3a_high_A <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    high_A |
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

m3b_high_B <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    high_B |
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

# Make sure the directory exists
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

coef_map <- c(
  "all"    = "Δ Migrant Stock (3Y) per 10,000",
  "low_A"  = "Δ Low-Skill Migrant Stock (3Y) per 10,000",
  "low_B"  = "Δ Low-Skill Migrant Stock (3Y) per 10,000",
  "high_A" = "Δ High-Skill Migrant Stock (3Y) per 10,000",
  "high_B" = "Δ High-Skill Migrant Stock (3Y) per 10,000"
)

coef_omit <- "gop_two_party_share_lag4|pop_total|hispanic_native_share|white_share|native_lt_hs_share|Intercept"

modelsummary(
  list(
    "1"  = m1_overall,
    "2A" = m2a_low_A,
    "2B" = m2b_low_B,
    "3A" = m3a_high_A,
    "3B" = m3b_high_B
  ),
  coef_map  = coef_map,
  coef_omit = coef_omit,
  stars = TRUE,
  output    = "output/tables/baseline_1.html", 
)

## V2. INCLUDE BOTH 

m1 <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    all |
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

m2b <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    low_B + high_B |
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

m3a <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    high_A  + low_A|
    county_fips + YEAR,
  data    = df,
  cluster = ~ county_fips
)

# Make sure the directory exists
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

coef_map <- c(
  "all"    = "Δ Migrant Stock (3Y) per 10,000",
  "low_A"  = "Δ Low-Skill Migrant Stock (3Y) per 10,000",
  "low_B"  = "Δ Low-Skill Migrant Stock (3Y) per 10,000",
  "high_A" = "Δ High-Skill Migrant Stock (3Y) per 10,000",
  "high_B" = "Δ High-Skill Migrant Stock (3Y) per 10,000"
)

coef_omit <- "gop_two_party_share_lag4|pop_total|hispanic_native_share|white_share|native_lt_hs_share|Intercept"

modelsummary(
  list(
    "1"  = m1,
    "2A" = m2b,
    "2B" = m3a
  ),
  coef_map  = coef_map,
  coef_omit = coef_omit,
  gof_omit  = "R2|RMSE|AIC|BIC",
  stars = TRUE,
  output    = "output/tables/baseline_1_bothHSLS.html", 
)


# 2. Models with skill split A × YEAR interactions

# Low-skill A: effect allowed to vary by year
m2A_low_A_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    low_A * factor(YEAR) +              # main low_A + YEAR dummies + interactions
    0 |                                 # no additional regressors on RHS here
    county_fips,                        # county FE
  data    = df,
  cluster = ~ county_fips
)

# High-skill A: effect allowed to vary by year
m3A_high_A_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    high_A * factor(YEAR) +
    0 |
    county_fips,
  data    = df,
  cluster = ~ county_fips
)

# Optional: combined model with both low_A and high_A interacted with YEAR
mA_both_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    low_A  * factor(YEAR) +
    high_A * factor(YEAR) +
    0 |
    county_fips,
  data    = df,
  cluster = ~ county_fips
)

# ---------------------------------------------
# 2. Table for the year-interaction models
#    (you can refine coef_map later for plotting)
# ---------------------------------------------

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

coef_map_year <- c(
  "low_A"  = "Δ Skilled Migrant Stock (3Y) per 10,000",
  "high_A" = "Δ Skilled Migrant Stock (3Y) per 10,000",
  "low_A:factor(YEAR)2012" = "Δ Migrants x 2012",
  "low_A:factor(YEAR)2016" = "Δ Migrants x 2016",
  "low_A:factor(YEAR)2020" = "Δ Migrants x 2020",
  "low_A:factor(YEAR)2024" = "Δ Migrants x 2024",
  "high_A:factor(YEAR)2012" = "Δ Migrants x 2012",
  "high_A:factor(YEAR)2016" = "Δ Migrants x 2016",
  "high_A:factor(YEAR)2020" = "Δ Migrants x 2020",
  "high_A:factor(YEAR)2024" = "Δ Migrants x 2024")


coef_omit_year <- paste(
  "gop_two_party_share_lag4",
  "pop_total",
  "hispanic_native_share",
  "white_share",
  "native_lt_hs_share",
  "factor\\(YEAR\\)",   # omit the plain year dummies from display
  "Intercept",
  sep = "|"
)

modelsummary(
  list(
    "2A (Low-skill A × Year)"  = m2A_low_A_year,
    "3A (High-skill A × Year)" = m3A_high_A_year
  ),
  coef_map  = coef_map_year,
  coef_omit = "gop_two_party_share_lag4|pop_total|hispanic_native_share|white_share|native_lt_hs_share|Intercept",
  stars     = TRUE,
  gof_omit  = "R2|RMSE|AIC|BIC",
  title     = "Model 2: Year-Varying Effects of Skill-Split A Migrant Inflows (per 10,000) on GOP Two-Party Vote Share",
  notes     = "Coefficients reported reflect the change in GOP two-party vote share associated with a 10,000-person increase in 3-year migrant inflows; interaction terms reflect year-specific effects.",
  output    = "output/tables/baseline_2_skillA_year.html"
)


# 2. Models with skill split A × YEAR interactions

mA_both_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    low_A  * factor(YEAR) +
    high_A * factor(YEAR) |
    county_fips,
  data    = df,
  cluster = ~ county_fips
)

summary(mA_both_year)

mB_both_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    low_B  * factor(YEAR) +
    high_B * factor(YEAR) |
    county_fips,
  data    = df,
  cluster = ~ county_fips
)

summary(mB_both_year)


coef_map_year <- c(
  # main effects
  "low_A"   = "Δ Low-skill per 10,000",
  "high_A"  = "Δ High-skill per 10,000",
  "low_B"   = "Δ Low-skill per 10,000",
  "high_B"  = "Δ High-skill per 10,000",
  
  # interactions – low skill A
  "low_A:factor(YEAR)2012" = "Low-skill × 2012",
  "low_A:factor(YEAR)2016" = "Low-skill × 2016",
  "low_A:factor(YEAR)2020" = "Low-skill × 2020",
  "low_A:factor(YEAR)2024" = "Low-skill × 2024",
  
  # interactions – high skill A
  "factor(YEAR)2012:high_A" = "High-skill × 2012",
  "factor(YEAR)2016:high_A" = "High-skill × 2016",
  "factor(YEAR)2020:high_A" = "High-skill × 2020",
  "factor(YEAR)2024:high_A" = "High-skill × 2024",
  
  # interactions – low skill B
  "low_B:factor(YEAR)2012" = "Low-skill × 2012",
  "low_B:factor(YEAR)2016" = "Low-skill × 2016",
  "low_B:factor(YEAR)2020" = "Low-skill × 2020",
  "low_B:factor(YEAR)2024" = "Low-skill × 2024",
  
  # interactions – high skill B    ← ***THESE ARE THE ONES YOU NEED***
  "factor(YEAR)2012:high_B" = "High-skill × 2012",
  "factor(YEAR)2016:high_B" = "High-skill × 2016",
  "factor(YEAR)2020:high_B" = "High-skill × 2020",
  "factor(YEAR)2024:high_B" = "High-skill × 2024"
)

# omit controls and plain YEAR dummies (but keep interactions)
coef_omit_year <- paste(
  "gop_two_party_share_lag4",
  "log\\(pop_total\\)",
  "hispanic_native_share",
  "white_share",
  "native_lt_hs_share",
  "Intercept",
  sep = "|"
)

modelsummary(
  list(
    "A: Low & High Skill (A) × Year" = mA_both_year,
    "B: Low & High Skill (B) × Year" = mB_both_year
  ),
  coef_map  = coef_map_year,
  coef_omit = coef_omit_year,
  stars     = TRUE,
  gof_omit  = "R2|RMSE|AIC|BIC",
  title     = "Model 2: Year-Varying Effects of Low- and High-Skill Migrant Inflows (per 10,000) on GOP Two-Party Vote Share",
  notes     = "Flows are scaled so coefficients represent the change in GOP two-party vote share associated with a 10,000-person increase in 3-year migrant inflows. Interaction terms capture year-specific effects by skill group.",
  output    = "output/tables/baseline_2_skillAB_year.html"
)

panel_iv <- read_csv("./data/panel_iv.csv")
panel_iv <- panel_iv %>%
  mutate(
    all_10k    = foreign_flow_3y      / 10000,
    low_A_10k  = d_fb_low_skill_a_3y  / 10000,
    high_A_10k = d_fb_high_skill_a_3y / 10000,
    low_B_10k  = d_fb_low_skill_b_3y  / 10000,
    high_B_10k = d_fb_high_skill_b_3y / 10000
  )
# (1) Overall inflow × Trump
m_total_trump_ols <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    all_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)

# (2) Low & High skill, split A × Trump
mA_both_trump_ols <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    low_A_10k  * factor(trump) +
    high_A_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)

# (3) Low & High skill, split B × Trump
mB_both_trump_ols <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + hispanic_native_share + white_share + native_lt_hs_share +
    low_B_10k  * factor(trump) +
    high_B_10k * factor(trump) |
    county_fips,
  data    = panel_iv,
  cluster = ~ county_fips
)
ols_trump_models <- list(
  "1"   = m_total_trump_ols,
  "2a: Low & High A × Trump"  = mA_both_trump_ols,
  "2b: Low & High B × Trump"  = mB_both_trump_ols
)

# Map coefficients to nice labels
coef_map_ols_trump <- c(
  # total inflow
  "all_10k"                     = "Total Inflow (3Y, per 10k)",
  "all_10k:factor(trump)1"     = "Total Inflow × Trump",
  
  # low/high A
  "low_A_10k"                   = "Low-Skill Inflow",
  "low_A_10k:factor(trump)1"    = "Low-Skill Inflow × Trump",
  "high_A_10k"                  = "High-Skill Inflow",
  "factor(trump)1:high_A_10k"   = "High-Skill Inflow × Trump",
  
  # low/high B
  "low_B_10k"                   = "Low-Skill Inflow",
  "low_B_10k:factor(trump)1"    = "Low-Skill Inflow × Trump",
  "high_B_10k"                  = "High-Skill Inflow",
  "factor(trump)1:high_B_10k"   = "High-Skill Inflow × Trump"
)

coef_omit_ols_trump <- "gop_two_party_share_lag4|log\\(pop_total\\)|hispanic_native_share|white_share|native_lt_hs_share|Intercept"

modelsummary(
  ols_trump_models,
  coef_map  = coef_map_ols_trump,
  coef_omit = coef_omit_ols_trump,
  stars     = TRUE,
  gof_omit  = "IC|Log|Adj|Pseudo|RMSE",
  title     = "OLS: Fitted 3-Year Migrant Inflows and Trump-Era Differences in GOP Vote Share",
  output    = "output/tables/ols_trump_interactions.html"
)

panel_iv <- read_csv("./data/panel_iv.csv")

panel_iv <- panel_iv %>%
  mutate(
    all_10k    = foreign_flow_3y      / 10000,
    low_A_10k  = d_fb_low_skill_a_3y  / 10000,
    high_A_10k = d_fb_high_skill_a_3y / 10000,
    low_B_10k  = d_fb_low_skill_b_3y  / 10000,
    high_B_10k = d_fb_high_skill_b_3y / 10000
  )
# 1: Total inflow × Hispanic ancestry
ols_total_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    all_10k * hispanic_native_share |
    county_fips + YEAR,
  data    = panel_iv,
  cluster = ~ county_fips
)

# 2a: Low-skill inflow (A) × Hispanic ancestry
ols_low_A_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    low_A_10k * hispanic_native_share |
    county_fips + YEAR,
  data    = panel_iv,
  cluster = ~ county_fips
)

# 2b: Low-skill inflow (B) × Hispanic ancestry
ols_low_B_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    low_B_10k * hispanic_native_share |
    county_fips + YEAR,
  data    = panel_iv,
  cluster = ~ county_fips
)

# 3a: High-skill inflow (A) × Hispanic ancestry
ols_high_A_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    high_A_10k * hispanic_native_share |
    county_fips + YEAR,
  data    = panel_iv,
  cluster = ~ county_fips
)

# 3b: High-skill inflow (B) × Hispanic ancestry
ols_high_B_ancestry <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    log(pop_total) + white_share + native_lt_hs_share +
    high_B_10k * hispanic_native_share |
    county_fips + YEAR,
  data    = panel_iv,
  cluster = ~ county_fips
)




