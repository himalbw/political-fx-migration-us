library(ggplot2)
library(purrr)
panel_iv <- read_csv("./data/panel_iv.csv")
df <- panel_iv %>% select(gop_two_party_share, gop_two_party_share_lag4, pop_total, )
panel_iv$trump <- ifelse(panel_iv$YEAR %in% c(2016, 2024), 1, 0)

m_low_B_fit_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    fit_low_B_10k * factor(YEAR) |     # <-- year-by-fit interactions here
    county_fips,                   # FE only for county
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

library(purrr)
get_year_effects <- function(model, base_var, years) {
  coefs <- coef(model)
  base  <- coefs[base_var]
  
  tibble(
    year = years,
    effect = map_dbl(years, function(yr) {
      if (is.na(base)) return(NA_real_)
      
      # interaction coefficient name possibilities
      int1 <- paste0(base_var, ":factor(YEAR)", yr)
      int2 <- paste0("factor(YEAR)", yr, ":", base_var)
      
      int_coef <- 0
      if (int1 %in% names(coefs)) int_coef <- coefs[int1]
      if (int2 %in% names(coefs)) int_coef <- coefs[int2]
      
      base + int_coef
    })
  )
}

election_years <- panel_iv %>%
  filter(YEAR %% 4 == 0) %>%
  pull(YEAR) %>%
  unique() %>%
  sort()

# Overall
eff_total_A <- get_year_effects(m_total_fit_year, "fit_overall", election_years) %>%
  mutate(series = "Total inflow")

# Low-skill A
eff_low_A <- get_year_effects(m_low_A_fit_year, "fit_low_A", election_years) %>%
  mutate(series = "Low-skill A")

# High-skill A
eff_high_A <- get_year_effects(m_high_A_fit_year, "fit_high_A", election_years) %>%
  mutate(series = "High-skill A")

eff_A <- bind_rows(eff_total_A, eff_low_A, eff_high_A)


p_A <- ggplot(eff_A, aes(x = year, y = effect, color = series, group = series)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = election_years) +
  labs(
    x = "Election year",
    y = "Effect on GOP two-party share",
    color = "Series",
    title = "Year-specific effects: Total vs Skill Split A"
  ) +
  theme_minimal()

p_A
ggsave("./output/figures/effect_by_year_A.png", p_A, width = 7, height = 4, dpi = 300)
# Overall (reuse same as above)
eff_total_B <- eff_total_A %>% mutate(series = "Total inflow")

# Low-skill B
eff_low_B <- get_year_effects(m_low_B_fit_year, "fit_low_B", election_years) %>%
  mutate(series = "Low-skill B")

# High-skill B
eff_high_B <- get_year_effects(m_high_B_fit_year, "fit_high_B", election_years) %>%
  mutate(series = "High-skill B")

eff_B <- bind_rows(eff_total_B, eff_low_B, eff_high_B)
p_B <- ggplot(eff_B, aes(x = year, y = effect, color = series, group = series)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = election_years) +
  labs(
    x = "Election year",
    y = "Effect on GOP two-party share",
    color = "Series",
    title = "Year-specific effects: Total vs Skill Split B"
  ) +
  theme_minimal()

p_B
ggsave("./output/figures/effect_by_year_B.png", p_B, width = 7, height = 4, dpi = 300)


##### SECOND APPROACH IS RUN YEARLY IV ####

library(broom)
election_years <- seq(2008, 2024, by = 4)
run_yearly_fit <- function(data, years, fit_var, label) {
  
  map_dfr(years, function(yr) {
    
    df_year <- data %>%
      filter(YEAR == yr)
    
    # Build formula without interactions and without FEs
    fml <- as.formula(
      paste(
        "gop_two_party_share ~",
        "gop_two_party_share_lag4 +",
        "pop_total + hispanic_native_share + white_share + native_lt_hs_share +",
        fit_var
      )
    )
    
    mod <- feols(fml, data = df_year)
    
    b  <- coef(mod)[fit_var]
    se <- se(mod)[fit_var]
    
    tibble(
      year   = yr,
      coef   = b,
      se     = se,
      series = label
    )
  })
}

#---------------------------
# A: Total vs Skill Split A
#---------------------------
eff_total_A_yearly <- run_yearly_fit(panel_iv, election_years, "fit_overall", "Total inflow")
eff_low_A_yearly   <- run_yearly_fit(panel_iv, election_years, "fit_low_A",   "Low-skill A")
eff_high_A_yearly  <- run_yearly_fit(panel_iv, election_years, "fit_high_A",  "High-skill A")

eff_A_yearly <- bind_rows(eff_total_A_yearly, eff_low_A_yearly, eff_high_A_yearly) %>%
  mutate(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  )

p_A_yearly <- ggplot(eff_A_yearly,
                     aes(x = year, y = coef, color = series, group = series)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = election_years) +
  labs(
    x     = "Election year",
    y     = "Effect on GOP two-party share",
    color = "Series",
    title = "Year-specific (cross-sectional) effects: Total vs Skill Split A"
  ) +
  theme_minimal()

p_A_yearly

ggsave("./output/figures/effect_by_year_A_yearly.png",
       p_A_yearly, width = 7, height = 4, dpi = 300)

#---------------------------
# B: Total vs Skill Split B
#---------------------------
eff_total_B_yearly <- run_yearly_fit(panel_iv, election_years, "fit_overall", "Total inflow")
eff_low_B_yearly   <- run_yearly_fit(panel_iv, election_years, "fit_low_B",   "Low-skill B")
eff_high_B_yearly  <- run_yearly_fit(panel_iv, election_years, "fit_high_B",  "High-skill B")

eff_B_yearly <- bind_rows(eff_total_B_yearly, eff_low_B_yearly, eff_high_B_yearly) %>%
  mutate(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  )

p_B_yearly <- ggplot(eff_B_yearly,
                     aes(x = year, y = coef, color = series, group = series)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = election_years) +
  labs(
    x     = "Election year",
    y     = "Effect on GOP two-party share",
    color = "Series",
    title = "Year-specific (cross-sectional) effects: Total vs Skill Split B"
  ) +
  theme_minimal()

p_B_yearly

ggsave("./output/figures/effect_by_year_B_yearly.png",
       p_B_yearly, width = 7, height = 4, dpi = 300)




