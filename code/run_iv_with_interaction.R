panel_iv <- read_csv("./data/panel_iv.csv")

m_low_B_fit_year <- feols(
  gop_two_party_share ~
    gop_two_party_share_lag4 +
    pop_total + hispanic_native_share + white_share + native_lt_hs_share +
    fit_low_B * factor(YEAR) |     # <-- year-by-fit interactions here
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



