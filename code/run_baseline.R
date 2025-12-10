df <- read_csv("./data/panel_iv.csv")

# scale main vars so they can be per 100k
df$all <- df$foreign_flow_3y*10000  
df$low_A <- df$d_fb_low_skill_a_3y*10000
df$high_A <- df$d_fb_high_skill_a_3y*10000
df$low_B <- df$d_fb_low_skill_b_3y*10000
df$high_B <- df$d_fb_high_skill_b_3y*10000



