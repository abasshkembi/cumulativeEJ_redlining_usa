library(tidyverse)
library(sf)
library(gbm)

# read in the data from 03b_data_prep.R
ej_reg_sf

# load the functions from 3a_helper_functions.R







# Run GBM Bootstraps for the overall cumulative effect (figure 3)

ej_df_trial <- ej_reg_sf %>%
  as_tibble() %>%
  mutate(red = ifelse(holc_grade == "D", 1, 0)) %>%
  select(neighborho, red, state, city, msa7, msa_name, holc_grade, RAW_E_LEAD:RAW_E_RSEI_AIR, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  mutate(state = factor(state), city = factor(city)) %>%
  na.omit()

# convert the data into within-city percentiles
ej_df_trial_percentile <- ej_df_trial %>%
  gather(env, value, -c(neighborho, red, holc_grade, state, city, msa7, msa_name, RAW_D_LOWINC, RAW_D_NO_HS)) %>%
  group_by(city, state, env) %>%
  mutate(city_percentile = rank(value)/length(value)) %>%
  ungroup() %>%
  select(-value) %>%
  spread(env, city_percentile)

# RUNNING 200 iterations!
########################### gbm_bootstrap(boot_i = 200)

cumulative_OR_gbm <- gbm_extract_cumulative(boot_i = 200)

fig2_a <- cumulative_OR_gbm %>%
  filter(quantile >= 0.1 & quantile <= 0.91) %>%
  group_by(quantile) %>%
  summarise(mean = round(median(OR), 2),
            q2.5 = round(quantile(OR, probs = 0.025), 3),
            q97.5 = round(quantile(OR, probs = 0.975), 3)) %>%
  ungroup() %>%
  ggplot(aes(x = quantile)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0, size = 0.6) +
  geom_point(aes(y = mean), shape = "—", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

fig2_a +
  labs(x = "Quantile of 12 environmental exposures",
       y = "Odds Ratio",
       title = "Joint odds of historically redlined neighborhood",
       subtitle = "Nationwide") +
  theme(plot.title = element_text(hjust = 0.01))

# get IQR diff
gbm_extract_cumulative(boot_i = 200, ref = 0.25) %>%
  filter(quantile >= 0.1 & quantile <= 0.91) %>%
  group_by(quantile) %>%
  summarise(mean = round(median(OR), 2),
            q2.5 = round(quantile(OR, probs = 0.025), 3),
            q97.5 = round(quantile(OR, probs = 0.975), 3)) %>%
  ungroup()


overall_relInf <- gbm_extract_relInf(boot_i = 200) 
overall_relInf %>%
  group_by(var) %>%
  summarise(mean = round(median(rel.inf), 2),
            q2.5 = round(quantile(rel.inf, probs = 0.025), 2),
            q97.5 = round(quantile(rel.inf, probs = 0.975), 2))  %>%
  ungroup() %>%
  arrange(-mean) %>%
  print(n = nrow(.))

var_relInf <- overall_relInf %>%
  group_by(var) %>%
  summarise(mean = round(median(rel.inf), 2),
            q2.5 = round(quantile(rel.inf, probs = 0.025), 2),
            q97.5 = round(quantile(rel.inf, probs = 0.975), 2))  %>%
  ungroup() %>%
  arrange(-mean) %>%
  filter(str_detect(var, "RAW_E")) %>%
  .$var

env_names <- tibble(var = var_relInf,
                    order = 1:12) %>%
  mutate(name = c("Haz. waste sites", "Wastewater d/c", "Traffic", "Diesel PM", 
                  "Udg. storage tanks", "Air cancer risk", "Superfund sites", "Resp. hazard",
                  "RMP sites", "Air tox. release", "Ozone", "PM2.5")) %>%
  mutate(name = factor(name, levels = c("Haz. waste sites", "Wastewater d/c", "Traffic", "Diesel PM", 
                                        "Udg. storage tanks", "Air cancer risk", "Superfund sites", "Resp. hazard",
                                        "RMP sites", "Air tox. release", "Ozone", "PM2.5")))



gbm_extract_boostrap("RAW_E_TSDF", boot_i = 200) %>%
  ggplot(aes(x = exposure)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  #geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0, size = 0.6, alpha = 0.3) +
  #geom_point(aes(y = mean), shape = 15, size = 2, alpha = 0.4) +
  geom_smooth(aes(y = mean), se = FALSE, color = "black") +
  geom_smooth(aes(y = q2.5), se = FALSE, linetype = "dashed", color = "black") +
  geom_smooth(aes(y = q97.5), se = FALSE, linetype = "dashed", color = "black") +
  #geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_cartesian(ylim = c(0.5, 2))


mean_cols

overall_pdps <- NULL
for(i in 2:length(mean_cols)) {
  overall_pdps_temp <- gbm_extract_boostrap(mean_cols[i], boot_i = 200)
  overall_pdps <- rbind(overall_pdps, overall_pdps_temp)
  print(i)
}

overall_pdps

# create plot object with loess regression lines
g1 <- ggplot(overall_pdps) + 
  stat_smooth(aes(x = exposure, y = q2.5, colour = "min"), se = FALSE) +
  stat_smooth(aes(x = exposure, y = q97.5, colour = "max"), se = FALSE) +
  facet_wrap(~var, scales = "free_x")
g1

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
df2 <- data.frame(exposure = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y,
                  panelno = gg1$data[[1]]$PANEL
) 

# use the loess data to add the 'ribbon' to plot 
df2 %>%
  filter(var == 4) %>%
  ggplot() +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.4) +
  facet_wrap(~var)

df3 <- tibble(var = sort(unique(overall_pdps$var)),
              panelno = unique(gg1$data[[1]]$PANEL))

fig2_b <- overall_pdps %>%
  left_join(env_names, by = "var") %>%
  ggplot(aes(x = exposure)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  #geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0, size = 0.6, alpha = 0.3) +
  #geom_point(aes(y = mean), shape = 15, size = 2, alpha = 0.4) +
  #geom_smooth(aes(y = q2.5), se = FALSE, color = "grey", linewidth = 0.5) +
  #geom_smooth(aes(y = q97.5), se = FALSE, color = "grey", linewidth = 0.5) +
  geom_ribbon(
    data = as_tibble(df2) %>% left_join(df3, by = "panelno") %>% left_join(env_names, by = "var"), inherit.aes = FALSE,
    aes(x = exposure, ymin = ymin, ymax = ymax), alpha = 0.3
  ) +
  geom_smooth(aes(y = mean), se = FALSE, color = "black", linewidth = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.position = c(0.93, 0.2)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  facet_wrap(~name, scales = "free_x", ncol = 4) +
  coord_cartesian(ylim = c(0.9, 1.5), xlim = c(0.05, 0.96))

fig2_b

ggarrange(
  fig2_a +
    labs(x = "Quantile of 12 environmental exposures",
         y = "Odds Ratio",
         title = "Joint odds of historically redlined neighborhood") +
    theme(plot.title = element_text(hjust = 0.01)),
  fig2_b +
    labs(x = "Percentile of exposure",
         y = "Odds Ratio",
         title = "Independent exposure-response relationships"),
  nrow = 2,
  labels = c("(A)", "(B)"),
  heights = c(0.5, 1)
)

























#### M/NM analysis

mnm_names_ordered <- ej_df_trial_percentile %>%
  group_by(msa_name) %>%
  count() %>%
  ungroup() %>%
  arrange(-n) %>%
  .$msa_name

ej_df_trial_percentile %>%
  group_by(msa_name) %>%
  count() %>%
  arrange(-n) %>%
  filter(n > 40) %>%
  print(n = nrow(.))

mnm_names_ordered[42]

files_mnm <- list.files("Code/02_Analysis/gbm bootstraps/")
files_mnm_names <-  files_mnm[str_detect(files_mnm, "gbm_redlining_env_i200_")] %>%
  str_remove("gbm_redlining_env_i\\d+_") %>%
  str_remove("\\.rds") %>%
  unique()

mnm_complete_names <- mnm_names_ordered[which(mnm_names_ordered %in% files_mnm_names)]

#gbm_bootstrap(boot_i = 200, msa3 = "New York-Newark-Jersey City, NY-NJ-PA")
#gbm_bootstrap(boot_i = 200, msa3 = "Chicago-Naperville-Elgin, IL-IN-WI")
#gbm_bootstrap(boot_i = 200, msa3 = "Los Angeles-Long Beach-Anaheim, CA")
#gbm_bootstrap(boot_i = 200, msa3 = "Boston-Cambridge-Nashua, MA-NH")
#gbm_bootstrap(boot_i = 200, msa3 = "Detroit-Warren-Dearborn, MI")
#gbm_bootstrap(boot_i = 200, msa3 = "San Francisco-Oakland-Hayward, CA")
#gbm_bootstrap(boot_i = 200, msa3 = "Cleveland-Elyria, OH")
#gbm_bootstrap(boot_i = 200, msa3 = "St. Louis, MO-IL")
#gbm_bootstrap(boot_i = 200, msa3 = "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD")
#gbm_bootstrap(boot_i = 200, msa3 = "New Orleans-Metairie, LA")
#gbm_bootstrap(boot_i = 200, msa3 = "Kansas City, MO-KS")

#for(i in 21:42) {
#  print(i)
#  gbm_bootstrap(boot_i = 200, msa3 = mnm_names_ordered[i])
#}

mnm_cumulative_OR_gbm <- NULL
for(i in 1:length(mnm_complete_names)) {
  temp_df <- gbm_extract_cumulative(boot_i = 200, ref = 0.25, msa3 = mnm_complete_names[i]) %>%
    filter(quantile > 0.74 & quantile < 0.76) %>%
    summarise(mean = round(median(OR), 2),
              q2.5 = round(quantile(OR, probs = 0.025), 3),
              q97.5 = round(quantile(OR, probs = 0.975), 3)) %>%
    ungroup() %>%
    mutate(city = mnm_complete_names[i])
  
  mnm_cumulative_OR_gbm <- rbind(mnm_cumulative_OR_gbm, temp_df)
}

mnm_pops <- ej_reg_sf %>%
  as_tibble() %>%
  group_by(msa_name) %>%
  summarise(sum_pop = sum(TOTALPOP))

mnm_percentiles <- ej_df_trial %>%
  select(-city, -msa7, -state, -RAW_D_LOWINC, -RAW_D_NO_HS, -holc_grade) %>%
  #select(neighborho, msa_name, RAW_E_PM25) %>%
  gather(env, value, -c(neighborho, msa_name, red)) %>%
  group_by(env) %>%
  mutate(percentile = rank(value)/length(value)) %>%
  ungroup() %>%
  select(-value) %>%
  filter(red == 1) %>%
  group_by(msa_name, neighborho) %>%
  summarise(mean_percentile = mean(percentile, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(msa_name) %>%
  summarise(mean_percentile_msa = mean(mean_percentile, na.rm = TRUE)) %>%
  ungroup()

# is there some confounding of the ORs by neighborhood count of MSA?
mnm_cumulative_OR_gbm %>%
  filter(mean < 30) %>%
  left_join(ej_df_trial_percentile %>%
              group_by(msa_name) %>%
              count(), by = c("city" = "msa_name")) %>%
  lm(mean~n, data = .) %>% summary()
# nope


# final figure 3
fig3 <- mnm_cumulative_OR_gbm %>%
  arrange(-mean) %>%
  left_join(mnm_pops, by = c("city" = "msa_name")) %>%
  #left_join(mnm_percentiles, by = c("city" = "msa_name")) %>%
  as_tibble() %>%
  #filter(mean < 10) %>%
  mutate(shape_bin = ifelse(q2.5 < 1, "open", "closed")) %>%
  ggplot(aes(y = fct_reorder(city, mean))) +
  #ggplot(aes(y = city)) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "black") +
  geom_errorbar(aes(xmin = q2.5, xmax = q97.5, color = shape_bin), width = 0.4) +
  geom_point(aes(x = mean, shape = shape_bin, size = sum_pop), color = "grey20") +
  #geom_point(aes(x = mean, shape = shape_bin, size = mean_percentile_msa), color = "grey20") +
  theme_bw() +
  #coord_cartesian(xlim = c(0, 10)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.17),
        legend.key.height = unit(0.035, "npc")
  ) +
  scale_shape_manual(values = c("open" = 0, "closed" = 15)) +
  scale_color_manual(values = c("open" = "grey70", "closed" = "grey40")) +
  #scale_size_manual(breaks = c(250000, 500000, 1e6)) %>%
  scale_size_binned(breaks = c(250000, 1e6),
                    labels = c("250k", "1m"),
                    range = c(0.5, 6.5),
                    name = "Metropolitan\nPopulation") +
  #scale_size_manual(labels = c("<25", "<50", "<75", ">100")) %>%
  scale_x_log10(breaks = c(0, 0.5, 1, 2, 5, 10, 50, 100)) +
  annotation_logticks(sides = "b") +
  labs(x = "Odds Ratio\n(from simultaneous, IQR increase in 12 environmental exposures)",
       y = NULL, 
       title = "Joint odds of historically redlined neighborhood",
       subtitle = "By metropolitan area") +
  guides(shape = "none", color = "none",
         size = guide_bins(override.aes=list(shape = 0)))

fig3


gbm_extract_cumulative(boot_i = 200, ref = 0.25, msa3 = "Oklahoma City, OK") %>%
  filter(quantile >= 0.1 & quantile <= 0.91) %>%
  group_by(quantile) %>%
  summarise(mean = round(median(OR), 2),
            q2.5 = round(quantile(OR, probs = 0.025), 3),
            q97.5 = round(quantile(OR, probs = 0.975), 3)) %>%
  ungroup() %>%
  ggplot(aes(x = quantile)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0, size = 0.6) +
  geom_point(aes(y = mean), shape = "—", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

gbm_extract_boostrap("RAW_E_TSDF", boot_i = 1, msa3 = "New York-Newark-Jersey City, NY-NJ-PA") %>%
  ggplot(aes(x = exposure, y = mean)) +
  geom_smooth()

