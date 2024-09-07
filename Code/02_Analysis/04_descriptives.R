
library(tidyverse)
library(spaMM)
library(spdep)
library(RSpectra)
library(sf)

# read in the data from 03b_data_prep.R
ej_reg_sf



### getting the number and percent of neighborhoods by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(n = n()) %>% # get the count
  ungroup() %>% ### to remove the group_by
  mutate(percent = round(n/sum(n)*100, 1))

### total number of neighborhoods
ej_df_final %>% nrow()


## also want total aggregate sum
### getting the number and percent of popn (hopefully) by HOLC grade (throws errors idk)
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(popn_sum = sum(TOTALPOP, sort = TRUE)) %>%
  ungroup() %>% ### to remove the group_by
  mutate(
    sum_popn_sum = sum(popn_sum),
    percent = round(popn_sum/sum_popn_sum * 100, 1))
#   
### getting the number and percent of minorities by HOLC grade (can't figure out how to get %)
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(popn_sum = sum(TOTALPOP, sort = TRUE),
            num_min = sum(NUM_MINORITY)) %>% # get the count
  ungroup() %>% 
  mutate(perc_min = round(num_min/popn_sum*100, 1))

ej_df_final %>%
  summarise(popn_sum = sum(TOTALPOP, sort = TRUE),
            num_min = sum(NUM_MINORITY)) %>% # get the count
  ungroup() %>% 
  mutate(perc_min = round(num_min/popn_sum*100, 1))

### getting the number and percent of low income residents by HOLC grade (can't figure out how to get %)
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(popn_sum = sum(TOTALPOP, sort = TRUE),
            lowinc_num = sum(LOWINC)) %>% # get the count
  ungroup()  %>% 
  mutate(lowinc_perc = round(lowinc_num/popn_sum*100, 1))

ej_df_final %>%
  summarise(popn_sum = sum(TOTALPOP, sort = TRUE),
            lowinc_num = sum(LOWINC)) %>% # get the count
  ungroup() %>% 
  mutate(perc_lowinc = round(lowinc_num/popn_sum*100, 1))

### getting the number and percent of individuals with no HS diploma by HOLC grade (can't figure out how to get %)
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(popn_sum = sum(TOTALPOP, sort = TRUE),
            nohs_num = sum(NO_HS)) %>% # get the count
  ungroup()  %>% 
  mutate(nohs_perc = round(nohs_num/popn_sum*100, 1))

ej_df_final %>%
  summarise(popn_sum = sum(TOTALPOP, sort = TRUE),
            nohs_num = sum(NO_HS)) %>% # get the count
  ungroup() %>% 
  mutate(perc_nohs = round(nohs_num/popn_sum*100, 1))


### getting the median and IQR of pm2.5 by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_PM25), # get the mean
            q1 = quantile(RAW_E_PM25, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_PM25, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of pm2.5
ej_df_final %>%
  summarise(median = median(RAW_E_PM25), # get the mean
            q1 = quantile(RAW_E_PM25, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_PM25, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR of diesel by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_DIESEL), # get the mean
            q1 = quantile(RAW_E_DIESEL, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_DIESEL, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of diesel
ej_df_final %>%
  summarise(median = median(RAW_E_DIESEL), # get the mean
            q1 = quantile(RAW_E_DIESEL, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_DIESEL, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR of ozone by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_O3), # get the mean
            q1 = quantile(RAW_E_O3, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_O3, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of ozone
ej_df_final %>%
  summarise(median = median(RAW_E_O3), # get the mean
            q1 = quantile(RAW_E_O3, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_O3, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR of air toxics by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_CANCER), # get the mean
            q1 = quantile(RAW_E_CANCER, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_CANCER, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of air toxics
ej_df_final %>%
  summarise(median = median(RAW_E_CANCER), # get the mean
            q1 = quantile(RAW_E_CANCER, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_CANCER, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()


### getting the median and IQR of air toxics by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_CANCER), # get the mean
            q1 = quantile(RAW_E_CANCER, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_CANCER, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of air toxics
ej_df_final %>%
  summarise(median = median(RAW_E_CANCER), # get the mean
            q1 = quantile(RAW_E_CANCER, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_CANCER, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR of respiratory hazard index by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_RESP), # get the mean
            q1 = quantile(RAW_E_RESP, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_RESP, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of respiratory hazard index
ej_df_final %>%
  summarise(median = median(RAW_E_RESP), # get the mean
            q1 = quantile(RAW_E_RESP, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_RESP, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR of toxic releases to air by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_RSEI_AIR), # get the mean
            q1 = quantile(RAW_E_RSEI_AIR, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_RSEI_AIR, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of toxic releases to air
ej_df_final %>%
  summarise(median = median(RAW_E_RSEI_AIR), # get the mean
            q1 = quantile(RAW_E_RSEI_AIR, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_RSEI_AIR, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR of toxic releases to air by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_LEAD), # get the mean
            q1 = quantile(RAW_E_LEAD, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_LEAD, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR of toxic releases to air
ej_df_final %>%
  summarise(median = median(RAW_E_LEAD), # get the mean
            q1 = quantile(RAW_E_LEAD, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_LEAD, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR traffic proximity by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_TRAFFIC), # get the mean
            q1 = quantile(RAW_E_TRAFFIC, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_TRAFFIC, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR traffic proximity
ej_df_final %>%
  summarise(median = median(RAW_E_TRAFFIC), # get the mean
            q1 = quantile(RAW_E_TRAFFIC, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_TRAFFIC, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR superfund site proximity by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_NPL), # get the mean
            q1 = quantile(RAW_E_NPL, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_NPL, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR superfund site proximity
ej_df_final %>%
  summarise(median = median(RAW_E_NPL), # get the mean
            q1 = quantile(RAW_E_NPL, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_NPL, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()


### getting the median and IQR RMP facility proximity by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_RMP), # get the mean
            q1 = quantile(RAW_E_RMP, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_RMP, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR RMP facility proximity
ej_df_final %>%
  summarise(median = median(RAW_E_RMP), # get the mean
            q1 = quantile(RAW_E_RMP, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_RMP, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR hazardous waste facility proximity by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_TSDF), # get the mean
            q1 = quantile(RAW_E_TSDF, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_TSDF, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR hazardous waste facility proximity
ej_df_final %>%
  summarise(median = median(RAW_E_TSDF), # get the mean
            q1 = quantile(RAW_E_TSDF, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_TSDF, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR underground storage tank count by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_UST), # get the mean
            q1 = quantile(RAW_E_UST, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_UST, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR underground storage tank count
ej_df_final %>%
  summarise(median = median(RAW_E_UST), # get the mean
            q1 = quantile(RAW_E_UST, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_UST, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### getting the median and IQR wastewater discharge concentration by HOLC grade
ej_df_final %>%
  group_by(holc_grade) %>%
  summarise(median = median(RAW_E_NPDES), # get the mean
            q1 = quantile(RAW_E_NPDES, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_NPDES, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

### get the overall median and IQR wastewater discharge concentration
ej_df_final %>%
  summarise(median = median(RAW_E_NPDES), # get the mean
            q1 = quantile(RAW_E_NPDES, probs = 0.25), # get the 25th percentile
            q3 = quantile(RAW_E_NPDES, probs = 0.75) # get the 75th percentile
  ) %>%
  ungroup()

#### end of table 1




### create figure 1
#### get data we are interested in
ej_df_trial <- ej_reg_sf %>%
  as_tibble() %>%
  mutate(red = ifelse(holc_grade == "D", 1, 0)) %>%
  select(neighborho, red, state, city, holc_grade, RAW_E_LEAD:RAW_E_RSEI_AIR, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  mutate(state = factor(state), city = factor(city)) %>%
  na.omit()


# convert the data into within-city percentiles
ej_df_trial_percentile <- ej_df_trial %>%
  gather(env, value, -c(neighborho, red, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS)) %>%
  group_by(city, state, env) %>%
  mutate(city_percentile = rank(value)/length(value)) %>%
  ungroup() %>%
  select(-value) %>%
  spread(env, city_percentile)

# percentage of holc_grades
ej_df_trial_percentile %>%
  group_by(holc_grade) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n))
# 23.5% of neighborhoods are D-graded


# checkout the data to see if it roughly follows a uniform distribution
ej_df_trial_percentile$RAW_E_NPL %>% hist()


fig1_df <- ej_df_trial_percentile %>%
  select(-RAW_D_LOWINC, -RAW_D_NO_HS) %>%
  gather(var, percentile, -c(neighborho:holc_grade)) %>%
  group_by(holc_grade, neighborho) %>%
  summarise(percentile = mean(percentile, na.rm = TRUE)) %>%
  ungroup() %>%
  #.$percentile %>% summary()
  mutate(quantile_var_breaks = cut(percentile, seq(0, 1, by = 0.1))) %>%
  group_by(holc_grade, quantile_var_breaks) %>%
  summarise(n = sum(percentile)) %>%
  mutate(percentage = n/sum(n)) %>%
  ungroup() %>%
  mutate(quantile_var_breaks = 10*(as.numeric(quantile_var_breaks)-1)) %>%
  mutate(count = ifelse(quantile_var_breaks == 80, 2, 1)) %>%
  uncount(count) %>%
  group_by(holc_grade, quantile_var_breaks) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  mutate(quantile_var_breaks = ifelse(count == 2, 90, quantile_var_breaks)) %>%
  print(n = nrow(.))

fig1_df %>%
  filter(quantile_var_breaks != 90) %>%
  group_by(quantile_var_breaks) %>%
  mutate(cumsum = percentage/sum(percentage)) %>%
  arrange(quantile_var_breaks) %>%
  print(n = nrow(.))




# Plot
fig_1a <- fig1_df %>%
  ggplot(aes(x=quantile_var_breaks, y=percentage)) + 
  geom_area(aes(fill=holc_grade), position = "fill") +
  geomtextpath::geom_texthline(size = 3, yintercept = 0.235, linetype = "dashed", label = "Nationwide proportion of D-graded neighborhoods", hjust = 0.1) +
  #annotate("text", x = 10.5, y = 0.21, , hjust = 0, size = 3.5) +
  geom_text(data = tibble(name = c("Grade D", "Grade C", "Grade B", "Grade A"),
                          quantile_var_breaks = c(89, 89, 89, 68.5),
                          percentage = c(0.61, 0.88, 0.975, 0.975)),
            aes(label = name), size = 3, hjust = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_cartesian(xlim = c(4, 86), ylim = c(0.043, 0.957)) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#e8e580", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  theme_classic() +
  theme(#axis.title = element_text(size = 10, face = "italic"),
    #axis.text = element_text(size = 12),
    #legend.title = element_text(face = "bold"),
    #legend.key.width = unit("0.75", "cm")
  ) +
  labs(
    title = "Share of Cumulative Environmental Exposures by HOLC Grade", 
    subtitle = "Nationwide",
    x = "Neighborhood's average environmental exposure percentile", 
    y = "Proportion of neighborhoods")# +
#theme(legend.position = "none")

fig_1a + theme(legend.position = "none")

