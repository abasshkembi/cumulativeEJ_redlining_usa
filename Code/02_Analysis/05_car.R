

library(tidyverse)
library(spaMM)
library(spdep)
library(RSpectra)
library(sf)

# read in the data from 03b_data_prep.R
ej_reg_sf


# begin regression

######### diesel PM

ej_reg_sf$RAW_E_DIESEL %>% hist()
# this is right skewed!

log(ej_reg_sf$RAW_E_DIESEL) %>% hist()
# yay - this is normal now

df_reg_diesel <- ej_reg_sf %>%
  # log transform diesel
  mutate(log_diesel = log(RAW_E_DIESEL)) %>% 
  # select relevant variables for regression
  select(neighborho, log_diesel, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_diesel)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_diesel, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_diesel <- fitme(log_diesel ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                            adjMatrix = adj_matrix,
                            data = df_reg_diesel, 
                            family = 'gaussian', 
                            method = "REML")
summary(mod_spatial_diesel) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_diesel) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_diesel),
           nb2listw(poly2nb(df_reg_diesel, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.25
# this is okay! not exactly zero but much closer to 0 than 1

# we can plot the residuals for Detroit as an example to see 
df_reg_diesel %>%
  mutate(residuals = resid(mod_spatial_diesel)) %>%
  filter(city == "Detroit") %>%
  ggplot(aes(fill = residuals)) +
  geom_sf() +
  scale_fill_viridis_b()



# get effect estimates (relative risks)

# by exponentiating our log-outcome, we now have relative risks
# this means our null is equal to 1
data.frame(summary(mod_spatial_diesel, verbose = F)["beta_table"]) %>%
  transmute(RR = round(exp(beta_table.Estimate), 3),
            q2.5 = round(exp(beta_table.Estimate - (1.96*beta_table.Cond..SE)), 3),
            q97.5 = round(exp(beta_table.Estimate + (1.96*beta_table.Cond..SE)), 3))

# make the relative risk transformation into a formula to repeat for all models
# and add grade-A back in as a reference for plotting purposes
RR_fitme <- function(mods, round=4, exp = TRUE) {
  
  if(exp == TRUE) {
    temp_df <- data.frame(summary(get(mods), verbose = F)["beta_table"]) %>%
      transmute(predictor = rownames(.),
                RR = round(exp(beta_table.Estimate),round),
                q2.5 = round(exp(beta_table.Estimate - (1.96*beta_table.Cond..SE)),round),
                q97.5 = round(exp(beta_table.Estimate + (1.96*beta_table.Cond..SE)),round))
  } else {
    temp_df <- data.frame(summary(get(mods), verbose = F)["beta_table"])
    intercept_value <- temp_df[1,1]
    temp_df <- temp_df %>%
      transmute(predictor = rownames(.),
                RR = round(beta_table.Estimate/intercept_value + 1,round),
                q2.5 = round((beta_table.Estimate/intercept_value) - (1.96*beta_table.Cond..SE/intercept_value) + 1,round),
                q97.5 = round((beta_table.Estimate/intercept_value) + (1.96*beta_table.Cond..SE/intercept_value) + 1,round))
  }
  
  
  
  temp_df %>%
    as_tibble() %>%
    filter(str_detect(predictor, "holc_grade")) %>%
    rbind(tibble(predictor = "holc_gradeA",
                 RR = 1,
                 q2.5 = 1,
                 q97.5 = 1)) %>%
    mutate(predictor = str_remove(as.character(predictor), "holc_grade")) %>%
    arrange(predictor) %>%
    mutate(predictor = factor(predictor)) %>%
    # multiply them by 100 to get percent difference
    mutate(RR = 100*(RR-1),
           q2.5 = 100*(q2.5-1),
           q97.5 = 100*(q97.5-1))
}

diesel_RR <- RR_fitme("mod_spatial_diesel", round = 3, exp = TRUE)
diesel_RR

diesel_p <- diesel_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Diesel PM", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

diesel_p

# we see that redlined neighborhoods have 4.8% higher levels of diesel PM compared
# to A-graded neighborhoods across the US
# this is a statistically significant association, as the 
# 95% CI does not include 0 (3.4 to 6.3%)






# AS AN ASIDE FOR YOUR LEARNING
# what if we did not run a spatial model?
# let's try running a simple linear regression model

#including HOLC Grade A as the reference
mod_nonspatial_diesel <- lm(log_diesel ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10), 
                            data = df_reg_diesel)
summary(mod_nonspatial_diesel) # summary output of model

resid(mod_nonspatial_diesel) %>% summary()

# let's check the residual spatial autocorrelation of our nonspatial model
moran.test(resid(mod_nonspatial_diesel),
           nb2listw(poly2nb(df_reg_diesel, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.82!!
# this is not okay! the value is much closer to 1, not 0, 
# indicating that we still have a lot of spatial autocorrelation in our model
# that we did not account for, violating one of the assumptions of linear regression
# (that observations of Y (diesel), are indepednent from one another)

# we can plot the residuals for Detroit as an example to see high residual spatial autocorrelation
df_reg_diesel %>%
  mutate(residuals = resid(mod_nonspatial_diesel)) %>%
  filter(city == "Detroit") %>%
  ggplot(aes(fill = residuals)) +
  geom_sf() +
  scale_fill_viridis_b()

# compared to the previous spatial model plot, you can see clusters of very high or very low 
# residuals. this is indicative of high residual spatial autocorrelation
# we want the residuals to be "randomly" distributed in space. We can see that this is not
# the case in this map, but was the case in our spatial model map












######### PM2.5

ej_reg_sf$RAW_E_PM25 %>% hist()
# this is normal!
# no need to log transform PM2.5

df_reg_pm25 <- ej_reg_sf %>%
  # select relevant variables for regression
  select(neighborho, RAW_E_PM25, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_pm25, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_pm25 <- fitme(RAW_E_PM25 ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                          adjMatrix = adj_matrix,
                          data = df_reg_pm25, 
                          family = 'gaussian', 
                          method = "REML")
summary(mod_spatial_pm25) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_pm25) %>% summary()

# check the residual spatial autocorrelation again
moran.test(resid(mod_spatial_pm25),
           nb2listw(poly2nb(df_reg_pm25, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I of 0.27 - not bad - it's close to 0



# get effect estimates (relative risks)
pm25_RR <- RR_fitme("mod_spatial_pm25", round = 3, exp = FALSE)
pm25_RR


pm25_p <- pm25_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "PM 2.5", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

pm25_p

# we see that redlined neighborhoods have 1.2% higher levels of PM2.5 compared
# to A-graded neighborhoods across the US
# however, this is not a statistically significant association, as the 
# 95% CI includes 0 (-0.3 to 2.7%)





######### Ozone
ej_reg_sf$RAW_E_O3 %>% hist()
# looks pretty dang normal to me

df_reg_o3 <- ej_reg_sf %>%
  # select relevant variables for regression
  select(neighborho, RAW_E_O3, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_o3, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL


############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_o3 <- fitme(RAW_E_O3 ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                        adjMatrix = adj_matrix,
                        data = df_reg_o3, 
                        family = 'gaussian', 
                        method = "REML")
summary(mod_spatial_o3) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_o3) %>% summary()

#check for spatial autocorrelation in residuals

moran.test(resid(mod_spatial_o3),
           nb2listw(poly2nb(df_reg_o3, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.27

# get effect estimates (relative risks)
o3_RR <- RR_fitme("mod_spatial_o3", round = 3, exp = FALSE)
o3_RR


o3_p <- o3_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "O3", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

o3_p

# redlined neighborhoods have 5% lower levels of O3 compared
# to A-graded neighborhoods across the US
# however, this is not a statistically significant association, as the 
# 95% CI includes 0 (~0.8% to ~-17%)








######### Air toxics cancer risk
ej_reg_sf$RAW_E_CANCER %>% hist()
# weird looking right skew

log(ej_reg_sf$RAW_E_CANCER) %>% hist()
# bimodal distribution - is this a problem?

# ABAS - just try running the same models with log-transformed outcome here

df_reg_cancer <- ej_reg_sf %>%
  # select relevant variables for regression
  select(neighborho, RAW_E_CANCER, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  mutate(log_cancer = log(RAW_E_CANCER)) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove NAs
  na.omit() %>%
  filter(!is.infinite(log_cancer))


# create queen-based neigborhoods
nb <- poly2nb(df_reg_cancer, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_cancer <- fitme(log_cancer ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                            adjMatrix = adj_matrix,
                            data = df_reg_cancer, 
                            family = 'gaussian', 
                            method = "REML")
summary(mod_spatial_cancer) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_cancer) %>% summary()

# check the residual spatial autocorrelation again
moran.test(resid(mod_spatial_cancer),
           nb2listw(poly2nb(df_reg_cancer, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I of 0.199 - not bad - it's close to 0



# get effect estimates (relative risks)
cancer_RR <- RR_fitme("mod_spatial_cancer", round = 3, exp = TRUE)
cancer_RR


cancer_p <- cancer_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Air toxics cancer risk", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

cancer_p





#### respiratory hazard index

ej_reg_sf$RAW_E_RESP %>% summary()
# weird looking right skew

ej_reg_sf %>%
  arrange(-RAW_E_RESP) %>%
  select(RAW_E_RESP) %>%
  filter(RAW_E_RESP < 2) # this would drop the outliers

# run all the code twice for two different data frames (one with the full data, and one with the outliers dropped)

log(ej_reg_sf$RAW_E_RESP) %>% hist()
# not much better

# ABAS - do a sensitivity analysis (running the regression) with and without the one outlier
#.       keep the log-transformed outcome
#.       check your effect estimates and how different they are
#.       check your residual spatial autocorrelation and see if keeping the outlier really increases
#.       the Moran's I


df_reg_resp <- ej_reg_sf %>%
  # select relevant variables for regression
  select(neighborho, RAW_E_RESP, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  mutate(log_resp = log(RAW_E_RESP)) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove NAs
  na.omit() %>%
  filter(!is.infinite(log_resp))


# create queen-based neigborhoods
nb <- poly2nb(df_reg_resp, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_resp <- fitme(log_resp ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                          adjMatrix = adj_matrix,
                          data = df_reg_resp, 
                          family = 'gaussian', 
                          method = "REML")
summary(mod_spatial_resp) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_resp) %>% summary()

# check the residual spatial autocorrelation again
moran.test(resid(mod_spatial_resp),
           nb2listw(poly2nb(df_reg_resp, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I of 0.19 - not bad - it's close to 0



# get effect estimates (relative risks)
resp_RR <- RR_fitme("mod_spatial_resp", round = 3, exp = TRUE)
resp_RR


resp_p <- resp_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Resp. hazard", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

resp_p



#### toxic releases to air

ej_reg_sf$RAW_E_RSEI_AIR %>% hist()
# very strong right skew

log(ej_reg_sf$RAW_E_RSEI_AIR) %>% hist()
# now normal

df_reg_rsei <- ej_reg_sf %>%
  # log transform rsei
  mutate(log_rsei = log(RAW_E_RSEI_AIR)) %>% 
  # select relevant variables for regression
  select(neighborho, log_rsei, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_rsei)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_rsei, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_rsei <- fitme(log_rsei ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                          adjMatrix = adj_matrix,
                          data = df_reg_rsei, 
                          family = 'gaussian', 
                          method = "REML")
summary(mod_spatial_rsei) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_rsei) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_rsei),
           nb2listw(poly2nb(df_reg_rsei, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.25
# this is okay! not exactly zero but much closer to 0 than 1

# get effect estimates (relative risks)
rsei_RR <- RR_fitme("mod_spatial_rsei", round = 3, exp = TRUE)
rsei_RR


rsei_p <- rsei_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "RSEI", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

rsei_p
# redlined neighborhoods have 14% higher levels of toxic releases to air compared
# to A-graded neighborhoods across the US
# and statistically significant since confidence interval does not include 0 (10 - 20%)






##### Lead paint indicator

ej_reg_sf$RAW_E_LEAD %>% hist()
# left skew - this is fine!

log(ej_reg_sf$RAW_E_LEAD) %>% hist()
# even more aggressive left skew

# ABAS - in this case, use the non-log-transformed outcome
# BUT don't use the RR_fitme() function as this is exponentiating our effect estimates
# just copy that function code and rerun it for this model without the exp() in the transmute() line


df_reg_lead <- ej_reg_sf %>%
  # log transform rsei
  # select relevant variables for regression
  select(neighborho, RAW_E_LEAD, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_lead, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_lead <- fitme(RAW_E_LEAD ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                          adjMatrix = adj_matrix,
                          data = df_reg_lead, 
                          family = 'gaussian', 
                          method = "REML")
summary(mod_spatial_lead) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_lead) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_lead),
           nb2listw(poly2nb(df_reg_lead, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.04
# this is okay! not exactly zero but much closer to 0 than 1

# get effect estimates (relative risks)
lead_RR <- RR_fitme("mod_spatial_lead", round = 3, exp = FALSE)
lead_RR


lead_p <- lead_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Lead indicator", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

lead_p


#################### PROXIMITIES ###########################

##### TRAFFIC

ej_reg_sf$RAW_E_TRAFFIC %>% hist()
# right skew

log(ej_reg_sf$RAW_E_TRAFFIC) %>% hist()
# normal!!!


df_reg_traffic <- ej_reg_sf %>%
  # log transform traffic
  mutate(log_traffic = log(RAW_E_TRAFFIC)) %>% 
  # select relevant variables for regression
  select(neighborho, log_traffic, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_traffic)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_traffic, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_traffic <- fitme(log_traffic ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                             adjMatrix = adj_matrix,
                             data = df_reg_traffic, 
                             family = 'gaussian', 
                             method = "REML")
summary(mod_spatial_traffic) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_traffic) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_traffic),
           nb2listw(poly2nb(df_reg_traffic, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.07


# get effect estimates (relative risks)
traffic_RR <- RR_fitme("mod_spatial_traffic", round = 3, exp = TRUE)
traffic_RR


traffic_p <- traffic_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Traffic", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

traffic_p
# redlined neighborhoods have 35% (!!) closer proximity to traffic compared
# to A-graded neighborhoods across the US
# and statistically significant since confidence interval does not include 0 (27 - 43%)




##### SUPERFUND SITES

ej_reg_sf$RAW_E_NPL %>% hist()
# right skew

log(ej_reg_sf$RAW_E_NPL) %>% hist()
# normalish (slight right skew)


df_reg_npl <- ej_reg_sf %>%
  # log transform NPL
  mutate(log_npl = log(RAW_E_NPL)) %>% 
  # select relevant variables for regression
  select(neighborho, log_npl, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_npl)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_npl, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_npl <- fitme(log_npl ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                         adjMatrix = adj_matrix,
                         data = df_reg_npl, 
                         family = 'gaussian', 
                         method = "REML")
summary(mod_spatial_npl) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_npl) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_npl),
           nb2listw(poly2nb(df_reg_npl, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.27


# get effect estimates (relative risks)
npl_RR <- RR_fitme("mod_spatial_npl", round = 3, exp = TRUE)
npl_RR


npl_p <- npl_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Superfund sites", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

npl_p
# redlined neighborhoods have 10% closer proximity to superfund sites compared
# to A-graded neighborhoods across the US
# and statistically significant since confidence interval does not include 0 (6.5 - 16%)




##### RMP facilities

ej_reg_sf$RAW_E_RMP %>% hist()
# right skew

log(ej_reg_sf$RAW_E_RMP) %>% hist()
# normal


df_reg_rmp <- ej_reg_sf %>%
  # log transform NPL
  mutate(log_rmp = log(RAW_E_RMP)) %>% 
  # select relevant variables for regression
  select(neighborho, log_rmp, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_rmp)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_rmp, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_rmp <- fitme(log_rmp ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                         adjMatrix = adj_matrix,
                         data = df_reg_rmp, 
                         family = 'gaussian', 
                         method = "REML")
summary(mod_spatial_rmp) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_rmp) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_rmp),
           nb2listw(poly2nb(df_reg_rmp, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.25


# get effect estimates (relative risks)
rmp_RR <- RR_fitme("mod_spatial_rmp", round = 3, exp = TRUE)
rmp_RR


rmp_p <- rmp_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "RMP sites", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

rmp_p
# redlined neighborhoods have 19% closer proximity to RMP sites compared
# to A-graded neighborhoods across the US
# and statistically significant since confidence interval does not include 0 (13 - 26%)




##### Hazardous waste (TSDF)

ej_reg_sf$RAW_E_TSDF %>% hist()
# right skew

log(ej_reg_sf$RAW_E_TSDF) %>% hist()
# normal

df_reg_tsdf <- ej_reg_sf %>%
  # log transform tsdf
  mutate(log_tsdf = log(RAW_E_TSDF)) %>% 
  # select relevant variables for regression
  select(neighborho, log_tsdf, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_tsdf)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_tsdf, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_tsdf <- fitme(log_tsdf ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                          adjMatrix = adj_matrix,
                          data = df_reg_tsdf, 
                          family = 'gaussian', 
                          method = "REML")
summary(mod_spatial_tsdf) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_tsdf) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_tsdf),
           nb2listw(poly2nb(df_reg_tsdf, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.25


# get effect estimates (relative risks)
tsdf_RR <- RR_fitme("mod_spatial_tsdf", round = 3, exp = TRUE)
tsdf_RR


tsdf_p <- tsdf_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Hazardous waste sites", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

tsdf_p
# redlined neighborhoods have 26.5% closer proximity to hazardous waste sites compared
# to A-graded neighborhoods across the US
# and statistically significant since confidence interval does not include 0 (20 - 33%)








##### underground storage tanks

ej_reg_sf$RAW_E_UST %>% hist()
# right skew

log(ej_reg_sf$RAW_E_UST) %>% hist()
# ....left skew

df_reg_ust <- ej_reg_sf %>%
  # log transform tsdf
  mutate(log_ust = log(RAW_E_UST)) %>% 
  # select relevant variables for regression
  select(neighborho, log_ust, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_ust)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_ust, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_ust <- fitme(log_ust ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                         adjMatrix = adj_matrix,
                         data = df_reg_ust, 
                         family = 'gaussian', 
                         method = "REML")
summary(mod_spatial_ust) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_ust) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_ust),
           nb2listw(poly2nb(df_reg_ust, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.01


# get effect estimates (relative risks)
ust_RR <- RR_fitme("mod_spatial_ust", round = 3, exp = TRUE)
ust_RR


ust_p <- ust_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Underground storage tanks", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

ust_p





##### Wastewater discharge

ej_reg_sf$RAW_E_NPDES %>% hist()
# right skew

log(ej_reg_sf$RAW_E_NPDES) %>% hist()
# normal

df_reg_npdes <- ej_reg_sf %>%
  # log transform tsdf
  mutate(log_npdes = log(RAW_E_NPDES)) %>% 
  # select relevant variables for regression
  select(neighborho, log_npdes, holc_grade, state, city, RAW_D_LOWINC, RAW_D_NO_HS) %>%
  #create new column called city_state that combines city and state info
  mutate(city_state = as.factor(paste0(city, state))) %>%
  # remove non-possible values like infinity
  filter(!is.infinite(log_npdes)) %>%
  # remove NAs
  na.omit()


# create queen-based neigborhoods
nb <- poly2nb(df_reg_npdes, queen = TRUE)
# create adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
row.names(adj_matrix) <- NULL

############### MODELLING

#including HOLC Grade A as the reference
mod_spatial_npdes <- fitme(log_npdes ~ holc_grade + I(RAW_D_LOWINC/10) + I(RAW_D_NO_HS/10) + (1|city_state) + (1|state) + adjacency(1|neighborho), 
                           adjMatrix = adj_matrix,
                           data = df_reg_npdes, 
                           family = 'gaussian', 
                           method = "REML")
summary(mod_spatial_npdes) # summary output of model

# we want the residuals to be randomly distributed about 0
resid(mod_spatial_npdes) %>% summary()

# we want very low spatal autocorrelation in the residuals
# we measure this with Moran's I, which ranges from -1 to 1 like regular correlation
# we want the value to be close to 0
moran.test(resid(mod_spatial_npdes),
           nb2listw(poly2nb(df_reg_npdes, queen = TRUE), style = "B", zero.policy = TRUE),
           zero.policy = TRUE)
# moran's I = 0.20


# get effect estimates (relative risks)
npdes_RR <- RR_fitme("mod_spatial_npdes", round = 3)
npdes_RR


npdes_p <- npdes_RR %>%
  ggplot(aes(x = predictor, y = RR, fill = predictor)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
  scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                    name = "HOLC Grade",
                    labels = c(
                      "A" = "Grade A",
                      "B" = "Grade B",
                      "C" = "Grade C",
                      "D" = "Grade D"
                    )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw() +
  labs(title = "Wastewater discharge", 
       y = "Percent difference (%)", x= "HOLC Grade") +
  theme(axis.title = element_text(face = "italic", size = 10),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit("1.5", "cm")
  )

npdes_p
# redlined neighborhoods have 66.1% closer proximity to RMP sites compared
# to A-graded neighborhoods across the US
# and statistically significant since confidence interval does not include 0 (30.9 - 111%) (?)


mod_spatial_cancer
mod_spatial_diesel
mod_spatial_lead
mod_spatial_npdes
mod_spatial_npl
mod_spatial_o3
mod_spatial_pm25
mod_spatial_resp
mod_spatial_rmp
mod_spatial_rsei
mod_spatial_traffic
mod_spatial_tsdf
mod_spatial_ust

all_mod_results <- cancer_RR %>% mutate(variable = "cancer") %>%
  rbind(diesel_RR %>% mutate(variable = "diesel")) %>%
  rbind(lead_RR %>% mutate(variable = "lead")) %>%
  rbind(npdes_RR %>% mutate(variable = "npdes")) %>%
  rbind(npl_RR %>% mutate(variable = "npl")) %>%
  rbind(o3_RR %>% mutate(variable = "ozone")) %>%
  rbind(pm25_RR %>% mutate(variable = "pm25")) %>%
  rbind(resp_RR %>% mutate(variable = "resp")) %>%
  rbind(rmp_RR %>% mutate(variable = "rmp")) %>%
  rbind(rsei_RR %>% mutate(variable = "rsei")) %>%
  rbind(traffic_RR %>% mutate(variable = "traffic")) %>%
  rbind(tsdf_RR %>% mutate(variable = "tsdf")) %>%
  rbind(ust_RR %>% mutate(variable = "ust"))


######### saveRDS(all_mod_results, "Code/02_Analysis/spatial_models_results.Rds")

all_mod_results <- readRDS("Code/02_Analysis/spatial_models_results.Rds")


fig1_gg <- function(var, title) {
  temp_df <- all_mod_results %>% filter(variable == var)
  
  temp_df %>%
    ggplot(aes(x = predictor, y = RR, fill = predictor)) +
    geom_col(color = "black", width = 0.7) +
    geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.2) +
    scale_fill_manual(values = c("#68947b", "#5fa0b4", "#f3f1af", "#c88c91"),
                      name = "HOLC Grade",
                      labels = c(
                        "A" = "Grade A",
                        "B" = "Grade B",
                        "C" = "Grade C",
                        "D" = "Grade D"
                      )) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme_bw() +
    labs(title = title, 
         y = "Percent difference (%)", x= "HOLC Grade") +
    theme(axis.title = element_text(face = "italic", size = 10),
          axis.text = element_text(size = 12),
          legend.position = "none",
          legend.direction = "horizontal",
          legend.title = element_text(face = "bold"),
          legend.key.width = unit("1.5", "cm")
    )
}

fig1_gg("cancer", "Air tox. cancer risk")

pm25_p <- fig1_gg("pm25", "PM 2.5")
diesel_p <- fig1_gg("diesel", "Diesel PM")
o3_p <- fig1_gg("ozone", "Ozone")
rsei_p <- fig1_gg("rsei", "Air tox. release")

cancer_p <- fig1_gg("cancer", "Air cancer risk")
resp_p <- fig1_gg("resp", "Resp. hazard")
lead_p <- fig1_gg("lead", "Lead indicator")
traffic_p <- fig1_gg("traffic", "Traffic")
npl_p <- fig1_gg("npl", "Superfund sites")
rmp_p <- fig1_gg("rmp", "RMP sites")
tsdf_p <- fig1_gg("tsdf", "Haz. waste sites")
ust_p <- fig1_gg("ust", "Udg. storage tanks")
npdes_p <- fig1_gg("npdes", "Wastewater d/c")






















# supplemental figure of individual relationships

ggarrange(
  pm25_p + coord_cartesian(ylim = c(-8.3, 8.3))+ labs(x = NULL, y = "% difference", title = expression('PM'[2.5])) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()), 
  diesel_p + coord_cartesian(ylim = c(-21, 21)) + labs(x = NULL, y = NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()), 
  o3_p + coord_cartesian(ylim = c(-5, 5)) + labs(x = NULL, y = NULL, title = "Ozone") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()), 
  rsei_p + coord_cartesian(ylim = c(-58, 58)) + labs(x = NULL, y = "% difference", title = "Air tox. release") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  cancer_p + coord_cartesian(ylim = c(-27, 27)) + labs(x = NULL, y = "% difference", title = "Air cancer risk") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  resp_p + coord_cartesian(ylim = c(-27, 27)) + labs(x = NULL, y = NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  lead_p  + coord_cartesian(ylim = c(-25, 25)) + labs(x = NULL, y = "% difference") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  traffic_p + coord_cartesian(ylim = c(-45, 45)) + labs(x = NULL, y = NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  npl_p  + coord_cartesian(ylim = c(-46, 46)) + labs(x = NULL, y = NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  rmp_p + coord_cartesian(ylim = c(-50, 50)) + labs(x = NULL, y = "% difference") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  tsdf_p + coord_cartesian(ylim = c(-54, 54)) + labs(x = NULL, y = NULL, title = "Haz. waste sites") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  ust_p + coord_cartesian(ylim = c(-54, 54)) + labs(x = NULL, y = NULL, title = "Udg. storage tanks") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  npdes_p  + coord_cartesian(ylim = c(-110, 110)) + labs(x = NULL, y = "% difference", title = "Wastewater d/c") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()),
  as_ggplot(ggpubr::get_legend(fig_1a)),
  
  ncol = 3,
  nrow = 5
)
