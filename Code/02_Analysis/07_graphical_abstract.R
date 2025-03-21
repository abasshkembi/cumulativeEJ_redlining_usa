# making second part of figure 1...

library(ggmap)
library(tidyverse)
library(sf)
ggmap::register_stadiamaps("b053b5be-46d3-40e7-8e16-2cff4f826149", write = TRUE)

# read in the data from 03b_data_prep.R
ej_reg_sf






# visualize cumulative impacts

set.seed(4234+200); boostrap_i_train <- sample(1:nrow(ej_df_trial_percentile), nrow(ej_df_trial_percentile), replace = TRUE); df_gbm_boostrap_train <- ej_df_trial_percentile[boostrap_i_train,]

boost_k_red_detroit <- readRDS(file = paste0("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Redlining EJ USA/Code/02_Analysis/gbm bootstraps/gbm_redlining_env_i", 200, "_", "all", ".rds"))

detroit_probs <- boost_k_red_detroit %>%
  pdp::partial(plot=FALSE, train = df_gbm_boostrap_train, n.trees = 500, pred.var = c(mean_cols[-1], 
                                                                                      "RAW_D_NO_HS", 
                                                                                      "RAW_D_LOWINC"),
               pred.grid = ej_df_trial_percentile %>% filter(city == "Detroit") %>% select(all_of(c(mean_cols[-1], "RAW_D_NO_HS", "RAW_D_LOWINC")))
  ) %>%
  transmute(probs = 1/(1+exp(-yhat))) %>%
  cbind(ej_df_trial_percentile %>% filter(city == "Detroit"))






detroit_df <- all_redlined_shapefiles %>%
  filter(neighborho %in% detroit_probs$neighborho) %>%
  #left_join(detroit_probs %>% select(neighborho, probs), by = "neighborho") %>%
  rowwise() %>%
  mutate(probs = case_when(
    holc_grade == "A" ~ runif(1, 0, 0.1),
    holc_grade == "B" ~ runif(1, 0, 0.1),
    holc_grade == "C" ~ runif(1, 0, 0.4),
    holc_grade == "D" ~ runif(1, 0.5, 1)
  ))

nc_map <- get_stadiamap(location = "Detroit, MI", zoom = 7)

ggmap(nc_map)

fig0_bbox <- unname(st_bbox(detroit_df))
fig0_bbox2 <- c(bottom = fig0_bbox[2], top = fig0_bbox[4] + 0.05, right = fig0_bbox[3]+0.01, left = fig0_bbox[1])
fig0_ggmap <- get_stadiamap(maptype = "stamen_toner_lite", bbox = fig0_bbox2, zoom = 11)

ggmap::ggmap(fig0_ggmap) +
  geom_sf(data = detroit_df, aes(fill = probs), color = "grey", linewidth = 0.1, inherit.aes = F,
          alpha = 0.75) +
  #scale_fill_distiller(palette = "YlOrRd", 
  #                     direction = 1, 
  #                     name = expression("Higher cumulative exposures" %->% "")) +
  scale_fill_gradient2(low = "#FFFFBB",
                       mid = "#F4BF7F",
                       high = "#832B42",
                       midpoint = 0.5,
                       name = expression("Higher cumulative exposures" %->% "")) +
  theme_void() +
  theme(legend.text = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top", 
        legend.key.width = unit(0.1, "npc"),
        legend.title.position = "top", 
        legend.title = element_text(hjust = 0.5, size = 14))