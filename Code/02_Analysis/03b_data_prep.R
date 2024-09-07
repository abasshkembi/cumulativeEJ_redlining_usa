library(tidyverse)
library(sf)

username <- "abasshkembi" 
working_directory <- paste0("/Users/", username, "/University of Michigan Dropbox/Abas Shkembi/Redlining EJ USA/cumulativeEJ_redlining_usa/")
setwd(working_directory)

# read in the processed data
ej_df_final <- readRDS("EJScreen data/Processed data/ejscreen_red_final.Rds")

# get a summary of all the columns
summary(ej_df_final)

all_redlined_shapefiles <- sf::read_sf("HOLC Shapefiles/fullshpfile/shapefile/holc_ad_data.shp")
all_redlined_shapefiles <- st_make_valid(all_redlined_shapefiles)


# this is the total neighborhoods we SHOULD have
unique_sf_neighborhoods <- unique(all_redlined_shapefiles$neighborho)
# these are the neighborhoods we DO have
unique_df_neighborhoods <- unique(ej_df_final$neighborho)

length(unique_sf_neighborhoods) - length(unique_df_neighborhoods)
#[1] 112


missing_neigh_trial <- unique_sf_neighborhoods[which(!(unique_sf_neighborhoods %in% unique_df_neighborhoods))]
# [1] 3948 3929 7175 3920 3945 3951 3734 3773 3805 3725 4016 4072 9576 9577 8410 8402 8319 1033 1056 4577 4609
# [22] 3487 4094 3840 4127 8239 4217 2964 8196 6644 2917 4239 4273 4241 3366 3372 3375 4850 4849 8027 8108 5612
# [43] 5608 4553 6846 9059  158  260  898  913  899 9586 5499 9544 7242 7192 2614 2604 2527 2539 2542 2544 2545
# [64] 2546 2537 2543 2352 2355  444  545  150 8777 8778 8779 8739 8757 8744 8746 8753 9431 9201 6410 2200 2199
# [85] 2145 2031 2040 8628 8645 8646 8897 8898 8935 8939 8940 8971 8972 9169 9015 2024 1992 1819 3908 6321 6280
# [106] 5328 5327 1690 1673 9224 9247 9256

length(missing_neigh_trial)
#112 missing

all_redlined_shapefiles %>% 
  left_join(ej_df_final, by = "neighborho") %>%
  as_tibble() %>%
  filter(neighborho %in% missing_neigh_trial)
# all of these neighborhoods have no one living in them, so they are dropped from the analysis




# we have a duplicate neighborhood
ej_reg_sf <- all_redlined_shapefiles %>%
  filter(!(neighborho %in% missing_neigh_trial)) %>%
  select(neighborho, state, city, holc_id, holc_grade) %>%
  left_join(ej_df_final, by = c("neighborho", "state", "city", "holc_id", "holc_grade"))

# get row where neighborhood is duplicated
which(ej_reg_sf$neighborho == 8678)
#[1] 1613 1614

# remove the second instance which has no data
ej_reg_sf <- ej_reg_sf[-1614,]




# add in the metropolitan/nonmetro areas for the gbm analysis

mnm <- st_read("EJScreen data/Helper data/msashape2/OES 2019 Shapefile.shp")
mnm <- mnm %>% st_transform("EPSG:4326")
mnm2 <- mnm %>% st_make_valid()

ej_centroids <- ej_reg_sf %>%
  select(neighborho, state, city, holc_id) %>%
  st_centroid()

red_in_mnm <- spatialEco::point.in.poly(x = ej_centroids, y = mnm2, sp = FALSE, duplicate = TRUE)


red_in_mnm %>%
  as_tibble() %>%
  filter(!is.na(msa7)) %>%
  group_by(city, state, msa7) %>%
  count() %>%
  ungroup %>%
  group_by(city, state) %>%
  mutate(n2 = n()) %>%
  filter(n2 > 1)
# there are two cities where there are more than one mnm assigned
### assigning Harrisburg PA to 25420 --- Harrisburg-Carlisle, PA
### assigning New Haven CT to 75700 --- New Haven, CT

red_mnm_crosswalk <- red_in_mnm %>%
  as_tibble() %>%
  mutate(msa7 = ifelse(msa7 == "49620", "25420", msa7),
         msa7 = ifelse(msa7 == "71950", "75700", msa7)) %>%
  filter(!is.na(msa7)) %>%
  group_by(city, state, msa7) %>%
  count() %>%
  ungroup() %>%
  select(-n)

# should have 202 rows


mnm_descriptions <- readxl::read_xlsx("EJScreen data/Helper data/area_definitions_m2019.xlsx") %>%
  select(`May 2019 MSA code`, `May 2019 MSA name`, `State abbreviation`) %>%
  rename(msa7 = 1, msa_name = 2, state = 3) %>%
  distinct()

red_mnm_crosswalk <- red_mnm_crosswalk %>%
  left_join(mnm_descriptions, by = c("msa7", "state"))


ej_reg_sf <- ej_reg_sf %>%
  left_join(red_mnm_crosswalk, by = c("city", "state"))



