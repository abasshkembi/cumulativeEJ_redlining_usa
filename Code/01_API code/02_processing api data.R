library(tidyverse)
library(sf)


## this should not be changed unless there is an error
username <- "abasshkembi"
working_directory <- paste0("/Users/", username, "/University of Michigan Dropbox/Abas Shkembi/Redlining EJ USA/cumulativeEJ_redlining_usa/")
setwd(working_directory)

# list files in folder
ej_files <- list.files("EJScreen data/Raw data/")
ej_files <- ej_files[str_which(ej_files, ".Rds")]

# read in each state file
ej_sf <- NULL
for(i in 1:length(ej_files)) {
  temp_sf <- readRDS(paste0("EJScreen data/Raw data/", ej_files[i]))
  
  ej_sf <- ej_sf %>% rbind(temp_sf)
}

# convert to tibble
ej_df <- ej_sf %>% as_tibble()

# get variable names we are interested in
ej_vars <- c(
  "state", "city", "name", "holc_id", "holc_grade", "neighborho", "area_descr",
  "TOTALPOP", "NUM_MINORITY", "WHITE", "BLACK", "AMERIND", "ASIAN", "HAWPAC", "OTHER_RACE", "TWOMORE", "HISP", 
  "NHWHITE", "NHBLACK", "NHAMERIND", "NHASIAN", "NHHAWPAC", "NHOTHER_RACE", "NHTWOMORE",
  "MALES", "FEMALES", "AGE_LT5", "AGE_LT18", "AGE_GT17", "AGE_GT64", "NO_HS", "LINGISO", "LOWINC", "DISABILITY", "AREALAND", "AREAWATER",
  
  "RAW_D_PEOPCOLOR", "RAW_D_INCOME", "RAW_D_LESSHS", "RAW_D_LING", "RAW_D_UNDER5", "RAW_D_OVER64", "RAW_D_UNEMPLOYED", "RAW_D_DEMOGIDX2",
  "RAW_E_LEAD", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", "RAW_E_TRAFFIC", "RAW_E_NPDES", "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", "RAW_E_O3", "RAW_E_PM25", "RAW_E_UST", "RAW_E_RSEI_AIR")
# select the varaible names
ej_df_small <- ej_df[ej_vars]

# remove neighborhoods with no one living there today
ej_df2 <- ej_df_small %>% filter(TOTALPOP > 0)
nrow(ej_df2) - nrow(ej_df_small)
# -200 rows removed

ej_df_small %>% filter(TOTALPOP < 1 | is.na(TOTALPOP)) %>% .$neighborho %>% unique %>% length()
# 188 unique neighborhoods removed

# split data
### the information of the neighborhood
ej_df2a <- ej_df2[1:7]
### the raw data from EJ Screen
ej_df2b <- ej_df2[8:57]

ej_df3b <- data.frame(sapply(ej_df2b, function(x) as.numeric(gsub("%", "", x))))
# these NAs introduced were N/A in the original data

ej_df4 <- ej_df2a %>% cbind(ej_df3b)

# note that neighborho 8678 has two holc_ids (B6 and B7), but is the same neighborhood
# manually changing the holc_id to B6 for both
ej_df4 <- ej_df4 %>% mutate(holc_id = ifelse(neighborho == 8678 & holc_id == "B7", "B6", holc_id))

sum_cols <- c("TOTALPOP", "NUM_MINORITY", "WHITE", "BLACK", "AMERIND", "ASIAN", "HAWPAC", "OTHER_RACE", "TWOMORE", "HISP", 
              "NHWHITE", "NHBLACK", "NHAMERIND", "NHASIAN", "NHHAWPAC", "NHOTHER_RACE", "NHTWOMORE",
              "MALES", "FEMALES", "AGE_LT5", "AGE_LT18", "AGE_GT17", "AGE_GT64", "NO_HS", "LINGISO", "LOWINC", "DISABILITY", "AREALAND", "AREAWATER")

ej_df_sums <- ej_df4 %>% select(state:AREAWATER) %>%
  as_tibble() %>%
  group_by(state, city, name, holc_id, holc_grade, neighborho, area_descr) %>%
  summarise(across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE))) %>% ungroup()

length(unique(ej_df_sums$neighborho))
#[1] 8769
length(unique(all_redlined_shapefiles$neighborho))
#[1] 8877
## our data has 108 fewer neighborhoods than the full data

# which neighborhood ids are missing from the extracted data to the full dataset
unique(all_redlined_shapefiles$neighborho)[which(!(unique(all_redlined_shapefiles$neighborho) %in% unique(ej_df_sums$neighborho)))]
#[1] 8897 8898 8972
### all three of these neighborhood ids did not have a polygon associated with them in the original data

# check
sum(is.na(ej_df2$TOTALPOP))
sum(is.na(ej_df3b$TOTALPOP))
sum(is.na(ej_df_sums$TOTALPOP))




# get population weighted averages of all the environmental data
mean_cols <- c("RAW_E_LEAD", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", "RAW_E_TRAFFIC", "RAW_E_NPDES", "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", "RAW_E_O3", "RAW_E_PM25", "RAW_E_UST", "RAW_E_RSEI_AIR")

ej_df_means <- ej_df4 %>% select(state:TOTALPOP, RAW_E_LEAD:RAW_E_RSEI_AIR) %>%
  as_tibble() %>%
  mutate(across(all_of(mean_cols), ~ .x * TOTALPOP)) %>%
  group_by(state, city, name, holc_id, holc_grade, neighborho, area_descr) %>%
  summarise(across(all_of(c("TOTALPOP", mean_cols)), ~ sum(.x, na.rm = TRUE))) %>% ungroup() %>%
  mutate(across(all_of(mean_cols), ~ .x/TOTALPOP))

ej_df_grouped <- ej_df_sums %>% 
  left_join(ej_df_means, by = c("state", "city", "name", "holc_id", "holc_grade", "neighborho", "area_descr", "TOTALPOP"))

ej_df_final <- ej_df_grouped %>%
  mutate(across(all_of(sum_cols[-c(1, 28, 29)]), ~ round(.x/TOTALPOP*100, 1), .names = "RAW_D_{.col}"))

# there are four "E" neighborhoods across the cities... remove those
ej_df_final <- ej_df_final %>%
  filter(holc_grade %in% c("A", "B", "C", "D"))

summary(ej_df_final)

## saving the final, processed data into Rds and csv
saveRDS(ej_df_final, "EJScreen data/Processed data/ejscreen_red_final.Rds")
write.csv(ej_df_final, "EJScreen data/Processed data/ejscreen_red_final.csv")

