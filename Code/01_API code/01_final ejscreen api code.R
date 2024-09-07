library(tidyverse)
library(sf)
library(nngeo)
library(EJSCREENbatch)

#devtools::install_github('USEPA/EJSCREENbatch')


# Step 1. Update your mac username --------------------------
username <- "abasshkembi"

# Step 2. Set working directory -----------------------------------------------------

## this should not be changed unless there is an error
working_directory <- paste0("/Users/", username, "/Dropbox (University of Michigan)/Redlining EJ USA/cumulativeEJ_redlining_usa/")
setwd(working_directory)

# Step 3. Read in redlining HOLC shapefiles

all_redlined_shapefiles <- sf::read_sf("HOLC Shapefiles/fullshpfile/shapefile/holc_ad_data.shp")


# Split multipolygons and fill holes --------------------------
## split multiple polygons into single polgyons
split_multipolygons <- sf::st_cast(all_redlined_shapefiles, "POLYGON")

## fill in holes in polygons
fill_holes <- nngeo::st_remove_holes(split_multipolygons)

# get unique states from dataframe
unique_states <- unique(fill_holes$state)


# for loop calling the EJSCREEN API for each state with redlined neighborhoods
for(i in 1:length(unique_states)) {
  # call ith state
  state_i <- unique_states[i]
  
  # filter data for state
  temp_shp_i <- fill_holes %>% filter(state == state_i)
  
  # call the API
  start_time <- Sys.time(); call_api_state_i <- EJSCREENbatch::EJSCREENBufferAPI(temp_shp_i, 0); end_time <- Sys.time()
  time_taken = end_time-start_time; time_taken
  
  ## get system date to include in final filename
  system_date <- Sys.Date() %>% str_remove_all("-")
  ## create standardized filename
  filename_final_rds <- paste0("EJScreen data/Raw data/", state_i, "_", system_date, ".Rds")
  saveRDS(call_api_state_i, file = filename_final_rds)
  ## finally create the csv file
  filename_final_csv <- paste0("EJScreen data/Raw data/csv/", state_i, "_", system_date, ".csv")
  write_csv(call_api_state_i, filename_final_csv)
}










