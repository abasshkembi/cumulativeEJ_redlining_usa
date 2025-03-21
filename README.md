## cumulativeEJ_redlining_usa
Examining whether historically redlined neighborhoods are disproportionately burdened by cumulative environmental impacts today.

--------------------------------------------------------------------------------

*Code for* 

**Abas Shkembi**, Richard Neitzel (2025). Historical Redlining and Cumulative Environmental Impacts in the United States. *Environmental Science & Technology Letters*. https://doi.org/10.1021/acs.estlett.4c01111

**Contact:** Abas Shkembi (ashkembi@umich.edu)

## Description

The dataset and code used to replicate analyses for the following paper, published on March 18, 2025:

https://pubs.acs.org/doi/10.1021/acs.estlett.4c01111

## 1. EJScreen Data

The data used to run the analyses are stored in across three folders:

  * **Raw Data/**
    * {STATE_ABB}_20240218.Rds
      * The raw, state-specific estimations of environmental pollutants by HOLC-defined neighborhood called through the EJScreen API
      * The data are replicated in .csv format in the `csv/` folder.
  * **Processed Data/**
    * ejscreen_red_final.{Rds | csv}
      * The processed, nationwide estimations of environmental pollutants by HOLC-defined neighborhood called through the EJScreen API. Stored in both `.rds` and `.csv` format
    * cleanshp/
      * A shapefile folder storing the polygon boundaries of the HOLC-defined neighborhoods in `ejscreen_red_final.Rds`, processed from the main `HOLC Shapefiles/` folder
  * **Helper Data/**
    * area_definitions_m2019.xlsx
      * A crosswalk between county and metropolitan/nonmetropolitan areas for the year 2019
    * ejscreen_var_crosswalk.rds
      * Definitions of variable names stored in the EJScreen
    * msashape2/
      * A shapefile folder storing the polygon boundaries of metropolitan/nonmetropolitan areas in 2019


## 2. HOLC Shapefiles

Raw shapefiles downloaded from https://dsl.richmond.edu/panorama/redlining/ on October 1, 2023. **NOTE:** These shapefiles are no longer maintained on the *Mapping Inequality* website - the files can be downloaded as a GEOJSON however.

## 3. Code

The code used to call and process the EJScreen API are under the `01_API code` folder, and the code to perform the statistical analyses are under the `02_Analysis` folder. **NOTE:** The EJScreen API is no longer maintained by the EPA as of February 5, 2025. The API was called for the purposes of our analyses on February 18, 2024.

  * **01_API code/**
    * 01_final ejscreen api code.R
      * calls the EJScreen API to extract EJScreen data for every HOLC neighborhood
    * 02_processing api data.R
      * summarizes the data extracted by the API
  * **02_Analysis/**
    * 03a_helper_functions.R
      * functions to run the gradient boosted regression tree analysis + bootstrap
      * run these while running `06_gbm.R`
    * 03b_data_prep.R
      * cleaning up all the data and processing all the linkages with the different data
      run this while running `04_descriptives.R`, `05_car.R`, `06_gbm.R`, and `07_figure1.R`
    * 04_descriptives.R
      * makes figure 1a
    * 05_car.R
      * conducts the conditional autoregressive models
    * 06_gbm.R
      * running the gradient boosted regression tree for the cumulative effects
      * running the region- and metropolitan-area-specific models for cumulative effects
      * makes figure 1b and 1c
    * 07_graphical_abstract.R
      * simulates "hypothesized" data for the second panel of the Graphical Abstract
    * spatial_models_results.Rds
      * stores model results of conditional autoregressive models

## 4. Helper documents

  * EJScreen field description.pdf
    * Definitions of variables names in EJScreen API
  * MORE EJScreen field description.pdf
    * More definitions of variables names in EJScreen API
  * ejscreen_technical_document_downloaded02212024.pdf
    * Documentation of EJScreen provided by the EPA, downloaded February 12, 2024
