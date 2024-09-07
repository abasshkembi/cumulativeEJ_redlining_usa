*Code for*
# Historical Redlining and Cumulative Environmental Impacts in the United States

Abas Shkembi*, Richard Neitzel

*e: ashkembi@umich.edu

## Description

text...

## 1. Data

## 2. HOLC Shapefiles

## 3. Code

01_final ejscreen api code.R
- calls the EJScreen API to extract EJScreen data for every HOLC neighborhood

02_processing api data.R
- summarizes the data extracted by the API

03a_helper_functions.R
- functions to run the gradient boosted regression tree analysis + bootstrap
- run these while running 06_gbm.R

03b_data_prep.R
- cleaning up all the data and processing all the linkages with the different data
- run this while running 04_descriptives.R, 05_car.R, 06_gbm.R, and 07_figure1.R

04_descriptives.R
- making table 1 and figure 2

05_car.R
- conducts the conditional autoregressive models

06_gbm.R
- running the gradient boosted regression tree for the cumulative effects
- running the metropolitan area specific models for cumulative effects
- makes figure 3 and figure 4

07_figure1.R
- simulates fake data for the second pabel of figure 1 

## 4. Helper documents

