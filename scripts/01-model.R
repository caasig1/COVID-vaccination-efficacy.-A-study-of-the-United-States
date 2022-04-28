#### Preamble ####
# Purpose: Creating and testing the model
# Author: Isaac Ng
# Date: April 26, 2022
# Contact: isaac.ng@mail.utoronto.ca

#### Modelling ####
# Libraries needed
library(modelsummary)
library(lmtest)
library(broom)
library(ggplot2)
library(haven)
library(tidyverse)
library(dplyr)
library(here)

# Function to find the number of outliers in the dataset
num_outliers <- function(fit) {
  total_observations <- nrow(fit$model)
  total_attributes <- length(fit$coefficients)
  
  augment(fit) |>
    filter(!(.std.resid < 2 & 
               .cooksd < 4 / (total_observations - total_attributes - 1) & 
               .hat < 2*total_attributes / total_observations)) |>
    nrow()
}

# Read in the cleaned data
data <- read_csv(here::here("inputs/data/final_dataset.csv"))

# Model used in this paper
final_model <- lm(dtc ~ 0 + log(change_in_perc_first) + as.factor(State), 
                  data = data)

# Used to see if any correlations exist in the data
pairs(dtc ~ change_in_perc_first + new_case + new_death + 
        Administered_Dose1_Pop_Pct, data = data)

# Summary of the final model (AIC, BIC, R2, F)
final_model |>
  modelsummary::modelsummary(fmt=2)

# Summary of the final model (P values)
summary(final_model)

# Plots with identifying features
par(mfrow = c(2,2))
plot(final_model)

# Small p value for power 2 so bigger powers wont affect it
resettest(final_model, power=2, type="regressor")

# Low number of outliers (20 out of 339)
num_outliers(final_model)
