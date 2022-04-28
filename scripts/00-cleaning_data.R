#### Preamble ####
# Purpose: Clean the survey data downloaded from CDC website (see references)
# Author: Isaac Ng
# Data: April 26, 2022
# Contact: isaac.ng@mail.utoronto.ca

### Workspace setup ###

# needed libraries
library(haven)
library(tidyverse)
library(dplyr)
library(here)

# Read in the raw data for cases
raw_case_data <- read_csv(here::here("inputs/data/Case_Death_data.csv"))

## Get the right attributes, omit any NAs, correct typings and names
case_data <-
  raw_case_data |>
  select(submission_date, state, new_case, new_death, tot_cases)
case_data <- na.omit(case_data)
case_data$new_case <- as.integer(case_data$new_case)
case_data$new_death <- as.integer(case_data$new_death)
colnames(case_data) <- c("Date", "State", "new_case", "new_death", "tot_cases")

# Read in the raw data for vaccines
raw_vac_data <- read_csv(here::here("inputs/data/vaccine_data.csv"))

## Get the right attributes and correct the names
vac_data <-
  raw_vac_data |>
  subset(date_type == "Admin") |>
  select(Date, Location, Administered_Dose1_Pop_Pct, Administered_Cumulative)
colnames(vac_data) <- c("Date", "State", "Administered_Dose1_Pop_Pct", "Administered_Cumulative")

# merge the two sets together
total <- merge(vac_data, case_data, by=c("Date", "State"))

#### Clean the data ####

# Change the date (American dates so cant use as.Date)
date_changes <- total |>
  separate(col = Date, into = c("M", "D", "Y")) |>
  select(-D)

# write raw totals
write_csv(date_changes, "inputs/data/raw_totals.csv")

# Sum cases and deaths over the month
monthly_sums <- aggregate(cbind(new_case, new_death) ~ M + Y+ State, data = date_changes, sum)

# Get the max population percentage with doses (end of the month)
monthly_percents <- aggregate(Administered_Dose1_Pop_Pct ~ M + Y + State, data = date_changes, max)

# Combine the datasets
monthly_combine <- merge(monthly_sums, monthly_percents)

# Order it correctly for the next step
monthly_combine <- monthly_combine |>
  arrange(M) |>
  arrange(Y) |>
  arrange(State) |>
  mutate(Y_lead = lead(Y))
monthly_combine$Y_lead[969] = 2022
monthly_combine$Y_lead[monthly_combine$Y_lead == 2020] <- 2022   

# Make a seasons column
monthly_combine$M <- as.integer(monthly_combine$M)
monthly_combine["Season"] <- as.integer((monthly_combine$M) / 3)
monthly_combine$Season <- replace(monthly_combine$Season, monthly_combine$Season == 4, 0)

# Sum cases and deaths over the season
season_sums <- aggregate(cbind(new_case, new_death) ~ Season + Y_lead+ State, data = monthly_combine, sum)

# Get the max population percentage with doses (end of the season)
season_percents <- aggregate(Administered_Dose1_Pop_Pct ~ Season + Y_lead + State, data = monthly_combine, max)

# Combine the two datasets
final_dataset <- merge(season_sums, season_percents)

# Order it correctly for the next step
final_dataset <- final_dataset |>
  arrange(Administered_Dose1_Pop_Pct) |>
  arrange(State)

# Get the change in percentages each season
final_dataset["change_in_perc_first"] <- c(0, diff(final_dataset$Administered_Dose1_Pop_Pct))
final_dataset$change_in_perc_first[final_dataset$change_in_perc_first < 0] <- final_dataset$Administered_Dose1_Pop_Pct[final_dataset$change_in_perc_first < 0]
final_dataset$change_in_perc_first[1] <- final_dataset$Administered_Dose1_Pop_Pct[1]

final_dataset <- tibble(final_dataset)

# Calculate the death to case ratio
final_dataset["dtc"] <- final_dataset$new_death / final_dataset$new_case

# Making data work for model
final_dataset <- na.omit(final_dataset)
final_dataset <- final_dataset |>
  subset(change_in_perc_first > 0)

# Write the csv file
write_csv(final_dataset, "inputs/data/final_dataset.csv")
