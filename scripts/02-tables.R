#### Preamble ####
# Purpose: Creating and testing the model
# Author: Isaac Ng
# Date: April 26, 2022
# Contact: isaac.ng@mail.utoronto.ca

#### Tables used in the data section ####
# necessary libraries
library(forcats)
library(dplyr)
library(haven)
library(tidyverse)
library(here)

# read in the data from models and the cleaning of data
data <- read_csv(here::here("inputs/data/final_dataset.csv"))
raw_data <- read_csv(here::here("inputs/data/raw_totals.csv"))

# add a new column which combines the dates into one that is easy to read
data["s_y"] <- paste(data$Season, data$Y_lead, sep = " ")
raw_data["m_y"] <- paste(raw_data$M, raw_data$Y, sep = " ")
raw_data <- raw_data |>
  arrange(M) |>
  arrange(Y)

# first graph comparing the percentage and percent change in population with a first vaccination dose
graph_first_vacs_over_time <- 
  data |>
  ggplot() + 
  geom_line(data = data, aes(x = fct_inorder(s_y), y = Administered_Dose1_Pop_Pct), color = "blue") +
  geom_line(data = data, aes(x = fct_inorder(s_y), y = change_in_perc_first), color = "red") +
  xlab('Dates') +
  ylab('Percents')

# second graph comparing number of new cases and number of new deaths
graph_cases_deaths_over_time <- 
  raw_data |>
  ggplot() + 
  geom_line(data = raw_data, aes(x = fct_inorder(m_y), y = new_case), color = "blue") +
  geom_line(data = raw_data, aes(x = fct_inorder(m_y), y = new_death), color = "red") +
  xlab('Dates') +
  ylab('Population') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# aggregating by state
total_cases_by_state <- aggregate(cbind(tot_cases, Administered_Cumulative) ~ State, max, data = raw_data)

# intermediate steps to help the bar graph next
int_step <- data.frame(total_cases_by_state$tot_cases, total_cases_by_state$State)
int_step["type"] <- "Cases count"
colnames(int_step) = c("value","State","type")
int_step2 <- data.frame(total_cases_by_state$Administered_Cumulative, total_cases_by_state$State)
int_step2["type"] <- "Vaccination count"
colnames(int_step2) = c("value","State","type")

# the combined data (written so that it can be used in the RMD file)
combined <- int_step |>
  full_join(int_step2)
write_csv(combined, "inputs/data/combined.csv")

# third graph with the total number of cases and total number of vaccinated people by state
graph_total_cases_by_state <- combined |>
  ggplot(aes(x = State, y = value, fill= type)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('Total cases') +
  xlab('State')

# fourth graph comparing cases to vaccinations
graph_cases_to_vacs <- data |>
  ggplot() + 
  geom_line(data = raw_data, aes(x = tot_cases, y = Administered_Cumulative, colour = State)) +
  xlab('Total cases in a state') +
  ylab('Total vaccines administered in a state')
