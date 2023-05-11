# Estimate multinomial logit (MNL) models for two groups in the data

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

pars_wtp <- c(
  "travelTime",
  "mode_bus", "bus_automated_yes", "bus_attendant_yes",
  "mode_RH", "RH_automated_yes", "RH_attendant_yes",
  "mode_sharedRH", "sharedRH_automated_yes", "sharedRH_attendant_yes"
)

# Read in choice data--------------

choiceData <- read_csv(here::here('data_processed', 'choiceData_Income.csv')) # TESTING INCOME


# -----------------------------------------------------------------------------
# Estimate models where all covariates are dummy coded

# Create dummy coded variables
data <- dummy_cols(choiceData, c('mode', 'automated', 'attendant')) %>%
  select(-mode_rail, -imgPath) %>% 
  mutate(
    bus_automated_yes      = mode_bus*automated_Yes,
    bus_automated_no       = mode_bus*automated_No,
    bus_attendant_yes      = mode_bus*attendant_Yes,
    bus_attendant_no       = mode_bus*attendant_No,
    RH_automated_yes       = mode_RH*automated_Yes,
    RH_automated_no        = mode_RH*automated_No,
    RH_attendant_yes       = mode_RH*attendant_Yes,
    RH_attendant_no        = mode_RH*attendant_No,
    sharedRH_automated_yes = mode_sharedRH*automated_Yes,
    sharedRH_automated_no  = mode_sharedRH*automated_No,
    sharedRH_attendant_yes = mode_sharedRH*attendant_Yes,
    sharedRH_attendant_no  = mode_sharedRH*attendant_No,
  ) %>% 
  select(id, obsID, choice, weights, everything()) %>% 
  arrange(id)

data$obsID = rep(seq(nrow(data) / 4), each = 4)

## Models by income----------------------------------------------------------------


# Split data into groups. Re-create obsID and respondent IDs. 
data_A <- data %>% filter(incomeGroup == "A") # mid/high income
data_A$obsID = rep(seq(nrow(data_A) / 4), each = 4)

id <-  sort(unique(data_A$id))

temp <- data.frame(id = id, newID = seq(1:length(id)))

data_A <- data_A %>% 
  left_join(temp, by = "id") %>% 
  select(-id) %>% 
  rename(id = newID)


data_B <- data %>% filter(incomeGroup == "B") # low-income households
data_B$obsID = rep(seq(nrow(data_B) / 4), each = 4)

id <-  sort(unique(data_B$id))

temp <- data.frame(id = id, newID = seq(1:length(id)))

data_B <- data_B %>% 
  left_join(temp, by = "id") %>% 
  select(-id) %>% 
  rename(id = newID)

# Estimate separate models for each group
mnl_group_A <- logitr(
  data    = data_A,
  outcome = "choice",
  obsID   = "obsID",
  pars = pars_wtp,
  scalePar = 'price'
)

mnl_group_B <- logitr(
  data    = data_B,
  outcome = "choice",
  obsID   = "obsID",
  pars = pars_wtp,
  scalePar = 'price'
)

# View summary of results
summary(mnl_group_A)
summary(mnl_group_B)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_group_A$gradient
mnl_group_B$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_group_A$hessian)$values
eigen(mnl_group_B$hessian)$values

# Save model objects
save(
  mnl_group_A,
  mnl_group_B,
  file = here("models", "mnl_incomeGroup_wtp.RData")
)

