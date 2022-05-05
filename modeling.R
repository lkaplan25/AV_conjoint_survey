
# Estimate multinomial logit (mnl) and mixed logit models

# Load libraries
library(logitr)
library(fastDummies)
library(here)
library(tidyverse)
library(maddTools)
# -----------------------------------------------------------------------------

# Variables:
# "respondentID"      = Identifies each survey respondent
# "qID"         = Identifies each question for each survey respondent
# "altID"       = Identifies the alternative in each unique choice observation
# "obsID"       = Identifies each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "mode"        = Transportation mode 
# "automated"   = Whether or not the mode is automated (yes, no)
# "waitTime"    = Wait time in minutes 
# "attendant"   = Whether an attendant is present in the vehicle (only for automated modes, yes/no)
# "price"       = Purchase price in dollars 
# "travelTime"  = Travel time in minutes


# Read in choice data--------------

choiceData <- read_csv(here::here('data_processed', 'choiceData.csv'))



# -----------------------------------------------------------------------------
# Estimate MNL model where all covariates are dummy-coded

# Create dummy coded variables
data_dummy <- dummy_cols(choiceData, c('mode', 'automated', 'attendant')) %>% 
  select(-mode_rail) %>% 
  mutate(
    bus_automated_yes = mode_bus*automated_Yes,
    bus_automated_no = mode_bus*automated_No,
    bus_attendant_yes = mode_bus*attendant_Yes,
    bus_attendant_no = mode_bus*attendant_No,
    RH_automated_yes = mode_RH*automated_Yes,
    RH_automated_no = mode_RH*automated_No,
    RH_attendant_yes = mode_RH*attendant_Yes,
    RH_attendant_no = mode_RH*attendant_No,
    sharedRH_automated_yes = mode_sharedRH*automated_Yes,
    sharedRH_automated_no = mode_sharedRH*automated_No,
    sharedRH_attendant_yes = mode_sharedRH*attendant_Yes,
    sharedRH_attendant_no = mode_sharedRH*attendant_No,
  ) %>% 
  select(id, obsID, choice, weight, everything()) %>% 
  arrange(id)

data_dummy$obsID = rep(seq(nrow(data_dummy) / 4), each = 4)



# Simple logit model -------
mnl_dummy <- logitr(
  data   = data_dummy,
  outcome = "choice",
  obsID  = "obsID",
  # Remember one level must be "dummied out"
  pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "price", "travelTime"),
  #clusterID = "id"
)


# Estimate a simple logit model in WTP space
mnl_dummy_WTP <- logitr(
  data   = data_dummy,
  outcome = "choice",
  obsID  = "obsID",
  # Remember one level must be "dummied out"
  pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "travelTime"),
  price = "price",
  modelSpace = "wtp",
  numMultiStarts = 10, # Use a multi-start since log-likelihood is nonconvex
  #clusterID = "id"
)


mnl_dummy_WTP_weighted <- logitr(
  data   = data_dummy,
  outcome = "choice",
  obsID  = "obsID",
  # Remember one level must be "dummied out"
  pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "travelTime"),
  price = "price",
  modelSpace = "wtp",
  numMultiStarts = 10, # Use a multi-start since log-likelihood is nonconvex
  weights = "weight",
  # robust = TRUE,
  # clusterID = "id"
)

# View summary of results
summary(mnl_dummy)
summary(mnl_dummy_WTP)
summary(mnl_dummy_WTP_weighted)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_dummy$gradient
mnl_dummy_WTP$gradient
mnl_dummy_WTP_weighted$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_dummy$hessian)$values
eigen(mnl_dummy_WTP$hessian)$values
eigen(mnl_dummy_WTP_weighted$hessian)$values

# Mixed logit models ----------

mxl_dummy <- logitr(
  data   = data_dummy,
  outcome = "choice",
  obsID  = "obsID",
  panelID = "id",
  # Remember one level must be "dummied out"
  pars   = c("price", "mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "travelTime"),
  randPars = c(price = 'n', mode_bus = 'n', mode_RH = 'n', mode_sharedRH = 'n', bus_automated_yes = 'n', bus_attendant_yes = 'n', RH_automated_yes = 'n', RH_attendant_yes = 'n', sharedRH_automated_yes = 'n', sharedRH_attendant_yes = 'n', travelTime = 'n'),
  #clusterID = "id"
)


mxl_wtp <- logitr(
  data   = data_dummy,
  outcome = "choice",
  obsID  = "obsID",
  panelID = "id",
  # Remember one level must be "dummied out"
  pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "travelTime"),
  price = "price",
  modelSpace = "wtp",
  randPars = c(mode_bus = 'n', mode_RH = 'n', mode_sharedRH = 'n', bus_automated_yes = 'n', bus_attendant_yes = 'n', RH_automated_yes = 'n', RH_attendant_yes = 'n', sharedRH_automated_yes = 'n', sharedRH_attendant_yes = 'n', travelTime = 'n'),
  numMultiStarts = 10, # Use a multi-start since log-likelihood is nonconvex
  #clusterID = "id"
)

mxl_wtp_weighted <- logitr(
  data   = data_dummy,
  outcome = "choice",
  obsID  = "obsID",
  panelID = "id", 
  # Remember one level must be "dummied out"
  pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "travelTime"),
  price = "price",
  modelSpace = "wtp",
  randPars = c(mode_bus = 'n', mode_RH = 'n', mode_sharedRH = 'n', bus_automated_yes = 'n', bus_attendant_yes = 'n', RH_automated_yes = 'n', RH_attendant_yes = 'n', sharedRH_automated_yes = 'n', sharedRH_attendant_yes = 'n', travelTime = 'n'),
  numMultiStarts = 1, # Use a multi-start since log-likelihood is nonconvex
  weights = "weight",
  robust = TRUE, 
  clusterID = "id"
)


# View summary of results
summary(mxl_wtp)
summary(mxl_wtp_weighted)

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_wtp$gradient
mxl_wtp_weighted$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_wtp$hessian)$values
eigen(mxl_wtp_weighted)$values

## Save model objects  -----------------------------------------------------------------------------

save(
  mnl_dummy,
  mnl_dummy_WTP,
  mnl_dummy_WTP_weighted,
  mxl_dummy,
  mxl_wtp,
  mxl_wtp_weighted,
  file = here::here("models", "mnl.RData")
)


# Subgroup analysis --------------------------


## Models by gender----------------------

# Split data into groups
data_A <- data_dummy %>% filter(genderGroup == "A") #male
data_B <- data_dummy %>% filter(genderGroup == "B") #female, transgender, non-binary

# Estimate separate models for each group in WTP space

mnl_wtp_A <- logitr(
  data   = data_A,
  outcome = "choice",
  obsID  = "obsID",
  # Remember one level must be "dummied out"
  pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "travelTime"),
  price = "price",
  modelSpace = "wtp",
  numMultiStarts = 10 
)

mnl_wtp_B <- logitr(
  data   = data_B,
  outcome = "choice",
  obsID  = "obsID",
  # Remember one level must be "dummied out"
  pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "travelTime"),
  price = "price",
  modelSpace = "wtp",
  numMultiStarts = 10 
)


# View summary of results
summary(mnl_wtp_A) # Male
summary(mnl_wtp_B) # Female/Trans

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp_A$gradient
mnl_wtp_B$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp_A$hessian)$values
eigen(mnl_wtp_B$hessian)$values

# Save model objects 

save(
  mnl_wtp_A,
  mnl_wtp_B,
  file = here("models", "mnl_wtp_gender.RData")
)

