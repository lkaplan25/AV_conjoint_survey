# Estimate mixed logit (mxl) models
# NOTE: The mxl models take a while (over an hour each) to run. We recommend running the code overnight.
# This file includes models in which travel time is estimated for each mode type. 

# Load libraries and settings
source(here::here('code', '0setup.R'))
options(dplyr.width = Inf)

# -----------------------------------------------------------------------------

# Variables:
# "respondentID" = Identifies each survey respondent
# "qID"          = Identifies each question for each survey respondent
# "altID"        = Identifies the alternative in each unique choice observation
# "obsID"        = Identifies each unique choice observation
# "choice"       = 1 if the alternative is chosen, 0 otherwise
# "mode"         = Transportation mode
# "automated"    = Whether or not the mode is automated (yes, no)
# "waitTime"     = Wait time in minutes
# "attendant"    = Whether an attendant is present in the vehicle
#                  (only for automated modes, yes/no)
# "price"        = Purchase price in dollars
# "travelTime"   = Travel time in minutes


# Read in choice data--------------

choiceData <- read_csv(here::here('data_processed', 'choiceData_gender.csv')) 


# Estimate models where all covariates are dummy coded

# Create dummy coded variables
data <- dummy_cols(choiceData, c('mode', 'automated', 'attendant')) %>%
  select(-mode_rail, -imgPath) %>% 
  mutate(
    # Adding in mode-specific travelTimes
    travelTime_bus         = mode_bus*travelTime,
    travelTime_RH          = mode_RH*travelTime,
    travelTime_sharedRH    = mode_sharedRH*travelTime,
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
    sharedRH_attendant_no  = mode_sharedRH*attendant_No
  ) %>% 
  select(id, obsID, choice, weights, everything()) %>% 
  arrange(id)

data$obsID = rep(seq(nrow(data) / 4), each = 4)

# Setup some common objects

numDraws <- 300
numMultiStarts <- 1 
numCores <- 7

pars_pref <- c(
  "price", "travelTime",
  "travelTime_bus", "travelTime_RH", "travelTime_sharedRH",
  "mode_bus", "bus_automated_yes", "bus_attendant_yes",
  "mode_RH", "RH_automated_yes", "RH_attendant_yes",
  "mode_sharedRH", "sharedRH_automated_yes", "sharedRH_attendant_yes"
)

pars_wtp <- c(
  "travelTime",
  "travelTime_bus", "travelTime_RH", "travelTime_sharedRH",
  "mode_bus", "bus_automated_yes", "bus_attendant_yes",
  "mode_RH", "RH_automated_yes", "RH_attendant_yes",
  "mode_sharedRH", "sharedRH_automated_yes", "sharedRH_attendant_yes"
)

randPars = c(
  travelTime = 'n',
  travelTime_bus = 'n', travelTime_RH = 'n', travelTime_sharedRH = 'n',
  mode_bus = 'n', bus_automated_yes = 'n', bus_attendant_yes = 'n',
  mode_RH = 'n', RH_automated_yes = 'n', RH_attendant_yes = 'n',
  mode_sharedRH = 'n', sharedRH_automated_yes = 'n', sharedRH_attendant_yes = 'n'
)

# ----------------------------------------------------------------------
#  Preference Space Model

mxl_pref <- logitr(
  data      = data,
  outcome   = "choice",
  obsID     = "obsID",
  panelID   = "id",
  clusterID = "id",
  numDraws  = numDraws,
  pars      = pars_pref,
  randPars  = randPars,
  numCores  = numCores,
  numMultiStarts = numMultiStarts
)

# WTP Space Model
mxl_wtp <- logitr(
  data       = data,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  scalePar   = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)

# WTP Space Model with Weights for gender
mxl_wtp_weighted <- logitr(
  data       = data,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  scalePar   = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  weights    = "weights",
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)

# Save
save(
  mxl_pref, mxl_wtp, 
  mxl_wtp_weighted, 
  file = here::here("models", "mxl_v2.RData") 
)
rm(mxl_pref, mxl_wtp, mxl_wtp_weighted)
gc()

