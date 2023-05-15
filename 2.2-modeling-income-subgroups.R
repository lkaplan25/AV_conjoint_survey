# Estimate mixed logit (mxl) models for Income Subgroups
# NOTE: The mxl models take a while (over an hour each) to run. We recommend running the code overnight.


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

choiceData <- read_csv(here::here('data_processed', 'choiceData.csv'))

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
numMultiStarts <- 30 
numCores <- 1

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

# Estimate separate models for each group in WTP space

mxl_wtp_A <- logitr(
  data       = data_A,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  modelSpace = "wtp",
  scalePar   = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  numCores   = numCores,
  numMultiStarts = numMultiStarts 
)


mxl_wtp_B <- logitr(
  data       = data_B,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  modelSpace = "wtp",
  scalePar   = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)

# View summary of results
summary(mxl_wtp_A) # Mid/High Income Household
summary(mxl_wtp_B) # Low Income Household

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_wtp_A$gradient
mxl_wtp_B$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_wtp_A$hessian)$values
eigen(mxl_wtp_B$hessian)$values

# Save model objects 

save(
  mxl_wtp_A, mxl_wtp_B,
  file = here("models", "mxl_income.RData") 
)
