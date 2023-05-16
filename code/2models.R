# Estimate mixed logit (mxl) models
#
# NOTE: Due to using a multistart, the mxl models can take a while 
# (over an hour each) to run. We recommend running the code overnight.

# Load libraries and settings
source(here::here('code', '0setup.R'))

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

data <- read_csv(here::here('data_processed', 'choiceData.csv'))

# -----------------------------------------------------------------------------
# Dummy-code covariates

# Create dummy coded variables
data <- dummy_cols(data, c('mode', 'automated', 'attendant')) %>%
  select(-mode_rail, -imgPath) %>% 
  mutate(
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
    sharedRH_attendant_no  = mode_sharedRH*attendant_No,
  ) %>% 
  select(id, obsID, choice, weights, everything()) %>% 
  arrange(id)

data$obsID <- rep(seq(nrow(data) / 4), each = 4)

# Setup some common objects

numDraws <- 500
numMultiStarts <- 30
numCores <- 2
drawType <- 'sobol'

pars <- c(
  "travelTime",
  "travelTime_bus", "travelTime_RH", "travelTime_sharedRH",
  "mode_bus", "bus_automated_yes", "bus_attendant_yes",
  "mode_RH", "RH_automated_yes", "RH_attendant_yes",
  "mode_sharedRH", "sharedRH_automated_yes", "sharedRH_attendant_yes"
)

randPars = c(
  mode_bus = 'n', # bus_automated_yes = 'n', bus_attendant_yes = 'n',
  mode_RH = 'n', #RH_automated_yes = 'n', RH_attendant_yes = 'n',
  mode_sharedRH = 'n' #sharedRH_automated_yes = 'n', sharedRH_attendant_yes = 'n'
)

# Function to define a single model

estimate_model <- function(
  data, pars, randPars, numDraws, drawType, numCores, numMultiStarts,
  weights = NULL
) {
  model <- logitr(
    data       = data,
    outcome    = "choice",
    obsID      = "obsID",
    panelID    = "id",
    clusterID  = "id",
    pars       = pars,
    scalePar   = "price",
    randPars   = randPars,
    numDraws   = numDraws,
    drawType   = drawType,
    numCores   = numCores,
    numMultiStarts = numMultiStarts,
    weights = weights
  )
}

# Models on entire sample ----------

mxl_wtp <- estimate_model(
  data, pars, randPars, numDraws, drawType, numCores, numMultiStarts = 1
)

mxl_wtp_weighted <- estimate_model(
  data, pars, randPars, numDraws, drawType, numCores, numMultiStarts,
  weights = "weights"
)

# Save

saveRDS(mxl_wtp, here::here('models', 'mxl_wtp.Rds'))
saveRDS(mxl_wtp_weighted, here::here('models', 'mxl_wtp_weighted.Rds'))

# Models by gender ----------------------

# Split data into groups. Re-create obsID and respondent IDs. 

data_A <- data %>% filter(genderGroup == "A") # male
data_A$obsID = rep(seq(nrow(data_A) / 4), each = 4)
id <-  sort(unique(data_A$id))
temp <- data.frame(id = id, newID = seq(1:length(id)))
data_A <- data_A %>% 
  left_join(temp, by = "id") %>% 
  select(-id) %>% 
  rename(id = newID)

data_B <- data %>% filter(genderGroup == "B") # female, transgender, non-binary
data_B$obsID = rep(seq(nrow(data_B) / 4), each = 4)
id <-  sort(unique(data_B$id))
temp <- data.frame(id = id, newID = seq(1:length(id)))
data_B <- data_B %>% 
  left_join(temp, by = "id") %>% 
  select(-id) %>% 
  rename(id = newID)

mxl_wtp_gender_male <- estimate_model(
  data_A, pars, randPars, numDraws, drawType, numCores, numMultiStarts
)

mxl_wtp_gender_female <- estimate_model(
  data_B, pars, randPars, numDraws, drawType, numCores, numMultiStarts
)

saveRDS(
  mxl_wtp_gender_male, here::here('models', 'mxl_wtp_gender_male.Rds'))
saveRDS(
  mxl_wtp_gender_female, here::here('models', 'mxl_wtp_gender_female.Rds'))

# Models by income ----------------------

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

mxl_wtp_income_high <- estimate_model(
  data_A, pars, randPars, numDraws, drawType, numCores, numMultiStarts
)

mxl_wtp_income_low <- estimate_model(
  data_B, pars, randPars, numDraws, drawType, numCores, numMultiStarts
)

saveRDS(
  mxl_wtp_income_high, here::here('models', 'mxl_wtp_income_high.Rds'))
saveRDS(
  mxl_wtp_income_low, here::here('models', 'mxl_wtp_income_low.Rds'))
