# Estimate multinomial logit (mnl) and mixed logit models

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

choiceData <- read_csv(here::here('data_processed', 'choiceData.csv'))

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

# Setup some common objects

numDraws <- 300
numMultiStarts <- 30 
numCores <- 1

pars_pref <- c(
  "price", "travelTime",
  "mode_bus", "bus_automated_yes", "bus_attendant_yes",
  "mode_RH", "RH_automated_yes", "RH_attendant_yes",
  "mode_sharedRH", "sharedRH_automated_yes", "sharedRH_attendant_yes"
)

pars_wtp <- c(
  "travelTime",
  "mode_bus", "bus_automated_yes", "bus_attendant_yes",
  "mode_RH", "RH_automated_yes", "RH_attendant_yes",
  "mode_sharedRH", "sharedRH_automated_yes", "sharedRH_attendant_yes"
)

randPars = c(
  travelTime = 'n',
  mode_bus = 'n', bus_automated_yes = 'n', bus_attendant_yes = 'n',
  mode_RH = 'n', RH_automated_yes = 'n', RH_attendant_yes = 'n',
  mode_sharedRH = 'n', sharedRH_automated_yes = 'n', sharedRH_attendant_yes = 'n'
)

# Simple logit model -------
mnl_pref <- logitr(
  data      = data,
  outcome   = "choice",
  obsID     = "obsID",
  clusterID = "id",
  pars      = pars_pref
)

# Estimate a simple logit model in WTP space
mnl_wtp <- logitr(
  data       = data,
  outcome    = "choice",
  obsID      = "obsID",
  clusterID  = "id",
  modelSpace = "wtp",
  price      = "price",
  pars       = pars_wtp,
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)

mnl_wtp_weighted <- logitr(
  data       = data,
  outcome    = "choice",
  obsID      = "obsID",
  clusterID  = "id",
  modelSpace = "wtp",
  price      = "price",
  pars       = pars_wtp,
  weights    = "weights",
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)

# View summary of results
summary(mnl_pref)
summary(mnl_wtp)
summary(mnl_wtp_weighted)

# Weights have little effect on WTP
wtpCompare(mnl_pref, mnl_wtp, "price")
wtpCompare(mnl_pref, mnl_wtp_weighted, "price")

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_pref$gradient
mnl_wtp$gradient
mnl_wtp_weighted$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_pref$hessian)$values
eigen(mnl_wtp$hessian)$values
eigen(mnl_wtp_weighted$hessian)$values


# Save models them remove to free up memory
save(
  mnl_pref, mnl_wtp, mnl_wtp_weighted,
  file = here::here("models_lk", "mnl.RData") #changed to avoid overwriting
)
rm(mnl_pref, mnl_wtp, mnl_wtp_weighted)
gc() # garbage collect to free up memory



# Mixed logit models ----------

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

mxl_wtp <- logitr(
  data       = data,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  modelSpace = "wtp",
  price      = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)

mxl_wtp_weighted <- logitr(
  data       = data,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  modelSpace = "wtp",
  price      = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  weights    = "weights",
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)

# Save
save(
  mxl_pref, mxl_wtp, mxl_wtp_weighted, 
  file = here::here("models_lk", "mxl.RData") #changed to avoid overwriting
)
rm(mxl_pref, mxl_wtp, mxl_wtp_weighted)
gc()

# Subgroup analysis --------------------------

## Models by gender ----------------------

# Split data into groups
data_A <- data %>% filter(genderGroup == "A") # male
data_B <- data %>% filter(genderGroup == "B") # female, transgender, non-binary

# Estimate separate models for each group in WTP space

# mnl_wtp_A <- logitr(
#   data       = data_A,
#   outcome    = "choice",
#   obsID      = "obsID",
#   clusterID  = "id",
#   modelSpace = "wtp",
#   price      = "price",
#   pars       = pars_wtp,
#   numCores   = numCores,
#   numMultiStarts = numMultiStarts
# )


mxl_wtp_A <- logitr(
  data       = data_A,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  modelSpace = "wtp",
  price      = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)


# mnl_wtp_B <- logitr(
#   data       = data_B,
#   outcome    = "choice",
#   obsID      = "obsID",
#   clusterID  = "id",
#   modelSpace = "wtp",
#   price      = "price",
#   pars       = pars_wtp,
#   numCores   = numCores,
#   numMultiStarts = numMultiStarts
# )


mxl_wtp_B <- logitr(
  data       = data_B,
  outcome    = "choice",
  obsID      = "obsID",
  panelID    = "id",
  clusterID  = "id",
  numDraws   = numDraws,
  modelSpace = "wtp",
  price      = "price",
  pars       = pars_wtp,
  randPars   = randPars,
  numCores   = numCores,
  numMultiStarts = numMultiStarts
)
# View summary of results
summary(mxl_wtp_A) # Male
summary(mxl_wtp_B) # Female/Trans

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
  file = here("models_lk", "mxl_gender.RData") #changed to avoid overwriting
)
