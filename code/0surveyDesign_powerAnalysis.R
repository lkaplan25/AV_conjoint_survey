# Load libraries and settings
source(here::here('code', '0setup.R'))

# This file contains the code to create a full factorial design of experiment
# based on the selected attribute levels.
# The file also includes code to perform a power analysis, which guided our
# target sample size for the survey.

# Create the survey----------------------------------------------------------------
# Define the attributes and levels
profiles <- cbc_profiles(
    mode                = c("sharedRH", "RH", "bus", "rail"),
    # Mode-specific attributes
    travelTime_sharedRH = c(0.8, 0.9, 1, 1.1, 1.2), # Time (min), for sharedRH will later multiply by RH travelTime
    travelTime_RH       = c(15, 20, 25, 30, 35),
    travelTime_bus      = c(20, 25, 30, 35, 40),
    travelTime_rail     = c(15, 20, 25, 30, 35),
    price_sharedRH      = c(0.5, 0.7, 0.8), # Price ($), will later multiply by RH price
    price_RH            = c(5, 7, 10, 12, 15),
    price_bus           = c(1, 2, 3, 4, 5),
    price_rail          = c(2, 3, 4, 5, 6),
    # All other attributes
    automated           = c(0, 1),
    attendant           = c(0, 1)) 

# Make a survey based on a full-factorial design of experiment
survey <- cbc_design(
  profiles = profiles,
  n_resp = 5000, # Number of respondents
  n_alts = 4, # Number of alternatives per question
  n_q    = 8, # Number of questions per respondent
  label = "mode" # Attributes are mode-specific
  ) %>% 
  # Recode price and travelTime variables
  mutate(
    price = case_when(
       mode == "sharedRH" ~ price_sharedRH,
       mode == "RH" ~ price_RH, 
       mode == "bus" ~ price_bus, 
       mode == "rail" ~ price_rail),
    travelTime = case_when(
       mode == "sharedRH" ~ travelTime_sharedRH, 
       mode == "RH" ~ travelTime_RH, 
       mode == "bus" ~ travelTime_bus, 
       mode == "rail" ~ travelTime_rail)) %>% 
    select(-starts_with("price_"), -starts_with("travelTime_")) %>% 
    mutate(
      # Rail should never be automated 
      automated = ifelse(mode == "rail", 0, automated),
      # If not automated, attendant should be 0 
      attendant = ifelse(automated == 0, 0, attendant))

# Fix sharedRH prices
price <- survey %>% 
  select(obsID, mode, price) %>% 
  spread(mode, price) %>% 
  mutate(sharedRH = sharedRH*RH) %>% 
  gather(key = "mode", value = "price", -obsID)

survey <- survey %>% 
  select(-price) %>% 
  left_join(price, by = c("obsID", "mode")) %>% 
  mutate(
    price = format(round(price, digits=2), nsmall = 2)
  )

# Fix sharedRH travelTime
time <- survey %>% 
  select(obsID, mode, travelTime) %>% 
  spread(mode, travelTime) %>% 
  mutate(sharedRH = round(sharedRH*RH)) %>% 
  gather(key = "mode", value = "travelTime", -obsID) 

survey <- survey %>% 
  select(-travelTime) %>% 
  left_join(time, by = c("obsID", "mode")) %>% 
  mutate(
    automated = ifelse(automated == 0, "No", "Yes"),
    attendant = ifelse(attendant == 0, "No",  "Yes")
  )


# Add image path for automated/attendant
imgBeg <- "'https://raw.githubusercontent.com/lkaplan25/AV_conjoint_survey_2022/main/survey_images/attendant_"

survey <- survey %>% 
  mutate(
    imgEnd = case_when(
      automated == "No" ~ "nonauto.png' alt='Not automated, No attendant present. Below text are an icon of an automated vehicle and an attendant that are both crossed out.'",
      (automated == "Yes" & attendant == "No") ~ "no.png' alt='Automated, No attendant present. Below text are an icon of an automated vehicle and an attendant that is crossed out.'",
      (automated == "Yes" & attendant == "Yes") ~ "yes.png' alt='Automated, Attendant present. Below text are an icon of an automated vehicle and an attendant.'"),
    imgPath = paste0(imgBeg, imgEnd)
  ) %>% 
  select(-imgEnd)

# Note: removed the code to save the survey. Survey included in the data_raw folder in the repo is the exact survey that was used in the research study. 


# Power analysis ---------------------------------------------------------------

survey_dummy <- dummy_cols(survey, c('mode', 'automated', 'attendant')) %>% 
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
        price = as.double(price)
    ) 

# Simulating random choices 
data <- cbc_choices(
    design = survey_dummy,
    obsID = "obsID"
)

# Estimate the same model on different size subsets of data

power <- cbc_power(
    nbreaks = 10,
    data = data,
    pars   = c("mode_bus", "mode_RH", "mode_sharedRH", "bus_automated_yes", "bus_attendant_yes", 
               "RH_automated_yes", "RH_attendant_yes", "sharedRH_automated_yes", "sharedRH_attendant_yes", "price", "travelTime"),
    outcome = "choice",
    obsID = "obsID", 
    n_q = 8
)

powerAnalysisPlot <- ggplot(power) +
    geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
    geom_point(aes(x = sampleSize, y = se, color = coef), size = 2) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0, 0.125)) +
    scale_x_continuous(limits = c(0, 5000)) +
    theme_bw() + 
    labs(
        x = "Sample size", 
        y = "Standard error", 
        color = "Coefficient"
    )
powerAnalysisPlot

ggsave(
    filename = here('figs', 'powerAnalysis.png'), 
    plot = powerAnalysisPlot, 
    width = 7, height = 5
)
