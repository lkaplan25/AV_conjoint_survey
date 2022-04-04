# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)
library(maddTools)
library(fastDummies)
library(cowplot)
options(dplyr.width = Inf)
library(dplyr)
library(forcats)
library(ggbump)
library(ggrepel)
library(directlabels)


# Visualize results of estimated mixed logit WTP space model

# -----------------------------------------------------------------------------
# Get WTP estimates with 95% CI

load(here::here("models", "mnl.RData")) # Load models

# Estimate WTP in WTP space model:
coefs <- coef(mxl_wtp)
covariance <- vcov(mxl_wtp)
wtp_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))

# Computing the combinations of WTP draws
wtp_draws <- wtp_draws %>% 
  mutate(
    wtp_bus_autoYes = mode_bus_mu + bus_automated_yes_mu,
    wtp_bus_autoYes_attendantYes = mode_bus_mu + bus_automated_yes_mu + bus_attendant_yes_mu,
    wtp_RH_autoYes = mode_RH_mu + RH_automated_yes_mu,
    wtp_RH_autoYes_attendantYes = mode_RH_mu + RH_automated_yes_mu + RH_attendant_yes_mu,
    wtp_sharedRH_autoYes = mode_sharedRH_mu + sharedRH_automated_yes_mu,
    wtp_sharedRH_autoYes_attendantYes = mode_sharedRH_mu + sharedRH_automated_yes_mu + sharedRH_attendant_yes_mu
  )


wtp_ci2 <- ci(wtp_draws)
wtp_ci2 <- wtp_ci2[-1,] # Drop lambda (we won't plot this)
wtp_ci2

# -----------------------------------------------------------------------------
# Plot results

wtp_ci <- wtp_ci2 

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
wtp_travelTime <- wtp_ci %>% filter(par == 'travelTime')
wtp_mode <- wtp_ci %>% filter(par %in% c('mode_bus_mu', 'wtp_bus_autoYes', 'wtp_bus_autoYes_attendantYes', 
                                         'mode_RH_mu', 'wtp_RH_autoYes', 'wtp_RH_autoYes_attendantYes', 
                                         'mode_sharedRH_mu','wtp_sharedRH_autoYes', 'wtp_sharedRH_autoYes_attendantYes'))

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)

df_mode <- wtp_mode %>% 
  mutate(mode = case_when(
    par %in% c('mode_bus_mu', 'mode_RH_mu', 'mode_sharedRH_mu') ~ "Not\nAutomated",
    par %in% c('wtp_bus_autoYes', 'wtp_RH_autoYes','wtp_sharedRH_autoYes') ~ "Automated,\nNo Attendant\nPresent",
    par %in% c('wtp_bus_autoYes_attendantYes', 'wtp_RH_autoYes_attendantYes','wtp_sharedRH_autoYes_attendantYes') ~ "Automated,\nAttendant\nPresent"
  ),
  par = recode_factor(wtp_mode$par, "mode_bus_mu" = "Bus", "mode_RH_mu" = "Ride-hailing", "mode_sharedRH_mu" = "Shared Ride-hailing",
                      'wtp_bus_autoYes' = "Bus", 'wtp_RH_autoYes' = "Ride-hailing",'wtp_sharedRH_autoYes' = "Shared Ride-hailing",
                      'wtp_bus_autoYes_attendantYes' = "Bus", 'wtp_RH_autoYes_attendantYes' = "Ride-hailing",'wtp_sharedRH_autoYes_attendantYes' = "Shared Ride-hailing")
  )



# Get upper and lower bounds (plots should have the same x-axis)
xmin <- floor(min(df_mode$lower))
xmax <- ceiling(max(df_mode$upper))


# Comparing all mode options 

plot_mode_automated_attendant_All <- df_mode %>% 
  ggplot(aes(y = par, x = mean, xmin = lower, xmax = upper)) +
  geom_point(size = 1.5, color = "navyblue") +
  geom_errorbar(width = 0.3, color = 'navyblue') +
  facet_grid(mode~., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  labs(y = NULL, x = 'Willingness to Pay ($1) relative to rail',
       title = "AV preferences shift with addition of an attendant",
       subtitle = "Automation alone does not drastically alter mode preferences") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(
    text = element_text(size=14, family = "Fira Sans", face = "bold"),
    strip.text.y = element_text(angle = 0),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12),
    plot.subtitle = element_text(face = "plain", size = 12)
  ) +
  scale_x_continuous(limits = c(xmin, xmax)) 

plot_mode_automated_attendant_All


ggsave(
  filename = here('figs', 'wtp_mode.png'), 
  plot = plot_mode_automated_attendant_All, 
  width = 9, height = 5
)


## Subgroup analysis-------------
### Gender---------
# Load estimated models
load(here::here("models", "mnl_wtp_gender.RData"))


# Get WTP estimates with 95% CI

# Take 10,000 draws of the coefficients of each model
coefs_A <- coef(mnl_wtp_A)
covariance_A <- vcov(mnl_wtp_A)
coef_draws_A <- as.data.frame(MASS::mvrnorm(10^4, coefs_A, covariance_A))

coefs_B <- coef(mnl_wtp_B)
covariance_B <- vcov(mnl_wtp_B)
coef_draws_B <- as.data.frame(MASS::mvrnorm(10^4, coefs_B, covariance_B))

# Computing the combinations of WTP draws
coef_draws_A <- coef_draws_A %>% 
  mutate(
    wtp_bus_autoYes = mode_bus + bus_automated_yes,
    wtp_bus_autoYes_attendantYes = mode_bus + bus_automated_yes + bus_attendant_yes,
    wtp_RH_autoYes = mode_RH + RH_automated_yes,
    wtp_RH_autoYes_attendantYes = mode_RH + RH_automated_yes + RH_attendant_yes,
    wtp_sharedRH_autoYes = mode_sharedRH + sharedRH_automated_yes,
    wtp_sharedRH_autoYes_attendantYes = mode_sharedRH + sharedRH_automated_yes + sharedRH_attendant_yes
  )

wtp_ci_A <- ci(coef_draws_A)
wtp_ci_A <- wtp_ci_A[-1,] %>% # Drop lambda (we won't plot this)
  mutate(
    group = "Male"
  )
wtp_ci_A$par <- row.names(wtp_ci_A)

coef_draws_B <- coef_draws_B%>% 
  mutate(
    wtp_bus_autoYes = mode_bus + bus_automated_yes,
    wtp_bus_autoYes_attendantYes = mode_bus + bus_automated_yes + bus_attendant_yes,
    wtp_RH_autoYes = mode_RH + RH_automated_yes,
    wtp_RH_autoYes_attendantYes = mode_RH + RH_automated_yes + RH_attendant_yes,
    wtp_sharedRH_autoYes = mode_sharedRH + sharedRH_automated_yes,
    wtp_sharedRH_autoYes_attendantYes = mode_sharedRH + sharedRH_automated_yes + sharedRH_attendant_yes
  )

wtp_ci_B <- ci(coef_draws_B)
wtp_ci_B <- wtp_ci_B[-1,] %>% # Drop lambda (we won't plot this)
  mutate(
    group = "Female",
  )
wtp_ci_B$par <- row.names(wtp_ci_B)


# Rejoin data frames

wtp_gender  <-  wtp_ci_A %>% 
  full_join(wtp_ci_B, by = c('mean', 'lower', 'upper', 'group', 'par'))


# Plot results

# Separate coefficient CIs by attribute 
wtp_mode <- wtp_gender %>% filter(par %in% c('mode_bus', 'wtp_bus_autoYes', 'wtp_bus_autoYes_attendantYes', 
                                             'mode_RH', 'wtp_RH_autoYes', 'wtp_RH_autoYes_attendantYes', 
                                             'mode_sharedRH','wtp_sharedRH_autoYes', 'wtp_sharedRH_autoYes_attendantYes'))

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)

df_mode <- wtp_mode %>% 
  mutate(mode = case_when(
    par %in% c('mode_bus', 'mode_RH', 'mode_sharedRH') ~ "Not\nAutomated",
    par %in% c('wtp_bus_autoYes', 'wtp_RH_autoYes','wtp_sharedRH_autoYes') ~ "Automated,\nNo Attendant\nPresent",
    par %in% c('wtp_bus_autoYes_attendantYes', 'wtp_RH_autoYes_attendantYes','wtp_sharedRH_autoYes_attendantYes') ~ "Automated,\nAttendant\nPresent"
  ),
  par = recode_factor(wtp_mode$par, "mode_bus" = "Bus", "mode_RH" = "Ride-hailing", "mode_sharedRH" = "Shared Ride-hailing",
                      'wtp_bus_autoYes' = "Bus", 'wtp_RH_autoYes' = "Ride-hailing",'wtp_sharedRH_autoYes' = "Shared Ride-hailing",
                      'wtp_bus_autoYes_attendantYes' = "Bus", 'wtp_RH_autoYes_attendantYes' = "Ride-hailing",'wtp_sharedRH_autoYes_attendantYes' = "Shared Ride-hailing")
  )


# Get upper and lower bounds (plots should have the same x-axis)
xmin <- floor(min(df_mode$lower))
xmax <- ceiling(max(df_mode$upper))

# Plot the WTP for each mode *with 95% CI*

plotColors = c("#F16814", "#3690BF")

plot_mode_automated_attendant <- df_mode %>% 
  ggplot(aes(x = par, y = mean, ymin = lower, ymax = upper, group = group, color = group)) +
  geom_point(size = 1.5, position=position_dodge(width = .5)) +
  geom_errorbar(width = 0.5, position=position_dodge(width = .5)) +
  coord_flip() +
  facet_grid(mode~., scales = "free_y", space = "free") +
  labs(x = NULL, y = 'Willingness to Pay ($1) relative to rail',
       subtitle = "Men willing to pay premium for automation + attendant") +
  scale_y_continuous(limits = c(xmin, xmax)) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    text = element_text(size=14, family = "Fira Sans", face = "bold"),
    strip.text.y = element_text(angle = 0),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12),
    legend.title=element_blank()
  ) +
  scale_color_manual(
    values = plotColors) 

plot_mode_automated_attendant

# Save plot 

ggsave(
  filename = here('figs', 'wtp_mode_gender.png'), 
  plot = plot_mode_automated_attendant, 
  width = 9, height = 5
)


# Scenario analyses ----------------------------------

# Create full df of scenarios 

df <- read_csv(here('sims', 'scenarios.csv')) %>% 
  mutate(
    price_reduction = 1
  )

price_reduction <- seq(1, .7, by = -0.05) # Define potential price reductions (up to 30%)

n <- length(price_reduction)  # Number of simulations


# Repeat full df for price levels

scenarios <- rep_df(df, n)

test <- scenarios %>% 
  filter(mode == "RH")

price_reduction <- rep(price_reduction, 18) #adjust based on number of scenarios or price levels

scenarios[which(scenarios$mode == "RH"),]$price_reduction <- price_reduction
scenarios[which(scenarios$mode == "sharedRH"),]$price_reduction <- price_reduction


scenarios <- scenarios %>% 
  mutate(
    price = price*price_reduction
  )



# Create dummy coded variables
scenarios <- dummy_cols(scenarios, c('mode', 'automated', 'attendant')) %>% 
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
    sharedRH_attendant_no = mode_sharedRH*attendant_No
  )




scenarios$obsID <- rep(seq(nrow(scenarios)/4), each = 4) # Reset obsIDs

# -----------------------------------------------------------------------------
# For each case, simulate the market share predictions

sens_price <- predict(
  mxl_dummy,
  newdata = scenarios, 
  obsID = 'obsID', 
  ci = 0.95,
  returnData = TRUE
) %>% 
  mutate(
    price = as.double(price),
    percent_red = 1 - price_reduction
  )

head(sens_price)

plot_red <- rep(rep(seq(0, .3, by = 0.05), each = 4), 18)

sens_price$percent_red <- plot_red

sens_price <- sens_price %>% 
  mutate(
    scenario_type = case_when(
      (percent_red == 0.3) ~ paste0(scenario_type, "Discount"),
      TRUE ~ scenario_type
    ),
    label = case_when(
      scenario_type == "baseline" ~ "Status Quo",
      scenario_type == "baselineDiscount" ~ "REMOVE",
      scenario_type == "auto" ~  "Automated",
      scenario_type == "autoDiscount" ~ "Automated,\n30% discount",
      scenario_type == "autoAttendant" ~ "Automated,\nattendant\npresent",
      scenario_type == "autoAttendantDiscount" ~ "Automated,\nattendant present,\n30% discount"
    ),
    mode = case_when(
      mode == "RH" ~ "Ride-hailing",
      mode == "rail" ~ "Rail",
      mode == "sharedRH" ~ "Shared\nRide-hailing",
      mode == "bus" ~ "Bus"
    )
  )




#--------------------------
# Bump chart

## Scenario 1
bump_chart1 <- sens_price %>% 
  filter(scenario_num == 1, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
  select(scenario_type, percent_red, everything()) %>% 
  mutate(
    label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" ))
  ) %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = predicted_prob_lower, ymax = predicted_prob_upper, group = mode, color = mode)) +
  geom_bump(size = .75) +
  #geom_ribbon(alpha = .2, color = NA) +
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        text = element_text(family = "Fira Sans")
  ) +
  labs(
    title = "Pro-Rail Scenario",
    x = NULL,
    y = "Market Share"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1)))


bump_chart1


## Scenario 2
bump_chart2 <- sens_price %>% 
  filter(scenario_num == 2, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
  select(scenario_type, percent_red, everything()) %>% 
  mutate(
    label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" ))
  ) %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = predicted_prob_lower, ymax = predicted_prob_upper, group = mode, color = mode)) +
  geom_bump(size = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "none",
        text = element_text(family = "Fira Sans")
  ) +
  labs(
    title = "Rail with Transfer Scenario",
    x = NULL,
    y = "Market Share"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1)))

bump_chart2

## Scenario 3
bump_chart3 <- sens_price %>% 
  filter(scenario_num == 3, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
  select(scenario_type, percent_red, everything()) %>% 
  mutate(
    label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" ))
  ) %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = predicted_prob_lower, ymax = predicted_prob_upper, group = mode, color = mode)) +
  geom_bump(size = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        text = element_text(family = "Fira Sans")
  ) +
  labs(
    title = "Pro-Bus Scenario",
    x = NULL,
    y = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1)))

bump_chart3

## Scenario 4

bump_chart4 <- sens_price %>% 
  filter(scenario_num == 4, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
  select(scenario_type, percent_red, everything()) %>% 
  mutate(
    label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" ))
  ) %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = predicted_prob_lower, ymax = predicted_prob_upper, group = mode, color = mode)) +
  geom_bump(size = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        text = element_text(family = "Fira Sans")
  ) +
  labs(
    title = "Trip from Lower Income Area Scenario",
    x = NULL,
    y = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1)))

bump_chart4

## Scenario 5

bump_chart5 <- sens_price %>% 
  filter(scenario_num == 5, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
  select(scenario_type, percent_red, everything()) %>% 
  mutate(
    label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" ))
  ) %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = predicted_prob_lower, ymax = predicted_prob_upper, group = mode, color = mode)) +
  geom_bump(size = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) + #changed from last.points
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        text = element_text(family = "Fira Sans")) +
  labs(
    title = "Long Trip Scenario",
    x = NULL,
    y = "Market Share"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1)))

bump_chart5

## Scenario 6

bump_chart6 <- sens_price %>% 
  filter(scenario_num == 6, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
  select(scenario_type, percent_red, everything()) %>% 
  mutate(
    label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" ))
  ) %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = predicted_prob_lower, ymax = predicted_prob_upper, group = mode, color = mode)) +
  geom_bump(size = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "none",
        text = element_text(family = "Fira Sans")) +
  labs(
    title = "Bad Transit Options Scenario",
    x = NULL,
    y = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1)))

bump_chart6


scenarioPlotAll <- plot_grid(
  bump_chart5,
  bump_chart3,
  bump_chart1,
  bump_chart4,
  bump_chart2,
  bump_chart6,
  nrow = 3, ncol = 2
)

ggsave(
  filename = here('figs', 'scenarioPlotAll.png'), 
  plot = scenarioPlotAll, 
  width = 10, height = 6.5
)




