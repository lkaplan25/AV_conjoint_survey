# Visualize results of estimated mixed logit WTP space model

# Load libraries and settings
source(here::here('code', '0setup.R'))

# Load models
mxl_wtp <- readRDS(here::here("models", "mxl_wtp.Rds"))
mxl_wtp_weighted <- readRDS(here::here("models", "mxl_wtp_weighted.Rds"))
mxl_wtp_income_low <- readRDS(here::here("models", "mxl_wtp_income_low.Rds"))
mxl_wtp_income_high <- readRDS(here::here("models", "mxl_wtp_income_high.Rds"))
mxl_wtp_gender_male <- readRDS(here::here("models", "mxl_wtp_gender_male.Rds"))
mxl_wtp_gender_female <- readRDS(here::here("models", "mxl_wtp_gender_female.Rds"))
                                                                                          
# Functions to compute mode variables from draws

compute_wtp_vars_15 <- function(draws) {
  time <- 15 
  draws <- draws %>% 
    mutate(
      wtp_bus = mode_bus + time*travelTime_bus,
      wtp_bus_autoYes = mode_bus + bus_automated_yes + time*travelTime_bus,
      wtp_bus_autoYes_attendantYes = mode_bus + bus_automated_yes + bus_attendant_yes + time*travelTime_bus,
      wtp_RH = mode_RH + time*travelTime_RH,
      wtp_RH_autoYes = mode_RH + RH_automated_yes + time*travelTime_RH,
      wtp_RH_autoYes_attendantYes = mode_RH + RH_automated_yes + RH_attendant_yes + time*travelTime_RH,
      wtp_sharedRH = mode_sharedRH + time*travelTime_sharedRH,
      wtp_sharedRH_autoYes = mode_sharedRH + sharedRH_automated_yes + time*travelTime_sharedRH,
      wtp_sharedRH_autoYes_attendantYes = mode_sharedRH + sharedRH_automated_yes + sharedRH_attendant_yes + time*travelTime_sharedRH
  )
  return(draws)
}

compute_wtp_vars_45 <- function(draws) {
  time <- 45 
  draws <- draws %>% 
    mutate(
      wtp_bus = mode_bus + time*travelTime_bus,
      wtp_bus_autoYes = mode_bus + bus_automated_yes + time*travelTime_bus,
      wtp_bus_autoYes_attendantYes = mode_bus + bus_automated_yes + bus_attendant_yes + time*travelTime_bus,
      wtp_RH = mode_RH + time*travelTime_RH,
      wtp_RH_autoYes = mode_RH + RH_automated_yes + time*travelTime_RH,
      wtp_RH_autoYes_attendantYes = mode_RH + RH_automated_yes + RH_attendant_yes + time*travelTime_RH,
      wtp_sharedRH = mode_sharedRH + time*travelTime_sharedRH,
      wtp_sharedRH_autoYes = mode_sharedRH + sharedRH_automated_yes + time*travelTime_sharedRH,
      wtp_sharedRH_autoYes_attendantYes = mode_sharedRH + sharedRH_automated_yes + sharedRH_attendant_yes + time*travelTime_sharedRH
    )
  return(draws)
}

# Function to obtain a formatted data frame of mode coefficients for plotting

get_df_mode <- function(wtp_ci) {
  
  wtp_mode <- wtp_ci %>% 
    filter(par %in% c(
      'wtp_bus', 'wtp_bus_autoYes', 'wtp_bus_autoYes_attendantYes',
      'wtp_RH', 'wtp_RH_autoYes', 'wtp_RH_autoYes_attendantYes',
      'wtp_sharedRH','wtp_sharedRH_autoYes', 'wtp_sharedRH_autoYes_attendantYes'
    ))
  
  # Create data frames for plotting each attribute:
  #   level   = The attribute level (x-axis)
  #   utility = The utility associated with each level (y-axis)
  
  df_mode <- wtp_mode %>% 
    mutate(
      mode = case_when(
        par %in% c('wtp_bus', 'wtp_RH', 'wtp_sharedRH') ~ "Not\nAutomated",
        par %in% c(
          'wtp_bus_autoYes', 'wtp_RH_autoYes','wtp_sharedRH_autoYes'
        ) ~ "Automated,\nNo Attendant\nPresent",
        par %in% c(
          'wtp_bus_autoYes_attendantYes', 'wtp_RH_autoYes_attendantYes',
          'wtp_sharedRH_autoYes_attendantYes'
        ) ~ "Automated,\nAttendant\nPresent"
      ),
      par = recode_factor(
        wtp_mode$par, 
        "wtp_bus" = "Bus", 
        "wtp_RH" = "Ride-hailing", 
        "wtp_sharedRH" = "Shared Ride-hailing",
        'wtp_bus_autoYes' = "Bus", 
        'wtp_RH_autoYes' = "Ride-hailing",
        'wtp_sharedRH_autoYes' = "Shared Ride-hailing",
        'wtp_bus_autoYes_attendantYes' = "Bus", 
        'wtp_RH_autoYes_attendantYes' = "Ride-hailing",
        'wtp_sharedRH_autoYes_attendantYes' = "Shared Ride-hailing"
      )
    )
  return(df_mode)
}

# Plot theme function 

plot_theme <- function() {
  return(
    theme_bw() + 
    theme(
      text = element_text(size=10, family = "Roboto Condensed", face = "bold"),
      strip.text.y = element_text(angle = 0),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 12),
      plot.subtitle = element_text(face = "plain", size = 12),
      legend.position = "none"
    )
  )
}

## Full Model Analysis---------------------------------------------------------

### Unweighted Model----------------------------------------------------------

# Estimate WTP in WTP space model:
coefs <- coef(mxl_wtp)
covariance <- vcov(mxl_wtp)
wtp_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

#### Short Trip-----------------

# Computing the combinations of WTP draws
wtp_draws <- compute_wtp_vars_15(wtp_draws)
wtp_ci <- ci(wtp_draws)
wtp_ci <- wtp_ci[-1,] # Drop lambda (we won't plot this)
wtp_ci

# Plot results

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
df_mode_short <- get_df_mode(wtp_ci)

#### Long Trip-----------------

# Computing the combinations of WTP draws
wtp_draws <- compute_wtp_vars_45(wtp_draws)
wtp_ci <- ci(wtp_draws)
wtp_ci <- wtp_ci[-1,] # Drop lambda (we won't plot this)
wtp_ci

# Plot results

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
df_mode_long <- get_df_mode(wtp_ci)


# Get upper and lower bounds (plots should have the same x-axis)
xmin <- floor(min(c(df_mode_short$lower, df_mode_long$lower)))
xmax <- ceiling(max(c(df_mode_short$upper, df_mode_long$upper)))

# Comparing all mode options 

plot_mode_shortTrip <- df_mode_short %>% 
  ggplot(aes(y = par, x = mean, xmin = lower, xmax = upper)) +
  geom_point(size = 1.5, color = "navyblue") +
  geom_errorbar(width = 0.3, color = 'navyblue') +
  facet_grid(mode~., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  labs(
    y = NULL, 
    x = NULL,
    title = "Short Trip (15min)"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  plot_theme()

plot_mode_shortTrip


# Comparing all mode options 

plot_mode_longTrip <- df_mode_long %>% 
  ggplot(aes(y = par, x = mean, xmin = lower, xmax = upper)) +
  geom_point(size = 1.5, color = "navyblue") +
  geom_errorbar(width = 0.3, color = 'navyblue') +
  facet_grid(mode~., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  labs(
    y = NULL, 
    x = 'Willingness to Pay ($1) relative to rail',
    title = "Long Trip (45min)"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  plot_theme()

plot_mode_longTrip

plot_modes <- plot_grid(
  plot_mode_shortTrip,
  plot_mode_longTrip,
  nrow = 2
)

title <- ggdraw() + 
  draw_label("AV preferences shift with addition of an attendant", 
             hjust = 0.5,
             fontfamily = "Roboto Condensed",
             fontface = "bold", 
             size = 16)

subtitle <- ggdraw() + 
  draw_label("Automation alone does not drastically alter mode preferences", 
             hjust = 0.5,
             fontfamily = "Roboto Condensed",
             fontface = "plain",
             size = 12
             )

plot_combined <- plot_grid(title,subtitle, plot_modes, ncol=1, rel_heights=c(0.09, 0.08, 1)) 
plot_combined

ggsave(
  filename = here::here('figs', 'wtp_mode.png'),
  plot = plot_combined,
  width = 7, height = 9
)

## Subgroup analysis-------------

### Gender---------

mxl_wtp_A <- mxl_wtp_gender_male
mxl_wtp_B <- mxl_wtp_gender_female

# Get WTP estimates with 95% CI

# Take 10,000 draws of the coefficients of each model
coefs_A <- coef(mxl_wtp_A)
covariance_A <- vcov(mxl_wtp_A)
coef_draws_A <- as.data.frame(MASS::mvrnorm(10^4, coefs_A, covariance_A))
coefs_B <- coef(mxl_wtp_B)
covariance_B <- vcov(mxl_wtp_B)
coef_draws_B <- as.data.frame(MASS::mvrnorm(10^4, coefs_B, covariance_B))

#### Short Trip ----------
# Computing the combinations of WTP draws
coef_draws_A <- compute_wtp_vars_15(coef_draws_A)
wtp_ci_A <- ci(coef_draws_A)
wtp_ci_A <- wtp_ci_A[-1,] 
wtp_ci_A$group <- "Male"
wtp_ci_A$par <- row.names(wtp_ci_A)

coef_draws_B <- compute_wtp_vars_15(coef_draws_B)
wtp_ci_B <- ci(coef_draws_B)
wtp_ci_B <- wtp_ci_B[-1,] 
wtp_ci_B$group <- "Female"
wtp_ci_B$par <- row.names(wtp_ci_B)

# Rejoin data frames

wtp_groups  <-  rbind(wtp_ci_A, wtp_ci_B) 

# Plot results

wtp_mode_short <- get_df_mode(wtp_groups)


#### Long Trip --------
# Computing the combinations of WTP draws
coef_draws_A <- compute_wtp_vars_45(coef_draws_A)
wtp_ci_A <- ci(coef_draws_A)
wtp_ci_A <- wtp_ci_A[-1,] 
wtp_ci_A$group <- "Male"
wtp_ci_A$par <- row.names(wtp_ci_A)

coef_draws_B <- compute_wtp_vars_45(coef_draws_B)
wtp_ci_B <- ci(coef_draws_B)
wtp_ci_B <- wtp_ci_B[-1,] 
wtp_ci_B$group <- "Female"
wtp_ci_B$par <- row.names(wtp_ci_B)

# Rejoin data frames

wtp_groups  <-  rbind(wtp_ci_A, wtp_ci_B) 

# Plot results

wtp_mode_long <- get_df_mode(wtp_groups)

# Get upper and lower bounds (plots should have the same x-axis)
xmin <- floor(min(c(wtp_mode_short$lower, wtp_mode_long$lower)))
xmax <- ceiling(max(c(wtp_mode_short$upper, wtp_mode_long$upper)))

# Plot the WTP for each mode *with 95% CI*

plotColors = c("#F16814", "#3690BF")

plot_mode_gender_shortTrip <- wtp_mode_short %>% 
  ggplot(
    aes(
      y = par, x = mean, xmin = lower, xmax = upper,  
      group = group, color = group
    )
  ) +
  geom_point(size = 1.5, position = position_dodge(width = .5)) +
  geom_errorbar(width = 0.5, position = position_dodge(width = .5)) +
  facet_grid(mode~., scales = "free_x", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  scale_color_manual(values = plotColors) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Short Trip"
  ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
  plot_theme() 
  
plot_mode_gender_shortTrip

plot_mode_gender_longTrip <- wtp_mode_long %>% 
  ggplot(
    aes(
      y = par, x = mean, xmin = lower, xmax = upper,  
      group = group, color = group
    )
  ) +
  geom_point(size = 1.5, position = position_dodge(width = .5)) +
  geom_errorbar(width = 0.5, position = position_dodge(width = .5)) +
  facet_grid(mode~., scales = "free_x", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  scale_color_manual(values = plotColors) +
  labs(
    y = NULL, 
    x = 'Willingness to Pay ($1) relative to rail',
    subtitle = "Long Trip"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  plot_theme() +
  theme(legend.position="bottom")

plot_mode_gender_longTrip


plot_gender <- plot_grid(
  plot_mode_gender_shortTrip,
  plot_mode_gender_longTrip,
  nrow = 2
)

title <- ggdraw() + 
  draw_label("Men willing to pay more than women for automation & attendant", 
             hjust = 0.5,
             fontfamily = "Roboto Condensed",
             fontface = "bold", 
             size = 14)

plot_combined <- plot_grid(title, plot_gender, ncol=1, rel_heights=c(0.1, 1)) 
plot_combined

ggsave(
  filename = here::here('figs', 'wtp_mode_gender.png'),
  plot = plot_combined,
  width = 7, height = 9
)


### Income---------

mxl_wtp_A <- mxl_wtp_income_high
mxl_wtp_B <- mxl_wtp_income_low

# Get WTP estimates with 95% CI

# Take 10,000 draws of the coefficients of each model
coefs_A <- coef(mxl_wtp_A)
covariance_A <- vcov(mxl_wtp_A)
coef_draws_A <- as.data.frame(MASS::mvrnorm(10^4, coefs_A, covariance_A))
coefs_B <- coef(mxl_wtp_B)
covariance_B <- vcov(mxl_wtp_B)
coef_draws_B <- as.data.frame(MASS::mvrnorm(10^4, coefs_B, covariance_B))

#### Short Trip ----------
# Computing the combinations of WTP draws
coef_draws_A <- compute_wtp_vars_15(coef_draws_A)
wtp_ci_A <- ci(coef_draws_A)
wtp_ci_A <- wtp_ci_A[-1,] 
wtp_ci_A$group <- "Mid/High Income"
wtp_ci_A$par <- row.names(wtp_ci_A)

coef_draws_B <- compute_wtp_vars_15(coef_draws_B)
wtp_ci_B <- ci(coef_draws_B)
wtp_ci_B <- wtp_ci_B[-1,] 
wtp_ci_B$group <- "Low Income"
wtp_ci_B$par <- row.names(wtp_ci_B)

# Rejoin data frames

wtp_groups  <-  rbind(wtp_ci_A, wtp_ci_B) 

# Plot results

wtp_mode_short <- get_df_mode(wtp_groups)


#### Long Trip --------
# Computing the combinations of WTP draws
coef_draws_A <- compute_wtp_vars_45(coef_draws_A)
wtp_ci_A <- ci(coef_draws_A)
wtp_ci_A <- wtp_ci_A[-1,] 
wtp_ci_A$group <- "Mid/High Income"
wtp_ci_A$par <- row.names(wtp_ci_A)

coef_draws_B <- compute_wtp_vars_45(coef_draws_B)
wtp_ci_B <- ci(coef_draws_B)
wtp_ci_B <- wtp_ci_B[-1,] 
wtp_ci_B$group <- "Low Income"
wtp_ci_B$par <- row.names(wtp_ci_B)

# Rejoin data frames

wtp_groups  <-  rbind(wtp_ci_A, wtp_ci_B) 

# Plot results

wtp_mode_long <- get_df_mode(wtp_groups)

# Get upper and lower bounds (plots should have the same x-axis)
xmin <- floor(min(c(wtp_mode_short$lower, wtp_mode_long$lower)))
xmax <- ceiling(max(c(wtp_mode_short$upper, wtp_mode_long$upper)))

# Plot the WTP for each mode *with 95% CI*

plotColors = c("#F16814", "#3690BF")

plot_mode_income_shortTrip <- wtp_mode_short %>% 
  ggplot(
    aes(
      y = par, x = mean, xmin = lower, xmax = upper,  
      group = group, color = group
    )
  ) +
  geom_point(size = 1.5, position = position_dodge(width = .5)) +
  geom_errorbar(width = 0.5, position = position_dodge(width = .5)) +
  facet_grid(mode~., scales = "free_x", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  scale_color_manual(values = plotColors) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Short Trip"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  plot_theme() 

plot_mode_income_shortTrip

plot_mode_income_longTrip <- wtp_mode_long %>% 
  ggplot(
    aes(
      y = par, x = mean, xmin = lower, xmax = upper,  
      group = group, color = group
    )
  ) +
  geom_point(size = 1.5, position = position_dodge(width = .5)) +
  geom_errorbar(width = 0.5, position = position_dodge(width = .5)) +
  facet_grid(mode~., scales = "free_x", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax)) +
  scale_color_manual(values = plotColors) +
  labs(
    y = NULL, 
    x = 'Willingness to Pay ($1) relative to rail',
    subtitle = "Long Trip"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  plot_theme() +
  theme(legend.position="bottom")

plot_mode_income_longTrip


plot_income <- plot_grid(
  plot_mode_income_shortTrip,
  plot_mode_income_longTrip,
  nrow = 2
)

title <- ggdraw() + 
  draw_label("Income differences", 
             hjust = 0.5,
             fontfamily = "Roboto Condensed",
             fontface = "bold", 
             size = 14)

plot_combined <- plot_grid(title, plot_income, ncol=1, rel_heights=c(0.1, 1)) 
plot_combined

ggsave(
  filename = here::here('figs', 'wtp_mode_income.png'),
  plot = plot_combined,
  width = 7, height = 9
)



# Scenario analyses ----------------------------------

# Create full df of scenarios 

df <- read_csv(here::here('sims', 'scenarios.csv')) %>% 
  mutate(
    price_reduction = 1
  )

price_reduction <- seq(1, .7, by = -0.05) # Define potential price reductions (up to 30%)

n <- length(price_reduction)  # Number of simulations


# Repeat full df for price levels

scenarios <- rep_df(df, n)

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
  )

scenarios$obsID <- rep(seq(nrow(scenarios)/4), each = 4) # Reset obsIDs


### Full model -----------------------------------------------------------------------------
# For each case, simulate the market share predictions

probs_mxl_wtp <- predict(
  mxl_wtp,
  newdata = scenarios, 
  obsID = 'obsID', 
  interval = "confidence",
  returnData = "TRUE"
) 

head(probs_mxl_wtp)

plot_red <- rep(rep(seq(0, .3, by = 0.05), each = 4), 18)

probs_mxl_wtp$percent_red <- plot_red

probs_mxl_wtp <- probs_mxl_wtp %>% 
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

### Low-income model -----------------------------------------------------------------------------
# For each case, simulate the market share predictions

probs_mxl_wtp_income_low <- predict(
  mxl_wtp_income_low,
  newdata = scenarios, 
  obsID = 'obsID', 
  interval = "confidence",
  returnData = "TRUE"
)

head(probs_mxl_wtp_income_low)

probs_mxl_wtp_income_low$percent_red <- plot_red

probs_mxl_wtp_income_low <- probs_mxl_wtp_income_low %>% 
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


# Bump chart

get_scenario <- function(probs_model, scenario) {
  probs_model <- probs_model %>% 
    filter(scenario_num == scenario, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
    mutate(
      label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" )))
  return(probs_model)
}

bump_plot_theme <- function() {
  return(
    theme_minimal_hgrid(font_size = 10) +
      theme(
        legend.position = "none",
        text = element_text(family = "Fira Sans")
      )
  )
}

## Scenario 1

s1 <- get_scenario(probs_mxl_wtp, 1)

bump_chart1 <- s1 %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = 0, ymax = .5, group = mode, color = mode)) +
  geom_bump(linewidth = .75) +
  #geom_ribbon(alpha = .2, color = NA) +
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  labs(
    title = "Pro-Rail Scenario",
    x = NULL,
    y = "Market Share"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  bump_plot_theme()

bump_chart1

ggsave(
  filename = here::here('figs', 'proRailScenario.png'), 
  plot = bump_chart1, 
  width = 7, height = 4
)

## Scenario 2 

s2 <- get_scenario(probs_mxl_wtp, 2)

bump_chart2 <- s2 %>% 
  filter(scenario_num == 2, percent_red %in% c(0.00, 0.30), scenario_type != "baselineDiscount" ) %>% 
  mutate(
    label = fct_relevel(label, c("Status Quo", "Automated", "Automated,\n30% discount", "Automated,\nattendant\npresent", "Automated,\nattendant present,\n30% discount" ))
  ) %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = 0, ymax = .5, group = mode, color = mode)) +
  geom_bump(linewidth = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  labs(
    title = "Rail with Transfer Scenario",
    x = NULL,
    y = "Market Share"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  bump_plot_theme()

bump_chart2

ggsave(
  filename = here::here('figs', 'rail_with_transfer.png'), 
  plot = bump_chart2, 
  width = 7, height = 4
)

## Scenario 3

s3 <- get_scenario(probs_mxl_wtp, 3)

bump_chart3 <- s3 %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = 0, ymax = .5, group = mode, color = mode)) +
  geom_bump(linewidth = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  labs(
    title = "Pro-Bus Scenario",
    x = NULL,
    y = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  bump_plot_theme()

bump_chart3

ggsave(
  filename = here::here('figs', 'proBus_scenario.png'), 
  plot = bump_chart3, 
  width = 7, height = 4
)


## Scenario 4 - Trip from Lower Income Area, using low-income model for this scenario

s4 <- get_scenario(probs_mxl_wtp_income_low, 4)

bump_chart4 <- s4 %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = 0, ymax = .5, group = mode, color = mode)) +
  geom_bump(linewidth = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  labs(
    title = "Trip from Lower Income Area Scenario (low income model)",
    x = NULL,
    y = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  bump_plot_theme()

bump_chart4

ggsave(
  filename = here::here('figs', 'low_income_scenario_lowIncomeModel.png'), 
  plot = bump_chart5, 
  width = 7, height = 4
)

## Scenario 5
s5 <- get_scenario(probs_mxl_wtp_income_low, 5)

bump_chart5 <- s5 %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = 0, ymax = .5, group = mode, color = mode)) +
  geom_bump(linewidth = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  labs(
    title = "Long Trip Scenario",
    x = NULL,
    y = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  bump_plot_theme()

bump_chart5

ggsave(
  filename = here::here('figs', 'longTripScenario.png'), 
  plot = bump_chart5, 
  width = 7, height = 4
)

## Scenario 6

s6 <- get_scenario(probs_mxl_wtp, 6)

bump_chart6 <- s6 %>% 
  ggplot(aes(x = label, y = predicted_prob, ymin = 0, ymax = .5, group = mode, color = mode)) +
  geom_bump(linewidth = .75) + 
  geom_point(size = 2) +
  #geom_errorbar(width = 0.3) +
  geom_dl(aes(label = mode), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = .75)) +
  labs(
    title = "Bad Transit Options Scenario",
    x = NULL,
    y = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(expand = expansion(add = c(.5, 1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  bump_plot_theme()

bump_chart6

ggsave(
  filename = here::here('figs', 'badTransitOptionsScenario.png'), 
  plot = bump_chart6, 
  width = 7, height = 4
)


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
  filename = here::here('figs', 'scenarioPlotAll.png'), 
  plot = scenarioPlotAll, 
  width = 10, height = 6.5
)


## Comparing scenario analyses outcomes full model vs. low income model -----

full <- probs_mxl_wtp %>% 
  filter(scenario_num == 4) %>% 
  select(predicted_prob)

low_income <- probs_mxl_wtp_income_low %>% 
  filter(scenario_num == 4) %>% 
  select(predicted_prob)

summary(full - low_income)

