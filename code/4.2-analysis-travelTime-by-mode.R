# Load libraries and settings
source(here::here('code', '0setup.R'))

# Load models
load(here::here("models", "mxl_v2.RData"))

# Visualize results of estimated mixed logit WTP space model in which travel time was separated out by mode

# -----------------------------------------------------------------------------
# Get WTP estimates with 95% CI

# Estimate WTP in WTP space model:
coefs <- coef(mxl_wtp)
covariance <- vcov(mxl_wtp) 
wtp_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

# Computing the combinations of WTP draws
wtp_draws <- wtp_draws %>% 
  mutate(
    travelTime_bus                    = travelTime + travelTime_bus,
    travelTime_RH                     = travelTime + travelTime_RH,
    travelTime_sharedRH               = travelTime + travelTime_sharedRH,
    wtp_bus_autoYes                   = mode_bus + bus_automated_yes,
    wtp_bus_autoYes_attendantYes      = mode_bus + bus_automated_yes + bus_attendant_yes,
    wtp_RH_autoYes                    = mode_RH + RH_automated_yes,
    wtp_RH_autoYes_attendantYes       = mode_RH + RH_automated_yes + RH_attendant_yes,
    wtp_sharedRH_autoYes              = mode_sharedRH + sharedRH_automated_yes,
    wtp_sharedRH_autoYes_attendantYes = mode_sharedRH + sharedRH_automated_yes + sharedRH_attendant_yes
  )

wtp_ci2 <- ci(wtp_draws)
wtp_ci2 <- wtp_ci2[-1,] # Drop lambda (we won't plot this)
wtp_ci2


# -----------------------------------------------------------------------------
# Plot results

wtp_ci <- wtp_ci2 

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
wtp_travelTime <- wtp_ci %>% filter(par %in% c('travelTime_bus', 'travelTime_RH', 'travelTime_sharedRH'))
wtp_mode <- wtp_ci %>% filter(par %in% c('mode_bus', 'wtp_bus_autoYes', 'wtp_bus_autoYes_attendantYes', 
                                         'mode_RH', 'wtp_RH_autoYes', 'wtp_RH_autoYes_attendantYes', 
                                         'mode_sharedRH','wtp_sharedRH_autoYes', 'wtp_sharedRH_autoYes_attendantYes'))

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)

# Plotting Travel Time----------------------------------------------

df_travelTime <- wtp_travelTime %>% 
  mutate(
    mode = par
  )

# Get upper and lower bounds (plots should have the same x-axis)
xmin <- floor(min(df_travelTime$lower))
xmax <- ceiling(max(df_travelTime$upper))

# Comparing travel times for different modes

plot_travelTime <- df_travelTime %>% 
  ggplot(aes(y = par, x = mean, xmin = lower, xmax = upper, group = mode)) +
  geom_point(size = 1.5, color = "navyblue") +
  geom_errorbar(width = 0.3, color = 'navyblue') +
  #facet_grid(mode~., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(xmin, xmax))

plot_travelTime


ggsave(
  filename = here('figs', 'wtp_travelTime.png'),
  plot = plot_travelTime,
  width = 7, height = 2.75
)


# Plotting Mode----------------------------------------------------------
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
  filename = here('figs', 'wtp_mode_diff_travelTime.png'),
  plot = plot_mode_automated_attendant_All,
  width = 7, height = 2.75
)

