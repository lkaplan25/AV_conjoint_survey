# Load libraries and settings
source(here::here('code', '0setup.R'))

# Load models
load(here::here("models", "mxl_gender.RData"))


## Gender Subgroup analysis-------------

# Get WTP estimates with 95% CI

# Take 10,000 draws of the coefficients of each model
coefs_A <- coef(mxl_wtp_gender_A)
covariance_A <- vcov(mxl_wtp_gender_A)
coef_draws_A <- as.data.frame(MASS::mvrnorm(10^4, coefs_A, covariance_A))

coefs_B <- coef(mxl_wtp_gender_B)
covariance_B <- vcov(mxl_wtp_gender_B)
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

wtp_ci_A <- ci(coef_draws_A, ci = 0.95)
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

wtp_groups  <-  wtp_ci_A %>% 
  full_join(wtp_ci_B, by = c('mean', 'lower', 'upper', 'group', 'par'))


# Plot results

# Separate coefficient CIs by attribute 
wtp_mode <- wtp_groups %>% filter(par %in% c('mode_bus', 'wtp_bus_autoYes', 'wtp_bus_autoYes_attendantYes', 
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
  labs(x = NULL, y = 'Willingness to Pay ($1) relative to rail'
       #subtitle = "Men willing to pay more than women for automation + attendant"
  ) +
  scale_y_continuous(limits = c(xmin, xmax)) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    text = element_text(size=13, family = "Fira Sans", face = "bold"),
    strip.text.y = element_text(angle = 0),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12),
    legend.title=element_blank(),
    #legend.position="none"
  ) +
  scale_color_manual(
    values = plotColors) 

plot_mode_automated_attendant