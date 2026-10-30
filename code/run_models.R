# Pitcher plant analysis

# Preliminaries #### 
library(tidyverse)
library(janitor)
library(patchwork)
library(MASS)
library(svglite)

# Source plotting functions
source("code/continuous_plot.R")
source("code/endpoint_plot.R")

pitch_cont <- read_csv("data/GHContinuous.csv")
pitch_end <- read_csv("data/GHFinal.csv")

pitch_cont <- clean_names(pitch_cont)
pitch_end <- clean_names(pitch_end)

water_colors <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0")
cont_colors <- c(c("#08519c", "#6baed6", "#eff3ff"))

# Put months in the correct order
pitch_cont %>% 
  mutate(monthn = tolower(monthn)) |> 
  mutate(month_ord = factor(monthn, levels = c("feb", "mar", "apr", "may", "jun",
                                                "jul", "aug", "sep", "oct", "nov"))) -> pitch_cont

# Filter data to months through August (end of growing season)
pitch_cont_sub <- pitch_cont %>%
  filter(month_ord %in% c("feb", "mar", "apr","may","jun","jul","aug")) |> 
  mutate(watering_interval = tolower(watering_interval))

# Continuous plots ####

# Figure S1: Soil moisture difference across treatments
ggplot(pitch_cont_sub, aes(month_ord , soilmdiff, group = plantid,
                           color = watering_interval)) +
  geom_line(linewidth = 0.75) +
  geom_point() +
  labs(x = "month",
       y = "soil moisture difference (%)\n wet\u2192dry",
       color = "watering interval") +
  theme_classic(base_size = 14) +
  scale_color_manual(values = rev(water_colors)) -> figs1

# Save plot
#ggsave("figs/fig_s1.png")

# Figure 2: Plant volume, number of pitchers, pitchers per volume over time 
cont_plot1 <- make_cont_plot(pitch_cont, "plant_volume") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

cont_plot2 <- make_cont_plot(pitch_cont, "num_pitchers") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

(cont_plot1 / cont_plot2 / make_cont_plot(pitch_cont, "pitchers_per_volume")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top", axis.title.y = element_text(vjust = 0.3)) -> cont_plot

# Linear models ####
# Remove plants that did not survive
pitch_end <- pitch_end |> filter(rootl > 0)

# Run linear models and create plots
plant_volume_mod <- make_endpoint_plot(data = pitch_end,
                                       response = "plantvolfinal",
                                       initial_var = "plantvolinitial",
                                       y_label = expression("plant volume (" * cm^3 * ")"))

npitch_mod <- make_endpoint_plot(data = pitch_end,
                                 response = "numpitchfinal",
                                 initial_var = "plantminitial",
                                 y_label = expression("number of pitchers"))

pitchvol_mod <- make_endpoint_plot(data = pitch_end,
                                   response = "avgpitchvol",
                                   initial_var = "plantminitial",
                                   y_label =expression("avg. pitcher volume (" * cm^3 * ")"))

propvol_plot <- make_endpoint_plot(data = pitch_end,
                                   response = "propvol",
                                   initial_var = "plantminitial",
                                   y_label = "proportion carnivorous volume")

root_plot <- make_endpoint_plot(data = pitch_end,
                                   response = "rootl",
                                   initial_var = "plantminitial",
                                   y_label = "root length (cm)")

# Make column for number of pitchers per unit volume
pitch_end$npitch_vol <- pitch_end$numpitchfinal / pitch_end$plantvolfinal

npitch_vol_plot <- make_endpoint_plot(pitch_end,
                                      response = "npitch_vol",
                                      initial_var = "plantminitial", 
                                      y_label = expression(frac("number pitchers",
                                      "plant volume (" * cm^3 * ")")))

# Put plots together
npitch_vol_plot <- npitch_vol_plot$plot +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

(propvol_plot$plot | (npitch_vol_plot / pitchvol_mod$plot)) +
  plot_layout(widths = c(3, 2), guides = "collect") +
  plot_annotation(tag_levels = "a") & theme(legend.position = "bottom",
                                            axis.title.y = element_text(vjust = 0.3)) -> main_plot


# Ordinal model ####

# Make a new variable that is an ordered factor for the regression
pitch_end$emerged_ord <- ordered(pitch_end$emermonth)

# Fit the model and look at coefficients
ord_mod <- polr(emerged_ord ~ avgsoilmdiff + plantminitial, data = pitch_end, Hess = TRUE)
summary(ord_mod)

# Get the confidence intervals around the coefficients
confint(ord_mod)
# Significant effect of soil moisture, but not of initial mass

## Plotting predictions from the model for each soil moisture treatment ####

# First calculate the average soil moisture for each treatment
soil_vals <- pitch_end %>% 
  group_by(treatment) %>% 
  summarize(mean_soildiff = mean(avgsoilmdiff, na.rm = T)) %>% 
  pull(mean_soildiff)

# Make predictions at average initial plant size
plant_mean <- mean(pitch_end$plantminitial, na.rm = TRUE)

# Set up empty data frame with x-values for predictions
newdat_soil <- tibble(
  avgsoilmdiff = soil_vals,
  plantminitial = plant_mean
)

# Make predictions for each soil moisture treatment given average initial weight
pred_probs_ord <- predict(ord_mod, newdata = newdat_soil, type = "probs")

# Calculate cumulative probabilities
as.data.frame(pred_probs_ord) %>%
  mutate(water_interval = c("1-Day", "3-Day", "5-Day", "7-Day")) %>%
  gather(key = "month", value = "prob", `2`:`5`) %>%
  group_by(water_interval) %>%
  # This line of code adds up the probabilities as you move from month to month
  mutate(cum_prob = cumsum(prob)) -> pred_df_cumprob

# Change month labels
pred_df_cumprob |> 
  mutate(month = case_when(month == 2 ~ "mar",
                           month == 3 ~ "apr",
                           month == 4 ~ "may",
                           month == 5 ~ "jun")) |> 
  mutate(month = factor(month, levels = c("mar", "apr", "may", "jun"))) -> pred_df_cumprob

# plot cumulative probabilities
ggplot(pred_df_cumprob,
       aes(x = month,
           y = cum_prob,
           color = water_interval,
           group = water_interval)) +
  geom_line() +
  geom_point(size = 3.5) +
  labs(x = "month", 
       y = "cumulative probability\nmature pitchers developed",
       color = "watering interval") +
  theme_classic(base_size = 12) +
  theme(legend.position = "top",
        legend.key.spacing.y = unit(-2, "pt") ) +
  scale_color_manual(values = rev(water_colors)) +
  guides(color = guide_legend(nrow = 2))-> ordinal_plot

# Get predicted values for in-text 
pred_df_cumprob %>% 
  filter(month == "apr") 
# Root correlation ####
pitch_end |> 
  ggplot(aes(x = propvol, y = rootl, fill = avgsoilmdiff)) +
  geom_smooth(se = F, method = "lm", color = "black", linetype = "dashed") +
  geom_point(size = 3.5, shape = 21, stroke = 0.6) +
  scale_fill_continuous(palette = c("#08519c", "#6baed6", "#eff3ff")) +
  labs(y = "root length (cm)",
       x = "proportion carnivorous volume",
       fill = "soil moisture\ndifference (%)") +
  theme_classic(base_size = 14) -> rootcorr_plot

pitch_end |> filter(complete.cases(rootl) & complete.cases(propvol)) -> pitch_end_sub
cor.test(pitch_end_sub$rootl, pitch_end_sub$propvol, method = "pearson")

# Plots ####
treatment_plot
ggsave("figs/figs1.png", cont_plot, height = 5, width = 7)

cont_plot
ggsave("figs/fig2.svg", cont_plot, height = 8, width = 5)

main_plot
ggsave("figs/fig3.svg", main_plot, height = 5.3, width = 7.25)

ordinal_plot
ggsave("figs/fig4.svg", ordinal_plot, height = 3.5, width = 3.5)

rootcorr_plot
ggsave("figs/fig5.svg", root_plot, height = 3.5, width = 5)

