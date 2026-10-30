# Figure S1: Plot soil moisture difference by treatment

# Preliminaries #### 
library(tidyverse); library(janitor)

pitch_cont <- read_csv("data/GHContinuous.csv")  
names(pitch_cont) <- tolower(names(pitch_cont))

colors <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0")

# Put months in the correct order
pitch_cont %>% 
  mutate(monthn = tolower(monthn)) |> 
  mutate(month_ord = factor(monthn, levels = c("feb", "mar", "apr", "may", "jun",
                                               "jul", "aug", "sep", "oct", "nov"))) -> pitch_cont

# Plot soil moisture difference by pot, colored by treatment
pitch_cont |> 
  ggplot(aes(x = month_ord, y = soilmdiff,
             group = plantid, color = wateringinterval)) +
  geom_point() +
  geom_line() +
  theme_classic(base_size = 14) +
  scale_color_manual(values = rev(colors)) +
  labs(x = "month",
       y = "soil moisture difference (%)",
       color = "watering interval")

# Save plot
ggsave("figs/fig_s1.png")
