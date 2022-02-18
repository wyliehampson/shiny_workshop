# practice code
library(palmerpenguins)
library(tidyverse)

# Filter for body masses
body_mass_df <- penguins %>% 
  filter(body_mass_g %in% 3000:4000)

# Scatter Plot
ggplot(na.omit(body_mass_df), 
       aes(x = flipper_length_mm,
           y = bill_length_mm,
           color = species,
           shape = species)) +
  geom_point() +
  scale_color_manual(values = c("#FEA346", "#B251F1", "#4BA4A4")) +
  labs( x = "Flipper Length (mm)",
        y = "Bill Length (mm)",
        color = "Penguin Species",
        shape = "Penguin Species")

