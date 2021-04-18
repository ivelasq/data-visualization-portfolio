# Best Selling Consoles Over Time

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggstream)
library(readxl)
library(showtext)

# Font --------------------------------------------------------------------

font_add_google("Economica", "economica")

# Data --------------------------------------------------------------------

consoles <-
  read_excel(here::here("data", "consoles data.xlsx")) %>% 
  slice(-31:-32) %>% 
  rename(console = `...1`) %>% 
  select(console, `1977`:`2018`)

consoles_tidy <- 
  consoles %>% 
  pivot_longer(cols = `1977`:`2018`,
               names_to = "year",
               values_to = "value") %>% 
  replace(is.na(.), 0) %>% 
  mutate(year = as.numeric(year))

colors <-
  read_csv(here::here("data", "colors.csv"))

consoles_join <-
  left_join(consoles_tidy, colors)

legend_comp <-
  data.frame(x1 = rep(1990, 6), 
             y1 = rep(0, 6), 
             Companies = c("Atari", "Nintendo", "PlayStation", "Sega", "Xbox", "Other"))

legend_comp$Companies <- factor(legend_comp$Companies,
                                levels = legend_comp$Companies)

# Plot --------------------------------------------------------------------

ggplot() +
  # fake dataset for custom legend
  geom_point(data = legend_comp, 
             aes(x1, y1, 
                 color = Companies),
             size = 4,
             shape = 15) +
  # create chart
  geom_stream(data = consoles_tidy,
              aes(year, value, 
                  fill = console)) +
  cowplot::theme_minimal_vgrid() +
  # custom colors
  scale_fill_manual(values = c("Atari 2600" = "#507320",
                               "Atari 5200" = "#82BF2C",
                               "NES" = "#8C2620",
                               "Game Boy / GB Color" = "#400B1D",
                               "Super NES" = "#402322",
                               "Nintendo 64" = "#BF332C",
                               "Game Boy Advance" = "#CA6184",
                               "Nintendo GameCube" = "#5F1A16",
                               "Wii" = "#CA706B",
                               "Nintendo DS" = "#BF2257",
                               "Nintendo 3DS" = "#401F2A",
                               "Wii U" = "#CC2D25",
                               "Nintendo Switch" = "#D62F27",
                               "Intellivision" = "#F7FFBC",
                               "ColecoVision" = "#EEFF70",
                               "TurboGrafx-16" = "#FFF2B0",
                               "PlayStation" = "#00E8C9",
                               "PlayStation 2" = "#34AC9C",
                               "PlayStation Portable" = "#009C87",
                               "PlayStation 3" = "#46E8D2",
                               "PlayStation Vita" = "#00695B",
                               "PlayStation 4" = "#36B5A4",
                               "Sega Master System" = "#1F5782",
                               "Sega Genesis / Mega Drive" = "#2B3F4F",
                               "Sega Game Gear" = "#0D2436",
                               "Sega Saturn" = "#466882",
                               "Sega Dreamcast" = "#328BCF",
                               "Xbox" = "#781F82",
                               "Xbox 360" = "#4B2B4F",
                               "Xbox One" = "#c8a2c8")) +
  # create legend
  guides(fill = FALSE,
         colour = guide_legend(nrow = 1)) +
  scale_color_manual(name = "Companies",
                    values = c("Atari" = "#507320", 
                               "Nintendo" = "#400B1D", 
                               "PlayStation" = "#00E8C9", 
                               "Sega" = "#1F5782", 
                               "Xbox" = "#781F82", 
                               "Other" = "#7CBB5D")) +
  # make pretty
  labs(title = "Best Selling Video Game Consoles by Year",
       x = "Year",
       y = "Units Sold") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank()) +
  annotate(geom = "text",
           x = 2010,
           y = -10,
           size = 4,
           label = "Test", 
           hjust = "center",
           lineheight = 0.9,
           color = "white")


ggplot() +
  # fake dataset for custom legend
  geom_point(data = legend_comp, 
             aes(x1, y1, 
                 color = Companies),
             size = 4,
             shape = 15) +
  # create chart
  geom_area(data = consoles_tidy,
              aes(year, value, 
                  fill = console)) +
  labs(title = "Best Selling Video Game Consoles by Year",
       x = "Year",
       y = "Units Sold") +
  theme(legend.position = "bottom") +
  guides(fill = FALSE,
         colour = guide_legend(nrow = 1))
